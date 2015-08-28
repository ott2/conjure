module Conjure.Process.Enumerate
    ( EnumerateDomain
    , enumerateDomain
    , enumerateInConstant
    , runEnumerateDomain
    , EnumerateDomainNoIO(..)
    ) where

import Conjure.Prelude
import Conjure.Language.AdHoc
import Conjure.Language
import Conjure.UI.IO

-- shelly
import Shelly ( run, lastStderr )

-- text
import qualified Data.Text as T ( null )

-- pipes
import qualified Pipes

-- temporary
import System.IO.Temp ( withSystemTempDirectory )


-- | This class is only to track where `enumerateDomain` might get called.
--   It is essentially MonadIO, but doesn't allow arbitrary IO.
class (Functor m, Applicative m, Monad m) => EnumerateDomain m where liftIO' :: IO a -> m a
instance EnumerateDomain IO where liftIO' = id
instance EnumerateDomain m => EnumerateDomain (IdentityT m) where liftIO' = lift . liftIO'
instance EnumerateDomain m => EnumerateDomain (MaybeT m) where liftIO' = lift . liftIO'
instance EnumerateDomain m => EnumerateDomain (ExceptT m) where liftIO' = lift . liftIO'
instance EnumerateDomain m => EnumerateDomain (ReaderT r m) where liftIO' = lift . liftIO'
instance EnumerateDomain m => EnumerateDomain (StateT st m) where liftIO' = lift . liftIO'
instance EnumerateDomain m => EnumerateDomain (Pipes.Proxy a b c d m) where liftIO' = lift . liftIO'
instance EnumerateDomain m => EnumerateDomain (NameGenM m) where liftIO' = lift . liftIO'

-- | Use this if you don't want to allow a (EnumerateDomain m => m a) computation actually do IO.
data EnumerateDomainNoIO a = Done a | TriedIO
    deriving (Eq, Ord, Show)

instance Functor EnumerateDomainNoIO where
    fmap _ TriedIO  = TriedIO
    fmap f (Done x) = Done (f x)

instance Applicative EnumerateDomainNoIO where
    pure = return
    (<*>) = ap

instance Monad EnumerateDomainNoIO where
    return = Done
    TriedIO >>= _ = TriedIO
    Done x  >>= f = f x

instance EnumerateDomain EnumerateDomainNoIO where liftIO' _ = TriedIO

runEnumerateDomain :: IO a -> IO a
runEnumerateDomain = id

enumerateDomainMax :: Int
enumerateDomainMax = 10000

enumerateDomain :: (MonadFail m, EnumerateDomain m) => Domain () Constant -> m [Constant]
enumerateDomain DomainBool = return [ConstantBool False, ConstantBool True]
enumerateDomain (DomainInt rs) = concatMapM enumerateRange rs
enumerateDomain (DomainEnum _dName (Just rs) _mp) = concatMapM enumerateRange rs
enumerateDomain (DomainTuple ds) = do
    inners <- mapM enumerateDomain ds
    return $ map (ConstantAbstract . AbsLitTuple) (sequence inners)
enumerateDomain (DomainMatrix (DomainInt indexDom) innerDom) = do
    inners <- enumerateDomain innerDom
    indexInts <- rangesInts indexDom
    return
        [ ConstantAbstract (AbsLitMatrix (DomainInt indexDom) vals)
        | vals <- replicateM (length indexInts) inners
        ]
enumerateDomain (DomainSet _ (SetAttr sizeAttr) innerDom) = do
    inners <- enumerateDomain innerDom
    sizes  <- case sizeAttr of
        SizeAttr_None -> return [0 .. genericLength inners]
        SizeAttr_Size (ConstantInt a) -> return [a]
        SizeAttr_MinSize (ConstantInt a) -> return [a .. genericLength inners]
        SizeAttr_MaxSize (ConstantInt a) -> return [0 .. a]
        SizeAttr_MinMaxSize (ConstantInt a) (ConstantInt b) -> return [a .. b]
        _ -> fail $ "sizeAttr, not an int:" <+> pretty sizeAttr
    return
        [ ConstantAbstract (AbsLitSet vals)
        | s <- sizes
        , vals <- replicateM (fromInteger s) inners
        , sorted vals
        ]
enumerateDomain (DomainFunction _ attr DomainBool innerTo) | attr == def = do
    inners <- enumerateDomain innerTo
    let outEmpty     = [ ConstantAbstract (AbsLitFunction []) ]
    let outOnlyFalse = [ ConstantAbstract (AbsLitFunction [ (ConstantBool False, x) ])
                       | x <- inners ]
    let outOnlyTrue  = [ ConstantAbstract (AbsLitFunction [ (ConstantBool True , x) ])
                       | x <- inners ]
    inners2 <- enumerateDomain innerTo
    let outBoth      = [ ConstantAbstract (AbsLitFunction [ (ConstantBool False, x)
                                                          , (ConstantBool True , y) ])
                       | x <- inners
                       , y <- inners2
                       ]
    return $ outEmpty
          ++ outOnlyFalse
          ++ outOnlyTrue
          ++ outBoth

-- the sledgehammer approach
enumerateDomain d = liftIO' $ withSystemTempDirectory ("conjure-enumerateDomain-" ++ show (hash d)) $ \ tmpDir -> do
    let model = Model { mLanguage = LanguageVersion "Essence" [1,0]
                      , mStatements = [Declaration (FindOrGiven Find "x" (fmap Constant d))]
                      , mInfo = def
                      }
    let essenceFile = tmpDir </> "out.essence"
    let outDir = tmpDir </> "outDir"
    writeModel PlainEssence (Just essenceFile) model
    (_, stderrSR) <- sh $ do
        stdoutSR <- run "conjure"
            [ "solve"
            , stringToText essenceFile
            , "-o", stringToText outDir
            , "--savilerow-options"
            , stringToText $ "-O0 -preprocess None -timelimit 60000 -num-solutions " ++ show enumerateDomainMax
            , "--minion-options"   , "-cpulimit 60"
            ]
        stderrSR <- lastStderr
        return (stdoutSR, stderrSR)
    unless (T.null stderrSR) $ fail (pretty stderrSR)
    solutions   <- filter (".solution" `isSuffixOf`) <$> getDirectoryContents outDir
    when (length solutions >= enumerateDomainMax) $ fail "Enumerate domain: too many."
    enumeration <- fmap concat $ forM solutions $ \ solutionFile -> do
        Model _ decls _ <- readModelFromFile (outDir </> solutionFile)
        return [ c | Declaration (Letting "x" (Constant c)) <- decls ]
    removeDirectoryRecursive outDir
    removeDirectoryRecursive tmpDir
    return enumeration


sorted :: Ord a => [a] -> Bool
sorted xs = and $ zipWith (<) xs (tail xs)

enumerateRange :: MonadFail m => Range Constant -> m [Constant]
enumerateRange (RangeSingle x) = return [x]
enumerateRange (RangeBounded (ConstantInt x) (ConstantInt y)) = return $ map ConstantInt [x..y]
enumerateRange RangeBounded{} = fail "enumerateRange RangeBounded"
enumerateRange RangeOpen{} = fail "enumerateRange RangeOpen"
enumerateRange RangeLowerBounded{} = fail "enumerateRange RangeLowerBounded"
enumerateRange RangeUpperBounded{} = fail "enumerateRange RangeUpperBounded"

enumerateInConstant :: MonadFail m => Constant -> m [Constant]
enumerateInConstant constant = case constant of
    ConstantAbstract (AbsLitMatrix _  xs) -> return xs
    ConstantAbstract (AbsLitSet       xs) -> return xs
    ConstantAbstract (AbsLitMSet      xs) -> return xs
    ConstantAbstract (AbsLitFunction  xs) -> return [ ConstantAbstract (AbsLitTuple [i,j]) | (i,j) <- xs ]
    ConstantAbstract (AbsLitSequence  xs) -> return [ ConstantAbstract (AbsLitTuple [i,j])
                                                    | (i',j) <- zip allNats xs
                                                    , let i = fromInt i'
                                                    ]
    ConstantAbstract (AbsLitRelation  xs) -> return $ map (ConstantAbstract . AbsLitTuple) xs
    ConstantAbstract (AbsLitPartition xs) -> return $ map (ConstantAbstract . AbsLitSet) xs
    TypedConstant c _                     -> enumerateInConstant c
    _ -> fail $ vcat [ "enumerateInConstant"
                     , "constant:" <+> pretty constant
                     ]
