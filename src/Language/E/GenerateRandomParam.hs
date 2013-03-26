{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}

module Language.E.GenerateRandomParam ( generateRandomParam ) where

import Language.E
import Language.E.DomainOf(domainOf)
import Language.E.Up.Debug(upBug)
import Language.E.Up.IO(getSpec)
import Language.E.Up.ReduceSpec(reduceSpec,removeNegatives)

import Control.Arrow(arr)

import Data.List(genericTake)
import Data.Set (Set)
import qualified Data.Set as Set

type Essence      = Spec
type EssenceParam = Spec

-- Data type representing choices
data Choice =
     CBool
   | CInt    Integer [Range]
   | CTuple  [Choice]
   | CSet    Range Choice
   | CMatrix [Range] Choice
     deriving (Show,Eq)

--TODO MSet 
--TODO relation 

data FAttrs = FTotal   | FInjective        | Fsurjective 
data PAttrs = PRegular | PNumParts Integer | PPartSize Integer


data Range  =
    RSingle Integer
  | RRange  Integer Integer
    deriving (Show, Eq)

-- This assumes no overlapping ranges
instance Ord Range where
  (RSingle a ) <= (RSingle b ) = a <= b
  (RRange a _) <= (RRange c _) = a <= c
  (RRange _ b) <= (RSingle c ) = b <= c
  (RSingle a)  <= (RRange b _) = a <= b

instance Pretty Range  where pretty = pretty . show
instance Pretty Choice where
    pretty (CInt i rs )      = "CInt{" <> pretty i <> "}"    <+> sep (map pretty rs)
    pretty (CBool)           = "CBool"
    pretty (CTuple vs )      = "CTuple" <+> "(" <+>
                               sep (map (\a -> pretty a <+> " ") vs) <> ")"
    pretty (CSet rs dom)     = "CSet" <+> pretty rs <+> "OF" <+> pretty dom
    pretty (CMatrix rs cs)   = "CMatrix" <+> sep (map pretty rs) <+> "[" <+> pretty cs <+> "]"

_c :: Choice
_c = CInt 51  [RRange 0 49, RSingle 50 ]

-- Converts a choice into an action
evalChoice :: (MonadConjure m, RandomM m) => Choice -> m E
evalChoice (CBool) = do
    index <- rangeRandomM (0, 1)
    return $ Tagged "value" [Tagged "literal" [Prim (B (index == 1) )]]

evalChoice (CInt size ranges) = do
    index <- rangeRandomM (0, fromIntegral size-1)
    let n = pickIth (toInteger index) ranges
    mkLog "Data" $  sep ["Index:"  <+> pretty index
                        ,"Ranges:" <+> (pretty . show) ranges
                        ,"Picked"  <+> pretty n]
    return [xMake| value.literal := [Prim (I n )] |]

evalChoice (CTuple doms) = do
    vals <- mapM evalChoice doms
    return $ [xMake| value.tuple.values :=  vals |]

evalChoice (CSet sizeRange dom) = do
    size <- evalRange sizeRange
    findSet Set.empty size (repeat dom)

-- TODO add IndexRange 
evalChoice (CMatrix sizeRange dom) = do
    let size  = sum . map countRange $ sizeRange
    vals     <- mapM evalChoice (genericTake size . repeat $ dom)
    return $ [xMake| value.matrix.values := vals
                   | value.matrix.indexrange.domain.int.ranges := indexRanges |]

    where
    indexRanges = map rangeToIndexRange sizeRange 
    rangeToIndexRange :: Range -> E
    rangeToIndexRange (RSingle i)  = [xMake| range.single.value.literal := [Prim (I i) ] |]
    rangeToIndexRange (RRange a b) = 
        [xMake| range.fromTo := map wrap [a,b] |]
    wrap i = [xMake| value.literal := [Prim (I i)]  |]


countRanges :: [Range] -> Integer
countRanges = sum . map countRange

countRange :: Range -> Integer
countRange (RSingle _ ) = 1
countRange (RRange a b) =  b - a + 1

-- Takes a size and a inf list of choice (by repeat) and returns a set of that size
findSet :: (MonadConjure m, RandomM m) => Set E -> Integer -> [Choice] -> m E
findSet set 0 _ =
    let vs = Set.toAscList set
    in  return $ [xMake| value.set.values := vs |]

findSet set size (c:cs) = do
    ele <- evalChoice c
    -- CHECK I think the element will be in order
    -- if not use normaliseSolutionEs
    let (size',set') = if Set.notMember ele set
        then (size - 1, Set.insert ele set)
        else (size,set)
    findSet set' size' cs

findSet _ _ _ = _bugg "findSet: Can never happen"

evalRange :: (MonadConjure m, RandomM m) => Range -> m Integer
evalRange (RSingle i ) = return i
evalRange (RRange a b) = do
    let size  = b - a + 1
    index <- rangeRandomM (0, fromIntegral size-1)
    let picked = [a..b] `genericIndex` index
    mkLog "RangeData" $ sep  ["Index:"  <+> pretty index
                             ,"Range:"  <+> pretty (RRange a b)
                             ,"Picked:" <+> pretty picked]
    return picked


pickIth :: Integer -> [Range] -> Integer
pickIth _ [] = _bugg "pickIth no values"
pickIth 0 (RSingle i:_) = i
pickIth index (RRange a b:_ ) | index <= b - a =  [a..b] `genericIndex`  index

pickIth index (RSingle _:xs)    = pickIth (index - 1) xs
pickIth index (RRange a b:xs) = pickIth (index - (b - a) - 1 ) xs


generateRandomParam :: (MonadConjure m, RandomM m) => Essence -> m EssenceParam
generateRandomParam essence' = do
    essence <- removeNegatives essence'
    let stripped@(Spec _ f) = stripDecVars essence
    (Spec v e) <- reduceSpec stripped
    let es = statementAsList e

    --mkLog "Spec" (vcat $ map (\a -> prettyAsPaths a <+> "\n" ) (statementAsList f) )
    mkLog "GivensSpec"   (pretty f)
    mkLog "Reduced   " $ pretty es <+> "\n"

    doms <-  mapM domainOf es
    {-mkLog "D" (sep $ map (\a -> prettyAsPaths a <+> "\n" ) doms )-}

    choices <-  mapM handleDomain doms
    mkLog "Choices" (sep . map pretty $ choices )

    givens <- mapM evalChoice choices

    let lettings = zipWith makeLetting es givens
    mkLog "Lettings" (vcat $ map pretty lettings)
    --mkLog "Lettings" (vcat $ map (\a -> prettyAsBoth a <+> "\n" ) lettings )

    let essenceParam = Spec v (listAsStatement lettings )
    --mkLog "EssenceParam" (pretty essenceParam)

    return essenceParam
    --return essence


makeLetting :: E -> E -> E
makeLetting given val =
    [xMake| topLevel.letting.name := [getRef given]
          | topLevel.letting.expr := [val]|]

    where
    getRef :: E -> E
    getRef [xMatch|  _  := topLevel.declaration.given.name.reference
                  | [n] := topLevel.declaration.given.name |] = n
    getRef e = _bug "getRef: should not happen" [e]


handleDomain :: MonadConjure m => E -> m Choice
handleDomain [xMatch| _ := domain.bool |] =  return CBool

handleDomain [xMatch| ranges := domain.int.ranges |] = do
    cRanges <- mapM handleRange ranges
    let sortedRange =  sortOn snd cRanges
    return $ createChoice sortedRange

    where
    createChoice :: [(Integer,Range)] -> Choice
    createChoice = uncurry CInt . first (arr sum) . unzip

handleDomain [xMatch| [inner] := domain.set.inner
                    | attr    := domain.set.attributes.attrCollection|] = do
    dom <- handleDomain inner
    sizeRange <- handleSetAttributes dom attr
    return $ CSet sizeRange dom

handleDomain [xMatch| doms := domain.tuple.inners |]  =
   liftM CTuple (mapM handleDomain doms)

handleDomain [xMatch| [range] := domain.matrix.index
                    | [dom]   := domain.matrix.inner |]  = do
    (CInt _ ranges) <- handleDomain range
    dom'            <- handleDomain dom
    return $ CMatrix ranges dom'


handleDomain e = mkLog "U" (prettyAsPaths e <+> "\n"  ) >> return _c

handleSetAttributes :: MonadConjure m => Choice -> [E] -> m Range
handleSetAttributes dom es =
    handleSetAttributes' result
    where
    sorted = sort es
    -- To make sure size is at the front if present
    rev    = reverse sorted
    result = addSize rev sorted

    addSize ([xMatch| [Prim (S "size")] := attribute.nameValue.name.reference |] :_)
      _
      = es

    addSize _
      ([xMatch| [Prim (S "maxSize")] := attribute.nameValue.name.reference |] :_)
      = es

    addSize _ _   = rev ++ [ [xMake| attribute.nameValue.name.reference := [Prim (S "maxSize")]
                                   | attribute.nameValue.value.value.literal := [Prim (I n)] |] ]
        where n = findSize dom


findSize :: Choice -> Integer
findSize CBool = 2
findSize (CInt size _) = size
findSize (CTuple doms) = product . map findSize $ doms

findSize (CSet range dom) = result
    where
    dSize = findSize dom
    result  = sizeFromRange range
    sizeFromRange :: Range -> Integer
    sizeFromRange (RSingle k)  = dSize `choose` k
    sizeFromRange (RRange a b) = sum . map (dSize `choose`  ) $ [a..b]


findSize (CMatrix ranges dom ) =  dSize ^ matSize
   where
   matSize = countRanges ranges
   dSize = findSize dom 

handleSetAttributes' :: MonadConjure m => [E] -> m Range
handleSetAttributes' [] = _bugg "handleSetAttributes' no size"
handleSetAttributes' ([xMatch| [Prim (S "size")] := attribute.nameValue.name.reference
                             | [Prim (I n)]      := attribute.nameValue.value.value.literal|]
                      :_) =
    return $  RSingle n

handleSetAttributes' [[xMatch| [Prim (S "minSize")] := attribute.nameValue.name.reference
                             | [Prim (I n)]         := attribute.nameValue.value.value.literal|]
                     ] =
    return $  RRange n 10

handleSetAttributes' [[xMatch| [Prim (S "maxSize")] := attribute.nameValue.name.reference
                             | [Prim (I n)]         := attribute.nameValue.value.value.literal|]
                     ] =
    return $  RRange 0 n

handleSetAttributes' [[xMatch| [Prim (S "minSize")] := attribute.nameValue.name.reference
                             | [Prim (I a)]         := attribute.nameValue.value.value.literal|]
                     ,[xMatch| [Prim (S "maxSize")] := attribute.nameValue.name.reference
                             | [Prim (I b)]         := attribute.nameValue.value.value.literal|]
                     ] =
    return $  RRange a b

handleSetAttributes' _ = _bugg "handleSetAttributes': Can never happen"


handleRange :: MonadConjure m => E -> m (Integer,Range)
handleRange [xMatch| [Prim (I a),Prim (I b)] := range.fromTo.value.literal |] =
    return  (abs (b - a) +1, RRange a b )

handleRange [xMatch| [Prim (I n) ]  := range.single.value.literal |] =
    return (1, RSingle n)

handleRange e = do
    mkLog "unhandled" (prettyAsPaths e)
    return (1,RSingle (-99))


stripDecVars :: Essence -> Essence
stripDecVars (Spec v x) = Spec v y
    where
        xs = statementAsList x
        ys = filter stays xs
        y  = listAsStatement ys

        stays [xMatch| _ := topLevel.declaration.given |] = True
        stays [xMatch| _ := topLevel.letting           |] = True
        stays [xMatch| _ := topLevel.where             |] = True
        stays _ = False


choose :: Integral a => a -> a -> a
_ `choose` 0 = 1
0 `choose` _ = 0
n `choose` r = (n-1) `choose` (r-1) * n `div` r



_r :: IO Essence -> IO [(Either Doc EssenceParam, LogTree)]
_r sp = do
    seed <- getStdGen
    spec <- sp
    return $ runCompE "gen" (set_stdgen seed >> generateRandomParam spec)

_d :: Choice -> IO [(Either Doc E, LogTree)]
_d c = do
    seed <- getStdGen
    return $ runCompE "gen" (set_stdgen seed >> evalChoice c)

-- _x  =<<  _r _i
_x :: [(Either Doc a, LogTree)] -> IO ()
_x ((_, lg):_) =   print (pretty lg)
_x _ = return ()

_getTest :: FilePath -> IO Spec
_getTest f = getSpec $ "/Users/bilalh/CS/conjure/test/generateParams/" ++ f  ++ ".essence"

_b :: IO Spec
_b = _getTest "bool"

_e :: IO Spec
_e = _getTest "enum-1"

_f :: IO Spec
_f = _getTest "func-1"

_i :: IO Spec
_i = _getTest "int-1"
_i2 :: IO Spec
_i2 = _getTest "int-2"

_l :: IO Spec
_l = _getTest "letting-1"

_p :: IO Spec
_p = _getTest "partition-1"

_m :: IO Spec
_m = _getTest "matrixes-0"
_m2 :: IO Spec
_m2 = _getTest "matrixes"
_ms :: IO Spec
_ms = _getTest "matrixes-set"

_t :: IO Spec
_t = _getTest "tuples-0"
_t2 :: IO Spec
_t2 = _getTest "tuples"
_t3 :: IO Spec
_t3 = _getTest "tuples-set"
_t4 :: IO Spec
_t4 = _getTest "tuples-set-2"

_s :: IO Spec
_s = _getTest "set-size"
_s2 :: IO Spec
_s2 = _getTest "set-all"
_s3 :: IO Spec
_s3 = _getTest "set-max"
_s4 :: IO Spec
_s4 = _getTest "set-min"
_s5 :: IO Spec
_s5 = _getTest "set-minMax"
_sn :: IO Spec
_sn = _getTest "set-nested-1"
_sb :: IO Spec
_sb = _getTest "set-nobounds"
_sb2 :: IO Spec
_sb2 = _getTest "set-nobounds-2"
_sn2 :: IO Spec
_sn2 = _getTest "set-nested-2"
_lots :: IO Spec
_lots = _getTest "lots"

_bug :: String -> [E] -> t
_bug  s = upBug  ("GenerateRandomParam: " ++ s)
_bugg :: String -> t
_bugg s = _bug s []

