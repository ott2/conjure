{-# LANGUAGE QuasiQuotes #-}

module Conjure.Process.Sanity ( sanityChecks ) where

import Conjure.Prelude
import Conjure.UserError
import Conjure.Language
import Conjure.Language.CategoryOf
import Conjure.Language.NameResolution ( resolveNamesX )


sanityChecks :: (MonadFail m, MonadUserError m, NameGen m) => Model -> m Model
sanityChecks model = do
    let
        recordErr :: MonadWriter [Doc] m => [Doc] -> m ()
        recordErr = tell . return . vcat

        check :: (MonadFail m, MonadUserError m, MonadWriter [Doc] m, NameGen m) => Model -> m Model
        check m = do
            forM_ (mStatements m) $ \ st -> case st of
                Declaration (FindOrGiven Given _ _) -> return () -- skip
                Declaration FindOrGiven{}           -> mapM_ (checkDomain (Just st)) (universeBi (forgetRefs st))
                _                                   -> mapM_ (checkDomain Nothing  ) (universeBi (forgetRefs st))
            mapM_ checkFactorial (universeBi $ mStatements m)
            statements2 <- mapM checkDomainIntE (mStatements m)
            statements3 <- transformBiM checkLit (concat statements2)
            return m { mStatements = statements3 }

        -- check for mset attributes
        -- check for binary relation attrobutes
        checkDomain :: MonadWriter [Doc] m => Maybe Statement -> Domain () Expression -> m ()
        checkDomain mstmt domain = case domain of
            DomainInt rs | inFind mstmt && isInfinite rs -> recordErr
                        [ "Infinite integer domain."
                        , "Context:" <++> maybe (pretty domain) pretty mstmt
                        ]
            DomainMSet _ (MSetAttr size occur) _ ->
                case (size, occur) of
                    (SizeAttr_Size{}, _) -> return ()
                    (SizeAttr_MaxSize{}, _) -> return ()
                    (SizeAttr_MinMaxSize{}, _) -> return ()
                    (_, OccurAttr_MaxOccur{}) -> return ()
                    (_, OccurAttr_MinMaxOccur{}) -> return ()
                    _ -> recordErr
                        [ "mset requires (at least) one of the following attributes: size, maxSize, maxOccur"
                        , "Context:" <++> maybe (pretty domain) pretty mstmt
                        ]
            DomainRelation _ (RelationAttr _ binRelAttr) [a,b]
                | binRelAttr /= def && a /= b
                -> recordErr
                        [ "Binary relation attributes can only be used for binary relations between identical domains."
                        , "Either remove these attributes:" <+> pretty binRelAttr
                        , "Or make sure that the relation is between identical domains."
                        , "Context:" <++> maybe (pretty domain) pretty mstmt
                        ]
            DomainRelation _ (RelationAttr _ binRelAttr) innerDoms
                | binRelAttr /= def && length innerDoms /= 2
                -> recordErr
                        [ "Binary relation attributes can only be used on binary relations."
                        , "Either remove these attributes:" <+> pretty binRelAttr
                        , "Or make sure that the relation is binary."
                        , "Context:" <++> maybe (pretty domain) pretty mstmt
                        ]
            _ -> return ()

        checkDomainIntE
            :: (MonadFail m, MonadUserError m, NameGen m)
            => Statement
            -> m [Statement]
        checkDomainIntE st0 = do
            let

                -- avoid going inside references
                goX :: (MonadFail m, MonadUserError m, NameGen m, MonadWriter [Statement] m)
                    => Expression
                    -> m Expression
                -- goX x | trace (show $ "goX" <+> pretty (show x)) False = bug""
                goX x@Reference{} = return x
                goX (Domain d) = Domain <$> goD d
                goX x = descendM goX x

                goD :: (MonadFail m, MonadUserError m, NameGen m, MonadWriter [Statement] m)
                    => Domain () Expression
                    -> m (Domain () Expression)
                -- goD x | trace (show $ "goD" <+> pretty x) False = bug""
                goD (DomainIntE v) = do
                    (iPat, i) <- quantifiedVar
                    v' <- resolveNamesX [essence| [ &i | &iPat <- &v ] |]
                    newLetting <- nextName "let"
                    tell [Declaration (Letting newLetting v')]                              -- new declaration
                    return (DomainIntE (Reference newLetting (Just (Alias v'))))            -- the replacement
                goD d = descendM goD d

            (st, new) <- runWriterT (case st0 of
                                        Declaration{} -> descendBiM goD st0
                                        _             -> descendBiM goX st0)
            return (new++[st])


        -- check for function literals
        --     they cannot contain anything > CatParameter
        --     they cannot map the same element to multiple range elemnets
        -- check for partition literals
        --     they cannot contain anything > CatParameter
        --     the parts have to be disjoint
        -- TODO: Generate where clauses for when they contain parameters.
        checkLit :: MonadFail m => Expression -> m Expression
        checkLit lit = case lit of
            AbstractLiteral (AbsLitFunction mappings) -> do
                let defineds = fromList $ map fst mappings
                return $ WithLocals lit (DefinednessConstraints [ [essence| allDiff(&defineds) |] ])
            AbstractLiteral (AbsLitPartition parts) -> do
                let disjoint = concat
                        [ checks
                        | (part1, after) <- withAfter parts
                        , part2 <- after
                        , let checks = [ [essence| &el1 != &el2 |]
                                       | el1 <- part1
                                       , el2 <- part2
                                       ]
                        ]
                return $
                    if null disjoint
                        then lit
                        else WithLocals lit (DefinednessConstraints disjoint)
            _ -> return lit

        checkFactorial :: MonadWriter [Doc] m => Expression -> m ()
        checkFactorial p@[essence| factorial(&x) |]
            | categoryOf x >= CatDecision
            = recordErr
                [ "The factorial function does not work on decision expressions."
                , "Context:" <++> pretty p
                ]
        checkFactorial _ = return ()

    (model', errs) <- runWriterT (check model)
    if null errs
        then return model'
        else userErr errs

-- | return True if a bunch of ranges represent an infinite domain.
--   return False if finite. also, return false if unsure.
isInfinite :: [Range a] -> Bool
isInfinite [] = True
isInfinite [RangeOpen{}] = True
isInfinite [RangeLowerBounded{}] = True
isInfinite [RangeUpperBounded{}] = True
isInfinite _ = False

inFind :: Maybe Statement -> Bool
inFind (Just (Declaration (FindOrGiven Find _ _))) = True
inFind _ = False

forgetRefs :: Statement -> Statement
forgetRefs = transformBi f
    where
        f (Reference nm _) = Reference nm Nothing
        f x = x
