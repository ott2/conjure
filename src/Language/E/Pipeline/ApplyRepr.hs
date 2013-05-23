{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.E.Pipeline.ApplyRepr ( applyRepr ) where

import Language.E
import Language.E.Pipeline.RuleReprToFunction ( ruleReprToFunction )
import Language.E.BuiltIn ( builtInRepr, mergeReprFunc )

import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import qualified Data.Text as T
import qualified Text.PrettyPrint as Pr


applyRepr
    :: MonadConjureList m
    => [RuleRepr]
    -> Spec
    -> m Spec
applyRepr rules spec = do
    theMode <- getsGlobal conjureMode
    withBindingScope' $ let mfunc = ruleReprToFunction rules in case mfunc of
        Left es     -> err ErrFatal $ vcat $ map (prettyError "repr") es
        Right func' -> do

            let func = mergeReprFunc (func' : builtInRepr)

            let Spec _ statements = spec
            mapM_ introduceStuff (statementAsList statements)

            let topLevels'
                    =  [ (x,n,d) | x@[xMatch| [Prim (S n)] := topLevel.declaration.find .name.reference
                                            | [d]          := topLevel.declaration.find .domain
                                            |] <- statementAsList statements ]
                    ++ [ (x,n,d) | x@[xMatch| [Prim (S n)] := topLevel.declaration.given.name.reference
                                            | [d]          := topLevel.declaration.given.domain
                                            |] <- statementAsList statements ]

            let topLevels = [ (x,n,d) | (x,n,d) <- topLevels', domainNeedsRepresentation d ]

            candidates :: [(Text,[RuleReprResult])]
                <- forM topLevels $ \ (x,n,d) -> do
                ys0 <- func (n,d,x)
                ys1 <- selectByMode theMode ys0
                let ys = sortBy (\ (_, _, reprName1, _, _)
                                   (_, _, reprName2, _, _)
                                   -> compare reprName1 reprName2) ys1
                case ys of
                    [] -> err ErrFatal $ "No representation rule matches domain:" <+> pretty x
                    _  -> do
                        let ysNames = flip map ys $ \ (_origDecl, _ruleName, reprName, _newDom, _cons) -> reprName
                        mkLog "representation" $ sep [ pretty x
                                                     , "(#" <> pretty (length ys) <> ")"
                                                     , prettyList id "," ysNames
                                                     ] 
                        return (n,ys)

            let
                allRegioned :: [(Text,Text)]
                allRegioned =
                    let
                        findsandgivens = S.fromList $ map fst candidates
                    in
                        nub [ (base, reg)
                            | [xMatch| [Prim (S nm)] := reference |] <- universeSpecNoFindGiven spec
                            , (base, Just reg, _) <- [identifierSplit nm]
                            , base `S.member` findsandgivens
                            ]

                nbOccurrence :: (Text, Text) -> Int
                nbOccurrence (base,reg) = length
                    [ ()
                    | [xMatch| [Prim (S nm')] := reference |] <- universeSpecNoFindGiven spec
                    , (base', Just reg', _) <- [identifierSplit nm']
                    , base == base'
                    , reg  == reg'
                    ]

                lookupTables :: [ M.HashMap (Text,Text) RuleReprResult ]
                lookupTables = map M.fromList $ allCombinations
                    [ ((nm,region), results)
                    | (nm, region) <- allRegioned
                    , Just results <- [lookup nm candidates]
                    , let cnt = nbOccurrence (nm, region)
                    , cnt > 0
                    ]

            when (null lookupTables) $ err ErrGeneratesNone "repr0"
            table <- returns lookupTables
            if M.null table
                then err ErrGeneratesNone "repr1"
                else do
                    let configStr = hsep [ pretty (identifierConstruct nm (Just region) (Just rName))
                                         | ((nm, region), (_,_,rName,_,_)) <- M.toList table
                                         ]
                    mkLog "configuration" configStr
                    catchError
                        (applyCongfigToSpec spec table)
                        (\ (eu, em, _) ->
                            throwError ( eu
                                       , vcat [ "Error in applyConfigToSpec", pretty em ]
                                       , Just spec
                                       )
                        )


isGiven :: E -> Bool
isGiven [xMatch| _ := topLevel.declaration.given  |] = True
isGiven _ = False

isFind :: E -> Bool
isFind [xMatch| _  := topLevel.declaration.find   |] = True
isFind  _ = False

isFindOrGiven :: E -> Bool
isFindOrGiven x = isFind x || isGiven x

applyCongfigToSpec
    :: MonadConjure m
    => Spec
    -> M.HashMap (Text,Text) RuleReprResult
    -> m Spec
applyCongfigToSpec spec config = withBindingScope' $ do
    reprLogPre <- gets representationLog
    unless (null reprLogPre) $
        bug "representationLog needs to be null at this point"
    void $ recordSpec "before applyCongfigToSpec" spec
    initialiseSpecState spec
    let
        f p@[xMatch| [Prim (S nm)] := reference |] =
            case identifierSplit nm of
                (base, Just region, Nothing) ->
                    case M.lookup (base,region) config of
                        Nothing -> return p
                        Just (origDecl, _ruleName, reprName, newDom, cons) -> do
                            let
                                reregion e@[xMatch| [Prim (S i)] := reference |] = case identifierSplit i of
                                    (iBase, Just "regionS", mrepr)
                                        | (base `mappend` "_") `T.isPrefixOf` iBase ->
                                            let i' = identifierConstruct iBase (Just region) mrepr
                                            in  [xMake| reference := [Prim (S i')] |]
                                    _ -> e
                                reregion e = e
                            let
                                cons' = map (transform reregion) cons
                            modify $ \ st -> st { representationLog = (base, reprName, origDecl, newDom)
                                                                    :  representationLog st
                                                , structuralConsLog =
                                                    if isGiven origDecl
                                                        then structuralConsLog st
                                                        else nub $ (reprName, cons') : structuralConsLog st
                                                }
                            let nm' = identifierConstruct base (Just region) (Just reprName)
                            return [xMake| reference := [Prim (S nm')] |]
                _ -> return p
        f p = return p
    spec' <- bottomUpSpecExcept' isFindOrGiven f spec
    let pipeline = addChannellingFromLog >=>
                   addStructuralFromLog
    pipeline spec'


addStructuralFromLog :: MonadConjure m => Spec -> m Spec
addStructuralFromLog (Spec v xs) = do
    let mk i = [xMake| topLevel.suchThat := [i] |]
    structuralConsLog' <- gets structuralConsLog
    modify $ \ st -> st { structuralConsLog = [] }
    allStructural <-
        forM structuralConsLog' $ \ (reprName, cs') -> do
            cs <- liftM nub $ mapM (liftM fst . runWriterT . simplify) cs'
            if null cs
                then return []
                else do
                    mkLog "addedStructuralCons" $ vcat $ Pr.braces (pretty reprName) : map pretty cs
                    return cs
    return $ Spec v $ listAsStatement $ statementAsList xs ++ map mk (concat allStructural)


addChannellingFromLog :: MonadConjure m => Spec -> m Spec
addChannellingFromLog (Spec v xs) = do
    mapM_ introduceStuff (statementAsList xs)
    rlogs <- gets representationLog

    let grouped = filter ( (>1) . length )
                $ groupBy ((==) `on` fst)
                $ sortBy (comparing fst)
                $ nub
                  [ (nm, reprName) | (nm, reprName, origDecl, _) <- rlogs
                                   , isFind origDecl
                                   ]

    let newCons = sortNub $ concat
            [ [ [xMake| topLevel.suchThat := [theCons] |]
              | ((nm1,r1),(nm2,r2)) <- allPairs one
              , let x1 = [xMake| reference := [ Prim $ S $ mconcat [nm1, "#", r1] ] |]
              , let x2 = [xMake| reference := [ Prim $ S $ mconcat [nm2, "#", r2] ] |]
              , let theCons = if x1 <= x2
                                then [eMake| &x1 = &x2 |]
                                else [eMake| &x2 = &x1 |]
              ]
            | one <- grouped
            ]

    mapM_ (mkLog "addedChannel" . pretty) newCons

    let
        insertAfter _         [] = []
        insertAfter (old,new) (s:ss) | old == s  = old : new : ss
                                     | otherwise = s : insertAfter (old,new) ss

        insertNewDecls = foldr insertAfter

    let
        mkWithNewDom :: (Text, Text, E, E) -> (E, E)
        mkWithNewDom (origName, reprName, oldDecl@[xMatch| _ := topLevel.declaration.find |], newDom ) =
            ( oldDecl
            , [xMake| topLevel.declaration.find.name.reference := [Prim (S $ mconcat [origName, "_", reprName])]
                    | topLevel.declaration.find.domain         := [newDom]
                    |]
            )
        mkWithNewDom (origName, reprName, oldDecl@[xMatch| _ := topLevel.declaration.given |], newDom ) =
            ( oldDecl
            , [xMake| topLevel.declaration.given.name.reference := [Prim (S $ mconcat [origName, "_", reprName])]
                    | topLevel.declaration.given.domain         := [newDom]
                    |]
            )
        mkWithNewDom _ = bug "Impossible: addChannellingFromLog.mkWithNewDom"

    let newDecls = nub $ map mkWithNewDom rlogs
    mapM_ (introduceStuff . snd) newDecls

    newDecls' <- mapM (\ (i,j) -> do j' <- liftM fst $ runWriterT $ simplify j ; return (i,j')) newDecls
    mapM_ (introduceStuff . snd) newDecls'

    newCons'  <- mapM (liftM fst . runWriterT . simplify) newCons

    mapM_ (mkLog "addedDecl" . pretty . snd) newDecls'

    return $ Spec v $ listAsStatement $ insertNewDecls (statementAsList xs) newDecls' ++ newCons'


allPairs :: [a] -> [(a,a)]
allPairs [ ] = []
allPairs [_] = []
allPairs (x:xs) = map (x,) xs ++ allPairs xs

