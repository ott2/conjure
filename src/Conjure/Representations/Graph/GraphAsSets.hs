{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE Rank2Types #-}

module Conjure.Representations.Graph.GraphAsSets ( graphAsSets ) where

-- conjure
import Conjure.Prelude
import Conjure.Language.Definition
import Conjure.Language.Domain
import Conjure.Language.TH
import Conjure.Language.Pretty
import Conjure.Representations.Internal


graphAsSets
    :: forall m . (MonadFail m, NameGen m)
    => (forall x . Pretty x => Domain HasRepresentation x -> Representation m)
    -> Representation m
graphAsSets _ = Representation chck downD structuralCons downC up

    where

        chck :: TypeOf_ReprCheck
        chck f (DomainGraph _ attrs innerDomain) =
            DomainGraph "GraphAsSets" attrs <$> f innerDomain
        chck _ _ = []

        nameVerts name = mconcat [name, "_", "GraphAsSets_Verts"]
        nameEdges name = mconcat [name, "_", "GraphAsSets_Edges"]

        downD :: TypeOf_DownD m
        downD (name, DomainGraph "GraphAsSets"
                    (GraphAttr nVerts nEdges _)
                    innerDomain) = 
            let repr s = case s of
                          SizeAttr_Size{} -> "Explicit"
                          _               -> "ExplicitVarSizeWithMarker"
            in  return . Just $
                  [ ( nameVerts name
                    , DomainSet (repr nVerts)
                                (SetAttr nVerts)
                                innerDomain
                    )
                  , ( nameEdges name
                    , DomainSet (repr nEdges)
                                (SetAttr nEdges) 
                                $ DomainTuple [innerDomain,innerDomain]
                    )
                  ]
        downD _ = na "{downD} GraphAsSets"

        structuralCons :: TypeOf_Structural m
        structuralCons f downX1 inDom = do

--            let innerStructuralCons vs es = do
--                  [od1,od2] <- downD inDom
--                  isg1 <- f od1
--                  isg2 <- f od2
--                  r1   <- isg1 graph
--                  r2   <- isg2 graph
--                  return $ r1 ++ r2

            let validEdgesCons vs es = do
                    (iPat, i) <- quantifiedVar
                    return $ return $ -- list
                        [essence|
                            and([ &i[1] in &vs /\ &i[2] in &vs
                                | &iPat <- &es ])
                        |]

            let compCons vs es = do
                    (iPat, i) <- quantifiedVar
                    (jPat, j) <- quantifiedVar
                    return $ return $
                        [essence|
                            and([ &i != &j -> ((&i,&j) in &es)
                                | &iPat <- &vs
                                , &jPat <- &vs])
                        |]

            return $ \ graph -> do
                refs <- downX1 graph
                case refs of
                    [vs,es] ->
                        concat <$> sequence
                            [ --innerStructuralCons vs es
                              validEdgesCons vs es
                            ]
                            -- , compCons vs es
                            -- , 
                            -- ]
                    _ -> na "{structuralCons} GraphAsSets"

        structuralCons _ _ _ = na "{structuralCons} GraphAsSets"

        downC :: TypeOf_DownC m
        downC _ = na "{downC} Function1DPartial"

        up :: TypeOf_Up m
        --up ctxt (name, domain@(DomainGraph "Function1DPartial"
        --                        (FunctionAttr _ PartialityAttr_Partial _)
        --                        innerDomainFr _)) =
        --    case (lookup (nameFlags name) ctxt, lookup (nameValues name) ctxt) of
        --        ( Just (ConstantAbstract (AbsLitMatrix _ flagMatrix)) ,
        --          Just (ConstantAbstract (AbsLitMatrix _ valuesMatrix)) ) -> do
        --            froms          <- domainValues innerDomainFr
        --            functionValues <- forM (zip3 flagMatrix froms valuesMatrix) $ \ (flag, from, to) ->
        --                case flag of
        --                    ConstantBool b -> return $ if b then Just (from,to) else Nothing
        --                    _ -> fail $ vcat [ "Expected a boolean, but got:" <+> pretty flag
        --                                     , "When working on:" <+> pretty name
        --                                     , "With domain:" <+> pretty domain
        --                                     ]
        --            return ( name, ConstantAbstract $ AbsLitFunction $ catMaybes functionValues )
        --        (Nothing, _) -> fail $ vcat $
        --            [ "No value for:" <+> pretty (nameFlags name)
        --            , "When working on:" <+> pretty name
        --            , "With domain:" <+> pretty domain
        --            ] ++
        --            ("Bindings in context:" : prettyContext ctxt)
        --        (_, Nothing) -> fail $ vcat $
        --            [ "No value for:" <+> pretty (nameValues name)
        --            , "When working on:" <+> pretty name
        --            , "With domain:" <+> pretty domain
        --            ] ++
        --            ("Bindings in context:" : prettyContext ctxt)
        --        _ -> fail $ vcat $
        --            [ "Expected matrix literals for:" <+> pretty (nameFlags name)
        --                                    <+> "and" <+> pretty (nameValues name)
        --            , "When working on:" <+> pretty name
        --            , "With domain:" <+> pretty domain
        --            ] ++
        --            ("Bindings in context:" : prettyContext ctxt)
        up _ _ = na "{up} Function1DPartial"
