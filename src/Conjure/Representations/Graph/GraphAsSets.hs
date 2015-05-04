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

        repr s = case s of
                   SizeAttr_Size{} -> "Explicit"
                   _               -> "ExplicitVarSizeWithMarker"

        getVerts (DomainGraph "GraphAsSets" (GraphAttr nVerts _ _) innerDomain) =
            return (DomainSet (repr nVerts) (SetAttr nVerts) innerDomain)

        getVerts domain = na $ vcat [ "{getVerts} GraphAsSets"
                                    , "domain:" <+> pretty domain
                                    ]

        getEdges (DomainGraph "GraphAsSets" (GraphAttr _ nEdges _) innerDomain) =
            return (DomainSet (repr nEdges) (SetAttr nEdges) $ DomainTuple [innerDomain,innerDomain])

        getEdges domain = na $ vcat [ "{getEdges} GraphAsSets"
                                    , "domain:" <+> pretty domain
                                    ]

        downD :: TypeOf_DownD m
        downD (name, inDom) = do
            vDom <- getVerts inDom
            eDom <- getEdges inDom
            return $ Just [ ( nameVerts name , vDom ) 
                          , ( nameEdges name , eDom )
                          ]

        structuralCons :: TypeOf_Structural m
        structuralCons f downX1 inDom = do

            let structuralConsVs vs = do
                  d <- getVerts inDom
                  innerStructuralConsGen <- f d
                  innerStructuralConsGen vs

            let structuralConsEs es = do
                  d <- getEdges inDom
                  innerStructuralConsGen <- f d
                  innerStructuralConsGen es

            let validEdgesCons vs es = do
                    (iPat, i) <- quantifiedVar
                    return $ return $
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

            let simpCons es = do
                    (iPat, i) <- quantifiedVar
                    return $ return $
                        [essence|
                            and([ &i[1] != &i[2]
                                | &iPat <- &es])
                        |]

            return $ \ graph -> do
                refs <- downX1 graph
                case refs of
                    [vs,es] -> do
                      comp <- case inDom of
                                DomainGraph "GraphAsSets" (GraphAttr _ _ True) _
                                  -> compCons vs es
                                _ -> return []
                      concat <$> sequence 
                             [ structuralConsVs vs
                             , structuralConsEs es
                             , validEdgesCons vs es
                             , simpCons es
                             , return comp
                             ]
                            -- , compCons vs es
                            -- , 
                            -- ]
                    _ -> na "{structuralCons} GraphAsSets"

        downC :: TypeOf_DownC m
        downC ( name
              , DomainGraph "GraphAsSets"
                    (GraphAttr vs es _)
                    inner
              , ConstantAbstract (AbsLitGraph vals)
              ) = do
            let vertsOut = map fst vals
                mkE (v,us) = map (\u -> ConstantAbstract $ AbsLitTuple [v,u]) us
                edgesOut = concat $ map mkE vals
            return $ Just
                [ ( nameVerts name
                  , DomainSet
                      (repr vs)
                      (SetAttr vs)
                      inner
                  , ConstantAbstract $ AbsLitSet
                      vertsOut
                  )
                , ( nameEdges name
                  , DomainSet
                      (repr es)
                      (SetAttr es)
                      $ DomainTuple [inner,inner]
                  , ConstantAbstract $ AbsLitSet
                      edgesOut
                  )
                ]
        downC _ = na "{downC} GraphAsSets"


        up :: TypeOf_Up m
        up ctxt (name, domain@(DomainGraph "GraphAsSets"
                                (GraphAttr _ _ _)
                                innerDomain)) = 
            case (lookup (nameVerts name) ctxt, lookup (nameEdges name) ctxt) of
                ( Just (ConstantAbstract (AbsLitSet vSet)) ,
                  Just (ConstantAbstract (AbsLitSet eSet)) ) -> do
                    let isAdjacent v (ConstantAbstract (AbsLitTuple [fr,to])) 
                          | v == fr   = Just to
                          | otherwise = Nothing
                        isAdjacent _ _ = Nothing
                        mkAdjList v = mapMaybe (isAdjacent v) eSet
                        xs = zip vSet $ map mkAdjList vSet
                    return ( name, ConstantAbstract $ AbsLitGraph xs )
                (Nothing, _) -> fail $ vcat $
                    [ "No value for:" <+> pretty (nameVerts name)
                    , "When working on:" <+> pretty name
                    , "With domain:" <+> pretty domain
                    ] ++
                    ("Bindings in context:" : prettyContext ctxt)
                (_, Nothing) -> fail $ vcat $
                    [ "No value for:" <+> pretty (nameEdges name)
                    , "When working on:" <+> pretty name
                    , "With domain:" <+> pretty domain
                    ] ++
                    ("Bindings in context:" : prettyContext ctxt)
                _ -> fail $ vcat $
                    [ "Expected set literals for:" <+> pretty (nameVerts name)
                                         <+> "and" <+> pretty (nameEdges name)
                    , "When working on:" <+> pretty name
                    , "With domain:" <+> pretty domain
                    ] ++
                    ("Bindings in context:" : prettyContext ctxt)
        up _ _ = na "{up} GraphAsSets"
