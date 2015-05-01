{-# LANGUAGE QuasiQuotes #-}

module Conjure.Rules.Vertical.Graph.GraphAsSets where

import Conjure.Rules.Import

rule_Comprehension_Verts :: Rule
rule_Comprehension_Verts = "graph-comprehension-verts{GraphAsSets}" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gocBefore, (pat, func), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
            Generator (GenInExpr pat@Single{} expr) -> return (pat, matchDefs [opVerts] expr)
            _ -> na "rule_Comprehension"
        "GraphAsSets" <- representationOf func
        [verts,edges]         <- downX1 func
        return
            ( "Mapping over a graph, GraphAsSets representation"
            , return $
                Comprehension body
                    $  gocBefore
                    ++ [ Generator (GenInExpr pat verts) ]
                    ++ gocAfter
            )
    theRule _ = na "rule_Comprehension_Verts"

rule_Comprehension_Edges :: Rule
rule_Comprehension_Edges = "graph-comprehension-edges{GraphAsSets}" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gocBefore, (pat, func), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
            Generator (GenInExpr pat@Single{} expr) -> return (pat, matchDefs [opEdges] expr)
            _ -> na "rule_Comprehension"
        "GraphAsSets" <- representationOf func
        [_,edges]         <- downX1 func
        return
            ( "Mapping over a graph, GraphAsSets representation"
            , return $
                Comprehension body
                    $  gocBefore
                    ++ [ Generator (GenInExpr pat edges) ]
                    ++ gocAfter
            )
    theRule _ = na "rule_Comprehension_Edges"
