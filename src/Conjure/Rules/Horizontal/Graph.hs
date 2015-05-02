{-# LANGUAGE QuasiQuotes #-}

module Conjure.Rules.Horizontal.Graph where

import Conjure.Rules.Import

--rule_Eq :: Rule
--rule_Eq = "graph-eq" `namedRule` theRule where
--    theRule p = do
--        (x,y)     <- match opEq p
--        TypeGraph{} <- typeOf x
--        TypeGraph{} <- typeOf y
--        return
--            ( "Horizontal rule for set equality"
--            , return $ make opAnd $ fromList
--                [ make opSubsetEq x y
--                , make opSubsetEq y x
--                ]
--            )

rule_Subgraph :: Rule
rule_Subgraph = "subgraph" `namedRule` theRule where
    theRule p = do
        (x,y)     <- match opSubgraph p
        TypeGraph{} <- typeOf x
        TypeGraph{} <- typeOf y
        return
            ( "Horizontal rule for subgraph"
            , return [essence| verts(&x) subsetEq verts(&y)
                            /\ edges(&x) subsetEq edges(&y) |]
            )
