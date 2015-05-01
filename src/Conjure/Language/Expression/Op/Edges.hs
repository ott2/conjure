{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Expression.Op.Edges where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common

import qualified Data.Aeson as JSON             -- aeson
import qualified Data.HashMap.Strict as M       -- unordered-containers
import qualified Data.Vector as V               -- vector


data OpEdges x = OpEdges x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpEdges x)
instance Hashable  x => Hashable  (OpEdges x)
instance ToJSON    x => ToJSON    (OpEdges x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpEdges x) where parseJSON = genericParseJSON jsonOptions

instance (Pretty x, TypeOf x) => TypeOf (OpEdges x) where
    typeOf p@(OpEdges x) = do
        ty <- typeOf x
        case ty of
            TypeGraph a -> return . TypeSet $ TypeTuple [a,a]
            _           -> raiseTypeError p

instance (Pretty x, DomainOf x x) => DomainOf (OpEdges x) x where
    domainOf (OpEdges f) = do
        fDom <- domainOf f
        case fDom of
            DomainGraph _ _ inner -> return $ DomainSet def def $ DomainTuple [inner,inner]
            _ -> fail "domainOf, OpEdges, not a graph"

instance EvaluateOp OpEdges where
    evaluateOp (OpEdges (ConstantAbstract (AbsLitGraph xs))) =
        error $ show xs
        --return (ConstantAbstract (AbsLitSet (sortNub (map mkE xs))))
      where mkE (u,xs) = map (\x -> ConstantAbstract . AbsLitTuple $ [u,x]) xs
    evaluateOp op = na $ "evaluateOp{OpEdges}:" <++> pretty (show op)

instance SimplifyOp OpEdges x where
    simplifyOp _ = na "simplifyOp{OpEdges}"

instance Pretty x => Pretty (OpEdges x) where
    prettyPrec _ (OpEdges a) = "edges" <> prParens (pretty a)

instance VarSymBreakingDescription x => VarSymBreakingDescription (OpEdges x) where
    varSymBreakingDescription (OpEdges a) = error ""
--    varSymBreakingDescription (OpRange a) = JSON.Object $ M.fromList
--        [ ("type", JSON.String "OpRange")
--        , ("children", JSON.Array $ V.fromList
--            [ varSymBreakingDescription a
--            ])
--        ]
