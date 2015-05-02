{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Expression.Op.Subgraph where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common

import qualified Data.Aeson as JSON             -- aeson
import qualified Data.HashMap.Strict as M       -- unordered-containers
import qualified Data.Vector as V
import qualified Data.Set as S


data OpSubgraph x = OpSubgraph x x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpSubgraph x)
instance Hashable  x => Hashable  (OpSubgraph x)
instance ToJSON    x => ToJSON    (OpSubgraph x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpSubgraph x) where parseJSON = genericParseJSON jsonOptions

instance BinaryOperator (OpSubgraph x) where
    opLexeme _ = L_subgraph

instance (TypeOf x, Pretty x) => TypeOf (OpSubgraph x) where
    typeOf p@(OpSubgraph a b) = do
        tya <- typeOf a
        tyb <- typeOf b
        case (tya, tyb) of
            (TypeGraph{}, TypeGraph{}) -> return TypeBool
            _ -> raiseTypeError p

instance DomainOf (OpSubgraph x) x where
    domainOf _ = fail "domainOf{OpSubgraph}"

instance EvaluateOp OpSubgraph where
    evaluateOp (OpSubgraph
        (ConstantAbstract (AbsLitGraph xs))
        (ConstantAbstract (AbsLitGraph ys))) =
            return $ fromBool $ S.isSubsetOf xs' ys'
            where xs' = S.fromList . concat $ map mkE xs
                  ys' = S.fromList . concat $ map mkE ys
                  mkE (u,vs) = map (\v -> ConstantAbstract $ AbsLitTuple [u,v]) vs
    evaluateOp op = na $ "evaluateOp{OpSubgraph}:" <++> pretty (show op)

instance SimplifyOp OpSubgraph x where
    simplifyOp _ = na "simplifyOp{OpSubgraph}"

instance Pretty x => Pretty (OpSubgraph x) where
    prettyPrec prec op@(OpSubgraph a b) = prettyPrecBinOp prec [op] a b

instance VarSymBreakingDescription x => VarSymBreakingDescription (OpSubgraph x) where
    varSymBreakingDescription (OpSubgraph a b) = JSON.Object $ M.fromList
        [ ("type", JSON.String "OpSubgraph")
        , ("children", JSON.Array $ V.fromList
            [ varSymBreakingDescription a
            , varSymBreakingDescription b
            ])
        ]
