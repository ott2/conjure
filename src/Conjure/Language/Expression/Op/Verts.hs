{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Expression.Op.Verts where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common

import qualified Data.Aeson as JSON             -- aeson
import qualified Data.HashMap.Strict as M       -- unordered-containers
import qualified Data.Vector as V               -- vector


data OpVerts x = OpVerts x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpVerts x)
instance Hashable  x => Hashable  (OpVerts x)
instance ToJSON    x => ToJSON    (OpVerts x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpVerts x) where parseJSON = genericParseJSON jsonOptions

instance (Pretty x, TypeOf x) => TypeOf (OpVerts x) where
    typeOf p@(OpVerts x) = do
        ty <- typeOf x
        case ty of
            TypeGraph a -> return (TypeSet a)
            _           -> raiseTypeError p

instance (Pretty x, DomainOf x x) => DomainOf (OpVerts x) x where
    domainOf (OpVerts f) = do
        fDom <- domainOf f
        case fDom of
            DomainGraph _ _ inner -> return $ DomainSet def def inner
            _ -> fail "domainOf, OpVerts, not a graph"

instance EvaluateOp OpVerts where
    evaluateOp (OpVerts (ConstantAbstract (AbsLitGraph xs))) =
        return (ConstantAbstract (AbsLitSet (sortNub (map fst xs))))
    evaluateOp op = na $ "evaluateOp{OpVerts}:" <++> pretty (show op)

instance SimplifyOp OpVerts x where
    simplifyOp _ = na "simplifyOp{OpVerts}"

instance Pretty x => Pretty (OpVerts x) where
    prettyPrec _ (OpVerts a) = "verts" <> prParens (pretty a)

instance VarSymBreakingDescription x => VarSymBreakingDescription (OpVerts x) where
    varSymBreakingDescription (OpVerts a) = error ""
--    varSymBreakingDescription (OpRange a) = JSON.Object $ M.fromList
--        [ ("type", JSON.String "OpRange")
--        , ("children", JSON.Array $ V.fromList
--            [ varSymBreakingDescription a
--            ])
--        ]
