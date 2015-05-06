{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Expression.Op.SubstringsCyclic where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common

import qualified Data.Aeson as JSON             -- aeson
import qualified Data.HashMap.Strict as M       -- unordered-containers
import qualified Data.Vector as V               -- vector


data OpSubstringsCyclic x = OpSubstringsCyclic x x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpSubstringsCyclic x)
instance Hashable  x => Hashable  (OpSubstringsCyclic x)
instance ToJSON    x => ToJSON    (OpSubstringsCyclic x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpSubstringsCyclic x) where parseJSON = genericParseJSON jsonOptions

instance BinaryOperator (OpSubstringsCyclic x) where
    opLexeme _ = L_substringsCyclic

instance (TypeOf x, Pretty x) => TypeOf (OpSubstringsCyclic x) where
    typeOf p@(OpSubstringsCyclic s l) = do
        tys <- typeOf s
        tyl <- typeOf l
        case (tys, tyl) of
            (TypeSequence{}, TypeInt{}) -> return TypeBool
            _ -> raiseTypeError p

instance (DomainOf x x) => DomainOf (OpSubstringsCyclic x) x where
    domainOf p@(OpSubstringsCyclic s l) = do
      DomainSequence _ _ inner <- domainOf s
      return $ DomainSet () def $ DomainSequence () def inner
    domainOf _ = fail "domainOf{OpSubstringsCyclic}"

instance EvaluateOp OpSubstringsCyclic where
    evaluateOp (OpSubstringsCyclic
        (ConstantAbstract (AbsLitSequence xs))
        (ConstantInt i)) =
            return $ ConstantAbstract $ AbsLitSet []
    evaluateOp op = na $ "evaluateOp{OpSubstringsCyclic}:" <++> pretty (show op)

instance SimplifyOp OpSubstringsCyclic x where
    simplifyOp _ = na "simplifyOp{OpSubstringsCyclic}"

instance Pretty x => Pretty (OpSubstringsCyclic x) where
    prettyPrec _ (OpSubstringsCyclic a b) = "substringsCyclic" <> prettyList prParens "," [a,b]

instance VarSymBreakingDescription x => VarSymBreakingDescription (OpSubstringsCyclic x) where
    varSymBreakingDescription (OpSubstringsCyclic a b) = JSON.Object $ M.fromList
        [ ("type", JSON.String "OpSubstringsCyclic")
        , ("children", JSON.Array $ V.fromList
            [ varSymBreakingDescription a
            , varSymBreakingDescription b
            ])
        ]
