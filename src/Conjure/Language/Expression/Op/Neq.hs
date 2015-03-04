{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Expression.Op.Neq where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common


data OpNeq x = OpNeq x x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpNeq x)
instance Hashable  x => Hashable  (OpNeq x)
instance ToJSON    x => ToJSON    (OpNeq x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpNeq x) where parseJSON = genericParseJSON jsonOptions

instance BinaryOperator (OpNeq x) where
    opLexeme _ = L_Neq

instance (TypeOf x, Pretty x) => TypeOf (OpNeq x) where
    typeOf (OpNeq a b) = sameToSameToBool a b

instance Pretty x => DomainOf (OpNeq x) x where
    domainOf op = na $ "evaluateOp{OpNeq}:" <++> pretty op

instance EvaluateOp OpNeq where
    evaluateOp (OpNeq ConstantUndefined{} _) = return $ fromBool False
    evaluateOp (OpNeq _ ConstantUndefined{}) = return $ fromBool False
    evaluateOp (OpNeq x y) = return $ ConstantBool $ x /= y

instance SimplifyOp OpNeq x where
    simplifyOp _ = na "simplifyOp{OpNeq}"

instance Pretty x => Pretty (OpNeq x) where
    prettyPrec prec op@(OpNeq a b) = prettyPrecBinOp prec [op] a b