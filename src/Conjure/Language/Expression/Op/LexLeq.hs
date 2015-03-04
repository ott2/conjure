{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Expression.Op.LexLeq where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common


data OpLexLeq x = OpLexLeq x x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpLexLeq x)
instance Hashable  x => Hashable  (OpLexLeq x)
instance ToJSON    x => ToJSON    (OpLexLeq x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpLexLeq x) where parseJSON = genericParseJSON jsonOptions

instance BinaryOperator (OpLexLeq x) where
    opLexeme _ = L_LexLeq

instance (TypeOf x, Pretty x) => TypeOf (OpLexLeq x) where
    typeOf p@(OpLexLeq a b) = do
        tyA <- typeOf a
        tyB <- typeOf b
        if typesUnify [TypeList TypeAny, tyA, tyB]
            then return TypeBool
            else raiseTypeError p

instance Pretty x => DomainOf (OpLexLeq x) x where
    domainOf op = na $ "evaluateOp{OpLexLeq}:" <++> pretty op

instance EvaluateOp OpLexLeq where
    evaluateOp (OpLexLeq (ConstantAbstract (AbsLitMatrix _ xs)) (ConstantAbstract (AbsLitMatrix _ ys))) =
        return $ ConstantBool $ xs <= ys
    evaluateOp op = na $ "evaluateOp{OpLexLeq}:" <++> pretty (show op)

instance SimplifyOp OpLexLeq x where
    simplifyOp _ = na "simplifyOp{OpLexLeq}"

instance Pretty x => Pretty (OpLexLeq x) where
    prettyPrec prec op@(OpLexLeq a b) = prettyPrecBinOp prec [op] a b