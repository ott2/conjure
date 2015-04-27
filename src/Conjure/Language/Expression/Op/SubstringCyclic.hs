{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Expression.Op.SubstringCyclic where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common

import qualified Data.Aeson as JSON             -- aeson
import qualified Data.HashMap.Strict as M       -- unordered-containers
import qualified Data.Vector as V               -- vector


data OpSubstringCyclic x = OpSubstringCyclic x x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpSubstringCyclic x)
instance Hashable  x => Hashable  (OpSubstringCyclic x)
instance ToJSON    x => ToJSON    (OpSubstringCyclic x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpSubstringCyclic x) where parseJSON = genericParseJSON jsonOptions

instance BinaryOperator (OpSubstringCyclic x) where
    opLexeme _ = L_substringCyclic

instance (TypeOf x, Pretty x) => TypeOf (OpSubstringCyclic x) where
    typeOf p@(OpSubstringCyclic a b) = do
        tya <- typeOf a
        tyb <- typeOf b
        case (tya, tyb) of
            (TypeSequence{}, TypeSequence{}) -> return TypeBool
            _ -> raiseTypeError p

instance DomainOf (OpSubstringCyclic x) x where
    domainOf _ = fail "domainOf{OpSubstringCyclic}"

instance EvaluateOp OpSubstringCyclic where
    evaluateOp (OpSubstringCyclic
        (ConstantAbstract (AbsLitSequence xs))
        (ConstantAbstract (AbsLitSequence ys))) =
            return $ fromBool $
                or [ isInfixOf zs xs
                   | zs <- take (length ys + length xs - 1) $ repeat ys
                   ]
    evaluateOp op = na $ "evaluateOp{OpSubstringCyclic}:" <++> pretty (show op)

instance SimplifyOp OpSubstringCyclic x where
    simplifyOp _ = na "simplifyOp{OpSubstringCyclic}"

instance Pretty x => Pretty (OpSubstringCyclic x) where
    prettyPrec prec op@(OpSubstringCyclic a b) = prettyPrecBinOp prec [op] a b

instance VarSymBreakingDescription x => VarSymBreakingDescription (OpSubstringCyclic x) where
    varSymBreakingDescription (OpSubstringCyclic a b) = JSON.Object $ M.fromList
        [ ("type", JSON.String "OpSubstringCyclic")
        , ("children", JSON.Array $ V.fromList
            [ varSymBreakingDescription a
            , varSymBreakingDescription b
            ])
        ]
