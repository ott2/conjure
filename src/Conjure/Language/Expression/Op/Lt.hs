{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Expression.Op.Lt where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common

import qualified Data.Aeson as JSON             -- aeson
import qualified Data.HashMap.Strict as M       -- unordered-containers
import qualified Data.Vector as V               -- vector


data OpLt x = OpLt x x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpLt x)
instance Hashable  x => Hashable  (OpLt x)
instance ToJSON    x => ToJSON    (OpLt x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpLt x) where parseJSON = genericParseJSON jsonOptions

instance BinaryOperator (OpLt x) where
    opLexeme _ = L_Lt

instance (TypeOf x, Pretty x) => TypeOf (OpLt x) where
    typeOf p@(OpLt a b) = sameToSameToBool p a b
                                [TypeBool, TypeInt, TypeEnum "?"]

instance EvaluateOp OpLt where
    evaluateOp (OpLt x y) = return $ ConstantBool $ x < y

instance SimplifyOp OpLt x where
    simplifyOp _ = na "simplifyOp{OpLt}"

instance Pretty x => Pretty (OpLt x) where
    prettyPrec prec op@(OpLt a b) = prettyPrecBinOp prec [op] a b

instance VarSymBreakingDescription x => VarSymBreakingDescription (OpLt x) where
    varSymBreakingDescription (OpLt a b) = JSON.Object $ M.fromList
        [ ("type", JSON.String "OpLt")
        , ("children", JSON.Array $ V.fromList
            [ varSymBreakingDescription a
            , varSymBreakingDescription b
            ])
        ]
