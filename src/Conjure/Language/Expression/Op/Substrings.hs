{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Expression.Op.Substrings where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common

import qualified Data.Aeson as JSON             -- aeson
import qualified Data.HashMap.Strict as M       -- unordered-containers
import qualified Data.Vector as V               -- vector


data OpSubstrings x = OpSubstrings x x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpSubstrings x)
instance Hashable  x => Hashable  (OpSubstrings x)
instance ToJSON    x => ToJSON    (OpSubstrings x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpSubstrings x) where parseJSON = genericParseJSON jsonOptions

instance BinaryOperator (OpSubstrings x) where
    opLexeme _ = L_substrings

instance (TypeOf x, Pretty x) => TypeOf (OpSubstrings x) where
    typeOf p@(OpSubstrings s l) = do
        tys <- typeOf s
        tyl <- typeOf l
        case (tys, tyl) of
            (TypeSequence inner, TypeInt{}) -> return . TypeSet $ TypeSequence inner
            _ -> raiseTypeError p

instance (DomainOf x x) => DomainOf (OpSubstrings x) x where
    domainOf p@(OpSubstrings s l) = do
      DomainSequence _ _ inner <- domainOf s
      return $ DomainSet () def $ DomainSequence () def inner

instance EvaluateOp OpSubstrings where
    evaluateOp (OpSubstrings
        (ConstantAbstract (AbsLitSequence xs))
        (ConstantInt i)) =
            return $ ConstantAbstract $ AbsLitSet $ mapMaybe go $ tails xs
      where go xs | length xs < fromIntegral i = Nothing
                  | otherwise     = Just . ConstantAbstract . AbsLitSequence $ take (fromIntegral i) xs

instance SimplifyOp OpSubstrings x where
    simplifyOp _ = na "simplifyOp{OpSubstrings}"

instance Pretty x => Pretty (OpSubstrings x) where
    prettyPrec _ (OpSubstrings a b) = "substrings" <> prettyList prParens "," [a,b]

instance VarSymBreakingDescription x => VarSymBreakingDescription (OpSubstrings x) where
    varSymBreakingDescription (OpSubstrings a b) = JSON.Object $ M.fromList
        [ ("type", JSON.String "OpSubstrings")
        , ("children", JSON.Array $ V.fromList
            [ varSymBreakingDescription a
            , varSymBreakingDescription b
            ])
        ]
