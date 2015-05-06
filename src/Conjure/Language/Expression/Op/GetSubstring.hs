{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Expression.Op.GetSubstring where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common

import qualified Data.Aeson as JSON             -- aeson
import qualified Data.HashMap.Strict as M       -- unordered-containers
import qualified Data.Vector as V               -- vector


data OpGetSubstring x = OpGetSubstring x x x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpGetSubstring x)
instance Hashable  x => Hashable  (OpGetSubstring x)
instance ToJSON    x => ToJSON    (OpGetSubstring x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpGetSubstring x) where parseJSON = genericParseJSON jsonOptions

instance (TypeOf x, Pretty x) => TypeOf (OpGetSubstring x) where
    typeOf p@(OpGetSubstring seq start len) = do
        tyS     <- typeOf seq
        tyStart <- typeOf start
        tyLen   <- typeOf len
        case (tyS, tyStart, tyLen) of
            (TypeSequence{}, TypeInt{}, TypeInt{}) -> return tyS
            _ -> raiseTypeError p

instance (DomainOf x x) => DomainOf (OpGetSubstring x) x where
    domainOf op@(OpGetSubstring seq _ len) = do
        DomainSequence _ _ inner <- domainOf seq
        DomainInt [RangeBounded l u] <- domainOf len
        return $ DomainSequence () (SequenceAttr (SizeAttr_MinMaxSize l u) def) inner

-- What if the numbers are no good?
instance EvaluateOp OpGetSubstring where
    evaluateOp (OpGetSubstring
        (ConstantAbstract (AbsLitSequence xs))
        (ConstantInt start)
        (ConstantInt len)) =
            return $ ConstantAbstract $ AbsLitSequence $ take (fromIntegral len) $ drop (fromIntegral start-1) xs
    evaluateOp op = na $ "evaluateOp{OpGetSubstring}:" <++> pretty (show op)

instance SimplifyOp OpGetSubstring x where
    simplifyOp _ = na "simplifyOp{OpGetSubstring}"

instance Pretty x => Pretty (OpGetSubstring x) where
    prettyPrec _ (OpGetSubstring a b c) = "getSubstring" <> prettyList prParens "," [a,b,c]

instance VarSymBreakingDescription x => VarSymBreakingDescription (OpGetSubstring x) where
    varSymBreakingDescription (OpGetSubstring a b c) = JSON.Object $ M.fromList
        [ ("type", JSON.String "OpGetSubstring")
        , ("children", JSON.Array $ V.fromList
            [ varSymBreakingDescription a
            , varSymBreakingDescription b
            , varSymBreakingDescription c
            ])
        ]
