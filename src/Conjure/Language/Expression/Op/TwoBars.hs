{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable, ViewPatterns #-}

module Conjure.Language.Expression.Op.TwoBars where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common
import Conjure.Language.DomainSizeOf
import Conjure.Language.NameGen ( runNameGen)

import qualified Data.Aeson as JSON             -- aeson
import qualified Data.HashMap.Strict as M       -- unordered-containers
import qualified Data.Vector as V               -- vector


data OpTwoBars x = OpTwoBars x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpTwoBars x)
instance Hashable  x => Hashable  (OpTwoBars x)
instance ToJSON    x => ToJSON    (OpTwoBars x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpTwoBars x) where parseJSON = genericParseJSON jsonOptions

instance (TypeOf x, Pretty x) => TypeOf (OpTwoBars x) where
    typeOf p@(OpTwoBars a) = do
        ty <- typeOf a
        case ty of
            TypeInt{}       -> return ()
            TypeList{}      -> return ()
            TypeSet{}       -> return ()
            TypeMSet{}      -> return ()
            TypeFunction{}  -> return ()
            TypeSequence{}  -> return ()
            TypeRelation{}  -> return ()
            TypePartition{} -> return ()
            _               -> raiseTypeError p
        return TypeInt

instance EvaluateOp OpTwoBars where
    evaluateOp (OpTwoBars x) =
        case x of
            -- absolute value
            ConstantInt y                      -> return $ ConstantInt $ abs y

            -- cardinality of a constant
            (viewConstantMatrix    -> Just (_, xs)) -> return $ ConstantInt $ genericLength                xs
            (viewConstantSet       -> Just xs)      -> return $ ConstantInt $ genericLength $ nub          xs
            (viewConstantMSet      -> Just xs)      -> return $ ConstantInt $ genericLength                xs
            (viewConstantFunction  -> Just xs)      -> return $ ConstantInt $ genericLength $ nub          xs
            (viewConstantSequence  -> Just xs)      -> return $ ConstantInt $ genericLength                xs
            (viewConstantRelation  -> Just xs)      -> return $ ConstantInt $ genericLength $ nub          xs
            (viewConstantPartition -> Just xs)      -> return $ ConstantInt $ genericLength $ nub $ concat xs

            -- cardinality of a domain
            DomainInConstant (DomainInt rs)    -> ConstantInt . genericLength <$> rangesInts rs
            DomainInConstant dom               -> runNameGen $ domainSizeOf dom
            _ -> na $ "evaluateOp OpTwoBars" <+> pretty (show x)

instance SimplifyOp OpTwoBars x where
    simplifyOp _ = na "simplifyOp{OpTwoBars}"

instance Pretty x => Pretty (OpTwoBars x) where
    prettyPrec _ (OpTwoBars a) = "|" <> pretty a <> "|"

instance VarSymBreakingDescription x => VarSymBreakingDescription (OpTwoBars x) where
    varSymBreakingDescription (OpTwoBars a) = JSON.Object $ M.fromList
        [ ("type", JSON.String "OpTwoBars")
        , ("children", JSON.Array $ V.fromList
            [ varSymBreakingDescription a
            ])
        ]
