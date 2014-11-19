{-# LANGUAGE FlexibleContexts #-}

module Conjure.Representations
    ( downD, downC, up
    , downD1, downC1, up1
    , downToX1
    , reprOptions, getStructurals
    , downX1
    ) where

-- conjure
import Conjure.Bug
import Conjure.Prelude
import Conjure.Language.Definition
import Conjure.Language.Type
import Conjure.Language.Ops
import Conjure.Language.TypeOf
import Conjure.Language.Pretty
import Conjure.Representations.Combined


-- | Refine (down) an expression (X), one level (1).
downX1 :: MonadFail m => Expression -> m [Expression]
downX1 (Constant x) = onConstant x
downX1 (AbstractLiteral x) = onAbstractLiteral x
downX1 (Reference x (Just refTo)) = onReference x refTo
downX1 (Op x) = onOp x
downX1 x@WithLocals{} = fail ("downX1:" <++> pretty (show x))
downX1 x = bug ("downX1:" <++> pretty (show x))

onConstant :: MonadFail m => Constant -> m [Expression]
onConstant (ConstantAbstract (AbsLitTuple xs)) = return (map Constant xs)
onConstant x = bug ("downX1.onConstant:" <++> pretty (show x))

onAbstractLiteral :: MonadFail m => AbstractLiteral Expression -> m [Expression]
onAbstractLiteral (AbsLitTuple xs) = return xs
onAbstractLiteral x = bug ("downX1.onAbstractLiteral:" <++> pretty (show x))

onReference :: MonadFail m => Name -> ReferenceTo -> m [Expression]
onReference nm refTo =
    case refTo of
        Alias{}                   -> bug  ("downX1.onReference.Alias:"           <++> pretty (show nm))
        InComprehension{}         -> fail ("downX1.onReference.InComprehension:" <++> pretty (show nm))
        DeclNoRepr{}              -> fail ("downX1.onReference.DeclNoRepr:"      <++> pretty (show nm))
        DeclHasRepr forg _ domain -> downToX1 forg nm domain

onOp :: MonadFail m => Ops Expression -> m [Expression]
onOp (MkOpIndexing (OpIndexing m i)) = do
    TypeMatrix{} <- typeOf m
    xs <- downX1 m
    let iIndexed x = Op (MkOpIndexing (OpIndexing x i))
    return (map iIndexed xs)
onOp op = fail ("downX1.onOp:" <++> pretty op)
