{-# LANGUAGE FlexibleContexts #-}

module Phases.ReprRefnCommon where

import Control.Applicative ( Applicative )
import Control.Monad.Error ( MonadError, throwError )
import Control.Monad.IO.Class ( MonadIO(..) )
import Control.Monad.State ( MonadState, get )
import Data.Generics.Uniplate.Direct ( Biplate, transformBi )

import Language.Essence
import Language.EssenceEvaluator ( runEvaluateExpr )
import Language.EssencePrinters ( prExpr )
import PrintUtils
import Utils


type ErrMsg = String

instantiateNames ::
    ( MonadState [Binding] m
    , MonadIO m
    , Biplate a Expr
    , Show a
    ) => a -> m a
instantiateNames x = do
    st <- get
    -- liftIO $ putStrLn "instantiateNames"
    -- liftIO $ print x
    -- liftIO $ print $ map snd3 st
    let
        -- funcs :: [a -> a]
        funcs = flip map st $ \ t -> case t of (InRule, nm, val) -> instantiateName nm val
                                               _                 -> id
    return $ applyAll x funcs

instantiateName :: Biplate a Expr => String -> Expr -> a -> a
instantiateName nm x = transformBi f
    where
        f (Identifier nm') | nm == nm' = x
        f y = y

checkWheres ::
    ( Applicative m
    , MonadError ErrMsg m
    , MonadState [Binding] m
    , MonadIO m
    ) => Where -> m ()
checkWheres x = do
    bindings  <- get
    (x',logs) <- runEvaluateExpr bindings x
    case x' of
        ValueBoolean True  -> return ()
        ValueBoolean False -> do
            xOut <- maybe (throwError ("cannot render: " ++ show x)) return $ prExpr x
            throwError . renderDoc $ text "where statement evaluated to false:" <+> xOut
        _                  -> do
            xOut <- maybe (throwError ("cannot render: " ++ show x)) return $ prExpr x
            throwError . renderDoc $ text "where statement cannot be fully evaluated:" <+> xOut
                              $+$ vcat (map text logs)
