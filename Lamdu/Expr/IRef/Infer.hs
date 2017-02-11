{-# LANGUAGE NoImplicitPrelude #-}
-- | Infer expressions where GlobalId's are known to be DefI's
module Lamdu.Expr.IRef.Infer
    ( M
    , liftInfer
    , liftTransaction
    , run
    ) where

import           Control.Monad.Trans.Either (EitherT(..), hoistEither)
import           Control.Monad.Trans.State (StateT(..), mapStateT)
import           Data.Store.Transaction (Transaction)
import           Lamdu.Infer (Infer)
import qualified Lamdu.Infer as Infer
import qualified Lamdu.Infer.Error as InferErr

import           Lamdu.Prelude

type T = Transaction

type M m = StateT Infer.Context (EitherT InferErr.Error (T m))

liftInfer :: Monad m => Infer a -> M m a
liftInfer = mapStateT hoistEither . Infer.run

liftTransaction :: Monad m => T m a -> M m a
liftTransaction = lift . lift

run :: M m a -> T m (Either InferErr.Error (a, Infer.Context))
run = runEitherT . (`runStateT` Infer.initialContext)
