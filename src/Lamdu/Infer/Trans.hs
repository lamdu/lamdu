-- | A convenience monad transformer that can load, infer, unify and update.
-- Allows binding together actions of all these kinds.
{-# LANGUAGE NoImplicitPrelude #-}
module Lamdu.Infer.Trans
    ( M
    , liftInfer
    , liftInner
    , run
    ) where

import           Control.Monad.Trans.Either (EitherT(..), hoistEither)
import           Control.Monad.Trans.State (StateT(..), mapStateT)
import           Lamdu.Infer (Infer)
import qualified Lamdu.Infer as Infer
import qualified Lamdu.Infer.Error as InferErr

import           Lamdu.Prelude

type M m = StateT Infer.Context (EitherT InferErr.Error m)

liftInfer :: Monad m => Infer a -> M m a
liftInfer = mapStateT hoistEither . Infer.run

liftInner :: Monad m => m a -> M m a
liftInner = lift . lift

run :: M m a -> m (Either InferErr.Error (a, Infer.Context))
run = runEitherT . (`runStateT` Infer.initialContext)
