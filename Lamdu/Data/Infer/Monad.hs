{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Lamdu.Data.Infer.Monad
  ( InferT(..), liftContext, inferError
  ) where

import Control.Applicative (Applicative)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Either (EitherT)
import Control.Monad.Trans.State (StateT(..))
import Lamdu.Data.Infer.Internal
import qualified Control.Monad.Trans.Either as Either

newtype InferT def m a = InferT { runInferT :: StateT (Context def) (EitherT (Error def) m) a }
  deriving (Functor, Applicative, Monad)

liftContext :: StateT (Context def) (EitherT (Error def) m) a -> InferT def m a
liftContext = InferT

liftError :: Monad m => EitherT (Error def) m a -> InferT def m a
liftError = liftContext . lift

inferError :: Monad m => Error def -> InferT def m a
inferError = liftError . Either.left

instance MonadTrans (InferT def) where
  lift = liftError . lift
