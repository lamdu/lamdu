{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Lamdu.Data.Infer.Monad
  ( InferT(..), liftContext, inferError
  ) where

import Control.Applicative (Applicative)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Either (EitherT)
import Control.Monad.Trans.State (StateT(..))
import qualified Control.Monad.Trans.Either as Either
import qualified Lamdu.Data.Infer.Internal as I

newtype InferT def m a = InferT { runInferT :: StateT (I.Context def) (EitherT (I.Error def) m) a }
  deriving (Functor, Applicative, Monad)

liftContext :: StateT (I.Context def) (EitherT (I.Error def) m) a -> InferT def m a
liftContext = InferT

liftError :: Monad m => EitherT (I.Error def) m a -> InferT def m a
liftError = liftContext . lift

inferError :: Monad m => I.Error def -> InferT def m a
inferError = liftError . Either.left

instance MonadTrans (InferT def) where
  lift = liftError . lift
