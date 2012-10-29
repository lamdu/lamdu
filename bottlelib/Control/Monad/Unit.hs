{-# LANGUAGE DeriveFunctor #-}
module Control.Monad.Unit(Unit(..)) where

import Control.Applicative (Applicative(..))
import Control.Monad (ap)

data Unit a = Unit
  deriving (Functor)

instance Applicative Unit where
  pure = return
  (<*>) = ap

instance Monad Unit where
  return = const Unit
  _ >>= _ = Unit
