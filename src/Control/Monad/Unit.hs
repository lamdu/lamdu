{-# LANGUAGE DerivingVia #-}

-- | A trivial unit monad

module Control.Monad.Unit
    ( Unit(..)
    ) where

import Data.Semigroup (Semigroup)
import Lamdu.Prelude

data Unit a = Unit
    deriving stock (Generic, Generic1, Functor, Foldable, Traversable)
    deriving Semigroup via Generically (Unit a)
    deriving Applicative via Generically1 Unit

instance Monad Unit where
    _ >>= _ = Unit
