-- | A trivial unit monad

module Control.Monad.Unit
    ( Unit(..)
    ) where

import Data.Semigroup (Semigroup)
import Lamdu.Prelude

data Unit a = Unit
    deriving (Functor, Foldable, Traversable)

instance Semigroup (Unit a) where (<>) _ _ = Unit

instance Applicative Unit where
    pure _ = Unit
    _ <*> _ = Unit

instance Monad Unit where
    _ >>= _ = Unit
