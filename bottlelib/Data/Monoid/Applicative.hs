module Data.Monoid.Applicative (ApplicativeMonoid(..)) where

import Control.Applicative (Applicative(..), (<$>))
import Data.Monoid (Monoid(..))

newtype ApplicativeMonoid f a =
  ApplicativeMonoid { runApplicativeMonoid :: f a }

instance (Monoid a, Applicative f) => Monoid (ApplicativeMonoid f a) where
  mempty = ApplicativeMonoid $ pure mempty
  {-# INLINE mempty #-}
  ApplicativeMonoid x `mappend` ApplicativeMonoid y =
    ApplicativeMonoid $ mappend <$> x <*> y
  {-# INLINE mappend #-}