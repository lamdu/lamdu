{-# LANGUAGE UndecidableInstances, GeneralizedNewtypeDeriving #-}
-- | Extend the Data.Monoid module

module Data.Monoid.Extended
    ( ExtendSemigroup(..)
    ) where

import GHC.Generics (Generic, Rep, to)

import Prelude.Compat

-- Useful for deriving via - to extend an existing semigroup with a
-- mempty from the underlying type.
--
-- In other words: Replace the mappend implementation of an existing
-- type in a new instance
newtype ExtendSemigroup a = ExtendSemigroup a
    deriving newtype (Generic, Semigroup)

to' :: Generic a => Rep a () -> a
to' = to

instance
    (Semigroup a, Generic a, Monoid (Rep a ())) =>
    Monoid (ExtendSemigroup a) where
    mempty = to' mempty
