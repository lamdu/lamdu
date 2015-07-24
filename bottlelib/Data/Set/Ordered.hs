{-# LANGUAGE NoImplicitPrelude, DeriveFoldable #-}
module Data.Set.Ordered
    ( OrderedSet
    , singleton
    ) where

import Prelude.Compat

newtype OrderedSet a = OrderedSet [a]
    deriving (Show, Eq, Ord, Foldable)
instance Eq a => Monoid (OrderedSet a) where
    mempty = OrderedSet []
    OrderedSet xs `mappend` OrderedSet ys = OrderedSet $ xs ++ filter (`notElem` xs) ys

singleton :: a -> OrderedSet a
singleton = OrderedSet . (:[])
