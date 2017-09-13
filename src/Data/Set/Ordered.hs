{-# LANGUAGE NoImplicitPrelude, TemplateHaskell, DeriveFoldable, DeriveTraversable #-}
module Data.Set.Ordered
    ( OrderedSet
    , singleton
    , filter
    ) where

import qualified Control.Lens as Lens
import qualified Lamdu.Prelude as Prelude
import           Lamdu.Prelude hiding (filter)

newtype OrderedSet a = OrderedSet { _orderedSet :: [a] }
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)
Lens.makeLenses ''OrderedSet

instance Eq a => Monoid (OrderedSet a) where
    mempty = OrderedSet []
    OrderedSet xs `mappend` OrderedSet ys =
        OrderedSet $ xs ++ Prelude.filter (`notElem` xs) ys

singleton :: a -> OrderedSet a
singleton = OrderedSet . (:[])

filter :: (a -> Bool) -> OrderedSet a -> OrderedSet a
filter p = orderedSet %~ Prelude.filter p
