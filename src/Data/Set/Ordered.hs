{-# LANGUAGE TemplateHaskell #-}
module Data.Set.Ordered
    ( OrderedSet
    , singleton
    , filter
    , null
    ) where

import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Data.Semigroup (Semigroup(..))
import qualified Prelude
import           Prelude hiding (filter, null)

newtype OrderedSet a = OrderedSet { _orderedSet :: [a] }
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)
Lens.makeLenses ''OrderedSet

instance Eq a => Semigroup (OrderedSet a) where
    OrderedSet xs <> OrderedSet ys = OrderedSet (xs ++ Prelude.filter (`notElem` xs) ys)
instance Eq a => Monoid (OrderedSet a) where
    mempty = OrderedSet []
    mappend = (<>)

singleton :: a -> OrderedSet a
singleton = OrderedSet . (:[])

filter :: (a -> Bool) -> OrderedSet a -> OrderedSet a
filter p = orderedSet %~ Prelude.filter p

null :: OrderedSet a -> Bool
null = Prelude.null . _orderedSet
