{-# LANGUAGE TemplateHaskell, DerivingVia, GeneralizedNewtypeDeriving #-}
module Data.Set.Ordered
    ( OrderedSet
    , singleton
    , filter
    , null
    ) where

import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Data.Monoid.Extended (ExtendSemigroup(..))
import           Data.Semigroup (Semigroup(..))
import           GHC.Generics (Generic)

import qualified Prelude
import           Prelude hiding (filter, null)

newtype OrderedSet a = OrderedSet { _orderedSet :: [a] }
    deriving newtype (Eq, Ord, Functor, Foldable)
    deriving stock (Generic, Show, Traversable)
    deriving Monoid via (ExtendSemigroup (OrderedSet a))

instance Eq a => Semigroup (OrderedSet a) where
    OrderedSet xs <> OrderedSet ys =
        OrderedSet (xs <> Prelude.filter (`notElem` xs) ys)

Lens.makeLenses ''OrderedSet

singleton :: a -> OrderedSet a
singleton = OrderedSet . (:[])

filter :: (a -> Bool) -> OrderedSet a -> OrderedSet a
filter p = orderedSet %~ Prelude.filter p

null :: OrderedSet a -> Bool
null = Prelude.null . _orderedSet
