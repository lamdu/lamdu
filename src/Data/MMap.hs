{-# LANGUAGE DerivingVia #-}
-- | A Data.Map wrapper with saner Semigroup/Monoid instances
{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, TypeFamilies, FlexibleInstances, MultiParamTypeClasses #-}
module Data.MMap
    ( MMap(..), _MMap
    , fromList
    , fromSet, keysSet
    , filter, mapMaybe
    , unionWithM, unionWith
    ) where

import qualified Control.Lens as Lens
import           Control.Lens.Operators
import qualified Data.Map.Extended as Map

import           Lamdu.Prelude hiding (filter)

-- | A Map with a sensible Monoid/Semigroup instance
newtype MMap k v = MMap (Map k v)
    deriving newtype (Eq, Ord, Binary, Functor, Foldable)
    deriving stock (Generic, Traversable, Show, Read)
    deriving Monoid via Generically (MMap k v)

instance (Ord k, Semigroup v) => Semigroup (MMap k v) where
    MMap m1 <> MMap m2 = Map.unionWith (<>) m1 m2 & MMap

Lens.makePrisms ''MMap

instance Lens.FunctorWithIndex k (MMap k) where imap f = _MMap %~ Lens.imap f
instance Lens.FoldableWithIndex k (MMap k) where ifoldMap f = Lens.ifoldMap f . (^. _MMap)
instance Lens.TraversableWithIndex k (MMap k) where itraverse f = _MMap %%~ Lens.itraverse f

type instance Lens.Index (MMap k _) = k
type instance Lens.IxValue (MMap _ v) = v
instance Ord k => Lens.Ixed (MMap k v) where ix k = _MMap . Lens.ix k
instance Ord k => Lens.At (MMap k v) where at k = _MMap . Lens.at k

fromList :: (Ord k, Semigroup v) => [(k, v)] -> MMap k v
fromList = MMap . Map.fromListWith (<>)

fromSet :: (k -> v) -> Set k -> MMap k v
fromSet f = MMap . Map.fromSet f

keysSet :: MMap k v -> Set k
keysSet (MMap m) = Map.keysSet m

filter :: (v -> Bool) -> MMap k v -> MMap k v
filter p = _MMap %~ Map.filter p

mapMaybe :: (a -> Maybe b) -> MMap k a -> MMap k b
mapMaybe f = _MMap %~ Map.mapMaybe f

unionWithM ::
    (Applicative f, Ord k) =>
    (a -> a -> f a) -> MMap k a -> MMap k a -> f (MMap k a)
unionWithM f (MMap x) (MMap y) = Map.unionWithM f x y <&> MMap

unionWith :: Ord k => (a -> a -> a) -> MMap k a -> MMap k a -> MMap k a
unionWith f (MMap x) (MMap y) = Map.unionWith f x y & MMap
