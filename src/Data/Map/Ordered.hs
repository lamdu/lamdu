{-# LANGUAGE DeriveFunctor #-}
module Data.Map.Ordered(
  OrderedMap,
  singleton, fromList,
  toMap, keys, elems,
  unionWith, difference,
  mapMaybe, mapKeys,
  intersectionWith)
where

import Data.Monoid(Monoid(..))
import Data.Map(Map, (!))
import Data.List(nub)
import qualified Data.Map as Map

data OrderedMap k a = OrderedMap {
  toMap :: Map k a,
  keys :: [k]
  }
  deriving (Functor)

-- Same kludgy Monoid instance as Map's
instance Ord k => Monoid (OrderedMap k a) where
  mempty = OrderedMap mempty mempty
  mappend = unionWith const

fromList :: Ord k => [(k, a)] -> OrderedMap k a
fromList items =
  OrderedMap (Map.fromList items)
  -- TODO: Faster algorithm for this:
  (reverse . nub . reverse $ map fst items)

singleton :: Ord k => k -> a -> OrderedMap k a
singleton k a = OrderedMap (Map.singleton k a) [k]

unionWith ::
  Ord k =>
  (a -> a -> a) -> OrderedMap k a -> OrderedMap k a -> OrderedMap k a
unionWith f (OrderedMap aMap aOrder) (OrderedMap bMap bOrder) =
  OrderedMap (Map.unionWith f aMap bMap) (aOrder ++ filter (`Map.notMember` aMap) bOrder)

difference ::
  Ord k => OrderedMap k a -> OrderedMap k b -> OrderedMap k a
difference (OrderedMap aMap aOrder) (OrderedMap bMap _) =
  OrderedMap (Map.difference aMap bMap) $
  filter (`Map.notMember` bMap) aOrder

mapMaybe :: Ord k => (a -> Maybe b) -> OrderedMap k a -> OrderedMap k b
mapMaybe f (OrderedMap aMap aOrder) = OrderedMap resultMap resultOrder
  where
    resultOrder = filter (`Map.member` resultMap) aOrder
    resultMap = Map.mapMaybe f aMap

-- preserves the key order
mapKeys :: (Ord k1, Ord k2) => (k1 -> k2) -> OrderedMap k1 a -> OrderedMap k2 a
mapKeys f (OrderedMap aMap aOrder) = OrderedMap (Map.mapKeys f aMap) (map f aOrder)

-- preserves the key order from the first map argument
intersectionWith ::
  Ord k =>
  (a -> b -> c) -> OrderedMap k a -> OrderedMap k b -> OrderedMap k c
intersectionWith f (OrderedMap aMap aOrder) (OrderedMap bMap _) =
  OrderedMap resultMap resultOrder
  where
    resultMap = Map.intersectionWith f aMap bMap
    resultOrder = filter (`Map.member` bMap) aOrder

elems :: Ord k => OrderedMap k a -> [a]
elems x = map (toMap x !) (keys x)
