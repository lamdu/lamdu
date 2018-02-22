module Data.Map.Utils
    ( setMapIntersection
    , partition
    , unionWithM
    , singleton, hasKey
    ) where

import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)

import           Prelude

singleton :: (Lens.At a, Monoid a) => Lens.Index a -> Lens.IxValue a -> a
singleton k v = mempty & Lens.at k ?~ v

hasKey :: Lens.Ixed a => Lens.Index a -> a -> Bool
hasKey = Lens.has . Lens.ix

setMapIntersection :: Ord k => Set k -> Map k a -> Map k a
setMapIntersection s m = m `Map.intersection` Map.fromSet (const ()) s

-- | Generalized Data.List.partition
partition :: Ord k => (a -> k) -> [a] -> Map k [a]
partition mkKey xs =
    xs <&> singletonMap & Map.unionsWith (++)
    where
        singletonMap x = Map.singleton (mkKey x) [x]

unionWithM :: (Applicative f, Ord k) => (a -> a -> f a) -> Map k a -> Map k a -> f (Map k a)
unionWithM f m0 m1 =
    go
    (Map.toAscList m0)
    (Map.toAscList m1)
    <&> Map.fromAscList
    where
        go [] ns = pure ns
        go ms [] = pure ms
        go ((k0,v0):ms) ((k1,v1):ns)
            | k0 < k1 = go ms ((k1,v1):ns) <&> ((k0,v0):)
            | k1 < k0 = go ((k0,v0):ms) ns <&> ((k1,v1):)
            | otherwise = (:) . (,) k0 <$> f v0 v1 <*> go ms ns
