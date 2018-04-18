module Data.Map.Extended
    ( module Data.Map
    , setMapIntersection
    , unionWithM
    ) where

import           Data.Map
import           Data.Set (Set)

import           Prelude

setMapIntersection :: Ord k => Set k -> Map k a -> Map k a
setMapIntersection s m = m `intersection` fromSet (const ()) s

unionWithM :: (Applicative f, Ord k) => (a -> a -> f a) -> Map k a -> Map k a -> f (Map k a)
unionWithM f m0 m1 =
    fromAscList <$>
    go
    (toAscList m0)
    (toAscList m1)
    where
        go [] ns = pure ns
        go ms [] = pure ms
        go ((k0,v0):ms) ((k1,v1):ns)
            | k0 < k1 = ((k0,v0):) <$> go ms ((k1,v1):ns)
            | k1 < k0 = ((k1,v1):) <$> go ((k0,v0):ms) ns
            | otherwise = (:) . (,) k0 <$> f v0 v1 <*> go ms ns
