{-# LANGUAGE NoImplicitPrelude #-}
module Data.Map.Utils
    ( setMapIntersection
    , partition
    , unionWithM
    ) where

import qualified Data.Map as Map

import           Lamdu.Prelude

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
