{-# LANGUAGE NoImplicitPrelude #-}
module Data.Map.Utils
    ( setMapIntersection
    , partition
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
