{-# LANGUAGE NoImplicitPrelude #-}
module Data.Map.Utils
    ( setMapIntersection
    ) where

import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import           Prelude.Compat

setMapIntersection :: Ord k => Set k -> Map k a -> Map k a
setMapIntersection s m = m `Map.intersection` Map.fromSet (const ()) s
