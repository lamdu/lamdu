module Data.Function.Decycle (decycleOn, decycle) where

import Control.Lens.Operators
import qualified Control.Lens as Lens
import qualified Data.Set as Set

-- | A fix for functions that terminates recursive cycles
decycleOn :: Ord b => (a -> b) -> res -> ((a -> res) -> a -> res) -> a -> res
decycleOn toOrd alreadyVisited notVisited =
  go Set.empty
  where
    go visited x
      | visited ^. Lens.contains o = alreadyVisited
      | otherwise = notVisited (go (visited & Lens.contains o .~ True)) x
      where
        o = toOrd x

decycle :: Ord a => res -> ((a -> res) -> a -> res) -> a -> res
decycle = decycleOn id
