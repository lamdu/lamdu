module Data.Function.Decycle (decycleOn, decycle) where

import Control.Lens.Operators
import qualified Control.Lens as Lens
import qualified Data.Set as Set

-- | A fix for functions that terminates recursive cycles
decycleOn :: Ord b => (a -> b) -> (Maybe (a -> res) -> a -> res) -> a -> res
decycleOn toOrd f =
  go Set.empty
  where
    go visited x = f (mRecurse visited (toOrd x)) x
    mRecurse visited o
      | visited ^. Lens.contains o = Nothing
      | otherwise = visited & Lens.contains o .~ True & go & Just

decycle :: Ord a => (Maybe (a -> res) -> a -> res) -> a -> res
decycle = decycleOn id
