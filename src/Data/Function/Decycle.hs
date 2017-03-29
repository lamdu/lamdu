module Data.Function.Decycle (decycleOn, decycle) where

import qualified Control.Lens as Lens
import           Control.Lens.Operators
import qualified Data.Set as Set

-- | A fix for functions that terminates recursive cycles
decycleOn :: Ord b => (a -> b) -> (a -> Maybe (a -> res) -> res) -> a -> res
decycleOn toOrd f =
    go Set.empty
    where
        go visited x = f x (mRecurse visited (toOrd x))
        mRecurse visited o
            | visited ^. Lens.contains o = Nothing
            | otherwise = visited & Lens.contains o .~ True & go & Just

decycle :: Ord a => (a -> Maybe (a -> res) -> res) -> a -> res
decycle = decycleOn id
