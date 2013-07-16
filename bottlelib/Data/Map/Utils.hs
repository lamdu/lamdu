module Data.Map.Utils (lookupOrSelf) where

import Control.Lens.Operators
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import qualified Control.Lens as Lens

lookupOrSelf :: Ord a => Map a a -> a -> a
lookupOrSelf m x = fromMaybe x $ m ^. Lens.at x
