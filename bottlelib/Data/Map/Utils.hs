module Data.Map.Utils (lookupOrSelf, pop, popKeys, matchKeys) where

import Control.Lens.Operators
import Control.Monad (guard)
import Control.Monad.Trans.State (StateT(..))
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Traversable (Traversable(..))
import qualified Control.Lens as Lens
import qualified Data.Map as Map

lookupOrSelf :: Ord a => Map a a -> a -> a
lookupOrSelf m x = fromMaybe x $ m ^. Lens.at x

pop :: Ord k => k -> Map k v -> Maybe (v, Map k v)
pop k m =
  Map.lookup k m <&> f
  where
    f v = (v, Map.delete k m)

popKeys :: (Traversable t, Ord k) => t k -> Map k v -> Maybe (t v, Map k v)
popKeys = runStateT . traverse (StateT . pop)

matchKeys :: (Traversable t, Ord k) => t k -> Map k v -> Maybe (t v)
matchKeys keys m = do
  (vals, rest) <- popKeys keys m
  guard $ Map.null rest
  return vals
