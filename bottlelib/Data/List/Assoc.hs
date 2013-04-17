module Data.List.Assoc
  ( match
  ) where

import Control.Monad (guard)
import qualified Data.List.Utils as List

match :: Eq k => (a -> b -> c) -> [(k, a)] -> [(k, b)] -> Maybe [(k, c)]
match f xs ys =
  sequence =<< List.match matchItem xs ys
  where
    matchItem (k0, v0) (k1, v1) = do
      guard $ k0 == k1
      return (k0, f v0 v1)
