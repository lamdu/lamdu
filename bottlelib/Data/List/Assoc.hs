module Data.List.Assoc
  ( at, match
  ) where

import Control.Applicative (Applicative)
import Control.Monad (guard)
import Control.Lens (LensLike')
import qualified Control.Lens as Lens
import qualified Data.List.Utils as List

at :: (Applicative f, Eq k) => k -> LensLike' f [(k, a)] a
at key = Lens.traverse . Lens.filtered ((== key) . fst) . Lens._2

match :: Eq k => (a -> b -> c) -> [(k, a)] -> [(k, b)] -> Maybe [(k, c)]
match f xs ys =
  sequence =<< List.match matchItem xs ys
  where
    matchItem (k0, v0) (k1, v1) = do
      guard $ k0 == k1
      return (k0, f v0 v1)
