module Data.Maybe.Utils(unsafeUnjust, maybeToMPlus) where

import Control.Monad (MonadPlus(..))

unsafeUnjust :: String -> Maybe a -> a
unsafeUnjust msg Nothing = error ("unsafeUnjust: " ++ msg)
unsafeUnjust _ (Just x) = x

maybeToMPlus :: MonadPlus m => Maybe a -> m a
maybeToMPlus Nothing = mzero
maybeToMPlus (Just x) = return x
