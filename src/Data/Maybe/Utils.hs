module Data.Maybe.Utils
    ( unsafeUnjust, maybeToMPlus, unionMaybeWith
    ) where

import Control.Monad (MonadPlus(..))

unsafeUnjust :: String -> Maybe a -> a
unsafeUnjust msg Nothing = error ("unsafeUnjust: " ++ msg)
unsafeUnjust _ (Just x) = x

maybeToMPlus :: MonadPlus m => Maybe a -> m a
maybeToMPlus Nothing = mzero
maybeToMPlus (Just x) = return x

-- | Similar to Map's @unionWith@
unionMaybeWith :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
unionMaybeWith _ Nothing x = x
unionMaybeWith _ x Nothing = x
unionMaybeWith f (Just a) (Just b) = Just (f a b)
