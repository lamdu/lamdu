module Data.Maybe.Extended
    ( module Data.Maybe
    , unsafeUnjust, maybeToMPlus, unionMaybeWith
    ) where

import           Control.Monad (MonadPlus(..))
import qualified Data.Maybe

import           Prelude

unsafeUnjust :: String -> Maybe a -> a
unsafeUnjust msg Nothing = error ("unsafeUnjust: " ++ msg)
unsafeUnjust _ (Just x) = x

maybeToMPlus :: MonadPlus m => Maybe a -> m a
maybeToMPlus Nothing = mzero
maybeToMPlus (Just x) = pure x

-- | Similar to Map's @unionWith@
unionMaybeWith :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
unionMaybeWith _ Nothing x = x
unionMaybeWith _ x Nothing = x
unionMaybeWith f (Just a) (Just b) = Just (f a b)
