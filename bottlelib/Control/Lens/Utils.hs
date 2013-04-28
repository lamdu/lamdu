{-# LANGUAGE RankNTypes, FlexibleContexts #-}
module Control.Lens.Utils
  ( _fromJust, allElementsOf
  ) where

import Control.Applicative (Applicative)
import Control.Lens (LensLike, IndexedLensLike)
import Control.Lens.Internal.Indexed (Indexing(..))
import Data.Maybe (fromMaybe)
import qualified Control.Lens as Lens

-- TODO: Into lens...
allElementsOf ::
  Applicative f =>
  LensLike (Indexing f) s t a b -> IndexedLensLike Int f s t a b
allElementsOf l iafb s = snd $ runIndexing (l (\a -> Indexing (\i -> i `seq` (i + 1, Lens.indexed iafb i a))) s) 0

_fromJust :: String -> Lens.Iso (Maybe a) (Maybe b) a b
_fromJust msg = Lens.iso (fromMaybe (error msg)) Just
{-# INLINE _fromJust #-}
