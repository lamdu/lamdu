module Control.Lens.Utils
  ( _fromJust
  ) where

import Data.Maybe (fromMaybe)
import qualified Control.Lens as Lens

_fromJust :: (Lens.Profunctor p, Functor f) => String -> Lens.Overloaded p f (Maybe a) (Maybe b) a b
_fromJust msg = Lens.iso (fromMaybe (error msg)) Just
{-# INLINE _fromJust #-}
