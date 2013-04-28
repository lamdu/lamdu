module Control.Lens.Utils
  ( _fromJust
  ) where

import Data.Maybe (fromMaybe)
import qualified Control.Lens as Lens

_fromJust :: String -> Lens.Iso (Maybe a) (Maybe b) a b
_fromJust msg = Lens.iso (fromMaybe (error msg)) Just
{-# INLINE _fromJust #-}
