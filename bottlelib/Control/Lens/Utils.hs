{-# LANGUAGE NoImplicitPrelude, RankNTypes #-}
module Control.Lens.Utils
    ( _fromJust
    , getPrism
    , tagged
    ) where

import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Data.Maybe.Utils (unsafeUnjust)

import           Prelude.Compat

_fromJust :: String -> Lens.Iso (Maybe a) (Maybe b) a b
_fromJust msg = Lens.iso (unsafeUnjust msg) Just
{-# INLINE _fromJust #-}

getPrism :: Lens.Prism s t a b -> s -> Either a t
getPrism p = p Left

{-# INLINE tagged #-}
tagged :: Lens.Prism' tag () -> Lens.Prism' (a, tag) a
tagged prism =
    Lens.prism (flip (,) (prism # ()))
    ( \(a, tag1) ->
      case Lens.matching prism tag1 of
      Left tag2 -> Left (a, tag2)
      Right () -> Right a
    )
