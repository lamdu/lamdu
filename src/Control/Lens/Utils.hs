{-# LANGUAGE NoImplicitPrelude, RankNTypes #-}
module Control.Lens.Utils
    ( tagged
    ) where

import qualified Control.Lens as Lens

import           Lamdu.Prelude

{-# INLINE tagged #-}
tagged :: Lens.Prism' tag () -> Lens.Prism' (a, tag) a
tagged prism =
    Lens.prism (flip (,) (prism # ()))
    ( \(a, tag1) ->
      case Lens.matching prism tag1 of
      Left tag2 -> Left (a, tag2)
      Right () -> Right a
    )
