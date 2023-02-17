-- | The Sprites component of the Lamdu Theme
{-# LANGUAGE TemplateHaskell, DerivingVia #-}
module Lamdu.Config.Theme.Sprites where

import qualified Control.Lens as Lens
import qualified Data.Aeson.TH.Extended as JsonTH
import           GUI.Momentu.Element.Id (ElemIds)

import           Lamdu.Prelude

data Sprites a = Sprites
    { _earthGlobe :: a
    , _pencilLine :: a
    , _theme :: a
    , _sugar :: a
    , _help :: a
    }
    deriving stock (Eq, Generic, Generic1, Show, Functor, Foldable, Traversable)
    deriving Applicative via (Generically1 Sprites)
    deriving anyclass (ElemIds)
JsonTH.derivePrefixed "_" ''Sprites
Lens.makeLenses ''Sprites
