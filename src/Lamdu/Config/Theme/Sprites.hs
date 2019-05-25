-- | The Sprites component of the Lamdu Theme
{-# LANGUAGE TemplateHaskell #-}
module Lamdu.Config.Theme.Sprites
    ( Sprites(..)
    , earthGlobe
    ) where

import qualified Control.Lens as Lens
import qualified Data.Aeson.TH.Extended as JsonTH

import           Lamdu.Prelude

newtype Sprites a = Sprites
    { _earthGlobe :: a
    } deriving (Eq, Generic, Show, Functor, Foldable, Traversable)
JsonTH.derivePrefixed "_" ''Sprites
Lens.makeLenses ''Sprites
