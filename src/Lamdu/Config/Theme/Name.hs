{-# LANGUAGE TemplateHaskell #-}
-- | The themes/ config format
module Lamdu.Config.Theme.Name where

import qualified Control.Lens as Lens
import qualified Data.Aeson.TH.Extended as JsonTH
import           Data.Vector.Vector2 (Vector2)
import qualified GUI.Momentu.Draw as Draw

import           Lamdu.Prelude

data Name = Name
    { _tagCollisionSuffixBGColor :: Draw.Color
    , _textCollisionSuffixBGColor :: Draw.Color
    , _collisionSuffixScaleFactor :: Vector2 Double
    } deriving (Eq, Show, Generic)
JsonTH.derivePrefixed "_" ''Name

Lens.makeLenses ''Name
