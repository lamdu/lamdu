{-# LANGUAGE TemplateHaskell #-}
-- | The themes/ config format
module Lamdu.Config.Theme.Name where

import qualified Control.Lens as Lens
import           Data.Aeson.TH (deriveJSON)
import qualified Data.Aeson.Types as Aeson
import           Data.List.Lens (prefixed)
import           Data.Vector.Vector2 (Vector2)
import qualified GUI.Momentu.Draw as Draw

import           Lamdu.Prelude

data Name = Name
    { _tagCollisionSuffixBGColor :: Draw.Color
    , _textCollisionSuffixBGColor :: Draw.Color
    , _collisionSuffixScaleFactor :: Vector2 Double
    } deriving (Eq, Show)
deriveJSON Aeson.defaultOptions
    {Aeson.fieldLabelModifier = (^?! prefixed "_")}
    ''Name

Lens.makeLenses ''Name
