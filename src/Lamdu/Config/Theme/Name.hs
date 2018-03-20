{-# LANGUAGE TemplateHaskell, CPP #-}
-- | The themes/ config format
module Lamdu.Config.Theme.Name where

import qualified Control.Lens as Lens
import           Data.Aeson.TH (deriveJSON)
import           Data.Aeson.Types (defaultOptions)
import qualified Data.Aeson.Types as Aeson
import           Data.Aeson.Utils (removePrefix)
import           Data.Vector.Vector2 (Vector2)
import qualified GUI.Momentu.Draw as Draw

import           Lamdu.Prelude

data Name = Name
    { _tagCollisionSuffixBGColor :: Draw.Color
    , _textCollisionSuffixBGColor :: Draw.Color
    , _collisionSuffixScaleFactor :: Vector2 Double
    } deriving (Eq, Show)
deriveJSON defaultOptions
#ifndef NO_CODE
    {Aeson.fieldLabelModifier = removePrefix "_"}
#endif
    ''Name

Lens.makeLenses ''Name
