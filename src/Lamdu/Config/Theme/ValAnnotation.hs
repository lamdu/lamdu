{-# LANGUAGE TemplateHaskell #-}
-- | The themes/ config format
module Lamdu.Config.Theme.ValAnnotation where

import qualified Control.Lens as Lens
import           Data.Aeson.TH (deriveJSON)
import qualified Data.Aeson.Types as Aeson
import           Data.Char (toLower)
import           Data.List.Lens (prefixed)
import qualified GUI.Momentu.Draw as Draw

import           Lamdu.Prelude

data ValAnnotation = ValAnnotation
    { _valAnnotationBGColor :: Draw.Color
    , _valAnnotationHoverBGColor :: Draw.Color
    , _valAnnotationSpacing :: Double -- as ratio of line height
    , _valAnnotationWidthExpansionLimit :: Double
    , _valAnnotationShrinkAtLeast :: Double
    , _valAnnotationMaxHeight :: Double
    } deriving (Eq, Show, Generic)
deriveJSON Aeson.defaultOptions
    { Aeson.fieldLabelModifier
        = (Lens.taking 2 traverse %~ toLower)
        . (^?! prefixed "_valAnnotation")
    }
    ''ValAnnotation

Lens.makeLenses ''ValAnnotation
