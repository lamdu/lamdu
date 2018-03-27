{-# LANGUAGE TemplateHaskell, CPP #-}
-- | The themes/ config format
module Lamdu.Config.Theme.ValAnnotation where

#ifndef NO_CODE
import qualified Control.Lens as Lens
#endif
import           Data.Aeson.TH (deriveJSON)
import qualified Data.Aeson.Types as Aeson
#ifndef NO_CODE
import           Data.Aeson.Utils (removePrefix)
import qualified Data.Char as Char
#endif
import qualified GUI.Momentu.Draw as Draw

import           Lamdu.Prelude

data ValAnnotation = ValAnnotation
    { _valAnnotationBGColor :: Draw.Color
    , _valAnnotationHoverBGColor :: Draw.Color
    , _valAnnotationSpacing :: Double -- as ratio of line height
    , _valAnnotationWidthExpansionLimit :: Double
    , _valAnnotationShrinkAtLeast :: Double
    , _valAnnotationMaxHeight :: Double
    } deriving (Eq, Show)
deriveJSON Aeson.defaultOptions
#ifndef NO_CODE
    { Aeson.fieldLabelModifier = \name ->
        name
        & removePrefix "_valAnnotation"
        & Lens.taking 2 traverse %~ Char.toLower
    }
#endif
    ''ValAnnotation

Lens.makeLenses ''ValAnnotation
