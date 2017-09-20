{-# LANGUAGE NoImplicitPrelude, TemplateHaskell #-}
-- | The themes/ config format
module Lamdu.Config.Theme.ValAnnotation
    ( ValAnnotation(..)
    ) where

import qualified Control.Lens as Lens
import           Data.Aeson.TH (deriveJSON)
import           Data.Aeson.Types (defaultOptions, fieldLabelModifier)
import           Data.Aeson.Utils (removePrefix)
import qualified Data.Char as Char
import qualified GUI.Momentu.Draw as Draw

import           Lamdu.Prelude

data ValAnnotation = ValAnnotation
    { valAnnotationBGColor :: Draw.Color
    , valAnnotationHoverBGColor :: Draw.Color
    , valAnnotationSpacing :: Double -- as ratio of line height
    , valAnnotationWidthExpansionLimit :: Double
    , valAnnotationShrinkAtLeast :: Double
    , valAnnotationMaxHeight :: Double
    } deriving (Eq, Show)
deriveJSON defaultOptions
    { fieldLabelModifier = \name ->
        name
        & removePrefix "valAnnotation"
        & Lens.taking 2 traverse %~ Char.toLower
    }
    ''ValAnnotation
