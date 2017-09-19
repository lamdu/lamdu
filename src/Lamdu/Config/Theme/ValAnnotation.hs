{-# LANGUAGE NoImplicitPrelude, DeriveGeneric #-}
-- | The themes/ config format
module Lamdu.Config.Theme.ValAnnotation
    ( ValAnnotation(..)
    ) where

import qualified Data.Aeson.Types as Aeson
import           GHC.Generics (Generic)
import qualified GUI.Momentu.Draw as Draw

import           Lamdu.Prelude

data ValAnnotation = ValAnnotation
    { valAnnotationBGColor :: Draw.Color
    , valAnnotationHoverBGColor :: Draw.Color
    , valAnnotationSpacing :: Double -- as ratio of line height
    , valAnnotationWidthExpansionLimit :: Double
    , valAnnotationShrinkAtLeast :: Double
    , valAnnotationMaxHeight :: Double
    } deriving (Eq, Generic, Show)
instance Aeson.ToJSON ValAnnotation where
    toJSON = Aeson.genericToJSON Aeson.defaultOptions
instance Aeson.FromJSON ValAnnotation
