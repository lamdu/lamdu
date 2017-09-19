{-# LANGUAGE NoImplicitPrelude, DeriveGeneric #-}
-- | The themes/ config format
module Lamdu.Config.Theme.CodeForegroundColors
    ( CodeForegroundColors(..)
    ) where

import qualified Data.Aeson.Types as Aeson
import           GHC.Generics (Generic)
import qualified GUI.Momentu.Draw as Draw

import           Lamdu.Prelude

data CodeForegroundColors = CodeForegroundColors
    { baseColor :: Draw.Color
    , nomColor :: Draw.Color
    , literalColor :: Draw.Color
    , grammarColor :: Draw.Color
    , caseTailColor :: Draw.Color
    , recordTailColor :: Draw.Color
    , lightLambdaUnderlineColor :: Draw.Color
    , foreignModuleColor :: Draw.Color
    , foreignVarColor :: Draw.Color
    , presentationChoiceColor :: Draw.Color
    } deriving (Eq, Generic, Show)
instance Aeson.ToJSON CodeForegroundColors where
    toJSON = Aeson.genericToJSON Aeson.defaultOptions
instance Aeson.FromJSON CodeForegroundColors
