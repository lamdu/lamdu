{-# LANGUAGE NoImplicitPrelude, TemplateHaskell #-}
-- | The themes/ config format
module Lamdu.Config.Theme.TextColors
    ( TextColors(..)
    ) where

import           Data.Aeson.TH (deriveJSON)
import           Data.Aeson.Types (defaultOptions)
import qualified GUI.Momentu.Draw as Draw

import           Lamdu.Prelude

data TextColors = TextColors
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
    , actionTextColor :: Draw.Color
    , infoTextColor :: Draw.Color
    , typeTextColor :: Draw.Color
    } deriving (Eq, Show)
deriveJSON defaultOptions ''TextColors
