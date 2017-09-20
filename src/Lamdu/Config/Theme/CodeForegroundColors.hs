{-# LANGUAGE NoImplicitPrelude, TemplateHaskell #-}
-- | The themes/ config format
module Lamdu.Config.Theme.CodeForegroundColors
    ( CodeForegroundColors(..)
    ) where

import           Data.Aeson.Types (defaultOptions)
import           Data.Aeson.TH (deriveJSON)
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
    } deriving (Eq, Show)
deriveJSON defaultOptions ''CodeForegroundColors
