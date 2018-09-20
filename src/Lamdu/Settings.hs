{-# LANGUAGE TemplateHaskell #-}
module Lamdu.Settings
    ( Settings(..), sAnnotationMode, sSelectedTheme, sHelpShown
    , HasSettings(..)
    ) where

import qualified Control.Lens as Lens
import           GUI.Momentu.Widgets.EventMapHelp (IsHelpShown(..))
import           Lamdu.Sugar.Convert.Input (AnnotationMode)
import qualified Lamdu.Themes as Themes

import           Lamdu.Prelude

data Settings = Settings
    { _sAnnotationMode :: AnnotationMode
    , _sSelectedTheme :: Themes.Selection
    , _sHelpShown :: IsHelpShown
    }
Lens.makeLenses ''Settings

class HasSettings env where settings :: Lens' env Settings
