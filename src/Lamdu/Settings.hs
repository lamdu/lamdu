{-# LANGUAGE TemplateHaskell #-}
module Lamdu.Settings
    ( Settings(..), sAnnotationMode, sSelectedTheme, sHelpShown
    , HasSettings(..)
    , initial
    ) where

import qualified Control.Lens as Lens
import           GUI.Momentu.Widgets.EventMapHelp (IsHelpShown(..))
import           Lamdu.Sugar.Convert.Input (AnnotationMode)
import qualified Lamdu.Sugar.Convert.Input as AnnotationMode
import qualified Lamdu.Themes as Themes

import           Lamdu.Prelude

data Settings = Settings
    { _sAnnotationMode :: AnnotationMode
    , _sSelectedTheme :: Themes.Selection
    , _sHelpShown :: IsHelpShown
    }
Lens.makeLenses ''Settings

initial :: Settings
initial =
    Settings
    { _sAnnotationMode = AnnotationMode.Evaluation
    , _sSelectedTheme = Themes.initial
    , _sHelpShown = HelpNotShown
    }

class HasSettings env where settings :: Lens' env Settings
