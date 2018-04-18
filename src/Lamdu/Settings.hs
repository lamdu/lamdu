{-# LANGUAGE TemplateHaskell #-}
module Lamdu.Settings
    ( AnnotationMode(..), IsHelpShown(..)
    , Settings(..), sAnnotationMode, sSelectedTheme, sHelpShown
    , HasSettings(..)
    , initial
    ) where

import qualified Control.Lens as Lens
import           GUI.Momentu.Widgets.EventMapHelp (IsHelpShown(..))
import           Lamdu.GUI.CodeEdit.AnnotationMode (AnnotationMode)
import qualified Lamdu.GUI.CodeEdit.AnnotationMode as AnnotationMode
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
    { _sAnnotationMode = AnnotationMode.initial
    , _sSelectedTheme = Themes.initial
    , _sHelpShown = HelpNotShown
    }

class HasSettings env where settings :: Lens' env Settings
