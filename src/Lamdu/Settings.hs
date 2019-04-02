{-# LANGUAGE TemplateHaskell #-}
module Lamdu.Settings
    ( Settings(..), sAnnotationMode, sSelectedTheme, sHelpShown
    , HasSettings(..)
    ) where

import qualified Control.Lens as Lens
import           GUI.Momentu.Widgets.EventMapHelp (IsHelpShown(..))
import qualified Lamdu.Annotations as Annotations
import qualified Lamdu.Themes as Themes

import           Lamdu.Prelude

data Settings = Settings
    { _sAnnotationMode :: Annotations.Mode
    , _sSelectedTheme :: Themes.Selection
    , _sHelpShown :: IsHelpShown
    }
Lens.makeLenses ''Settings

class HasSettings env where settings :: Lens' env Settings
