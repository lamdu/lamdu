{-# LANGUAGE TemplateHaskell #-}
module Lamdu.Settings
    ( Settings(..), sAnnotationMode, sSelectedTheme, sSelectedLanguage, sHelpShown
    , HasSettings(..)
    ) where

import qualified Control.Lens as Lens
import           GUI.Momentu.Widgets.EventMapHelp (IsHelpShown(..))
import qualified Lamdu.Annotations as Annotations
import           Lamdu.Config.Folder (Selection)
import           Lamdu.Config.Theme (Theme)
import           Lamdu.I18N.Texts (Language)

import           Lamdu.Prelude

data Settings = Settings
    { _sAnnotationMode :: Annotations.Mode
    , _sSelectedTheme :: Selection Theme
    , _sSelectedLanguage :: Selection Language
    , _sHelpShown :: IsHelpShown
    }
Lens.makeLenses ''Settings

class HasSettings env where settings :: Lens' env Settings
