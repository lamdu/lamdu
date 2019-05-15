{-# LANGUAGE TemplateHaskell #-}
module Lamdu.Settings
    ( Settings(..), sAnnotationMode, sSelectedTheme, sSelectedLanguage, sHelpShown
    ) where

import qualified Control.Lens as Lens
import qualified Data.Aeson.TH.Extended as JsonTH
import           GUI.Momentu.Widgets.EventMapHelp (IsHelpShown(..))
import qualified Lamdu.Annotations as Annotations
import           Lamdu.Config.Folder (Selection)
import           Lamdu.Config.Theme (Theme)
import           Lamdu.I18N.Language (Language)

import           Lamdu.Prelude

data Settings = Settings
    { _sAnnotationMode :: Annotations.Mode
    , _sSelectedTheme :: Selection Theme
    , _sSelectedLanguage :: Selection Language
    , _sHelpShown :: IsHelpShown
    }
    deriving stock (Eq, Ord, Generic)
JsonTH.derivePrefixed "_s" ''Settings
Lens.makeLenses ''Settings
