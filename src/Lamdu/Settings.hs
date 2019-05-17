{-# LANGUAGE TemplateHaskell #-}
module Lamdu.Settings
    ( Settings(..), sAnnotationMode, sSelectedTheme, sSelectedLanguage, sHelpShown
    ) where

import qualified Control.Lens as Lens
import qualified Data.Aeson.TH.Extended as JsonTH
import           GUI.Momentu.Widgets.EventMapHelp (IsHelpShown(..))
import qualified Lamdu.Annotations as Annotations
import           Lamdu.Config.Folder (Selection)
import qualified Lamdu.Config.Folder as Folder

import           Lamdu.Prelude

data Settings = Settings
    { _sAnnotationMode :: Annotations.Mode
    , _sSelectedTheme :: Selection Folder.Theme
    , _sSelectedLanguage :: Selection Folder.Language
    , _sHelpShown :: IsHelpShown
    }
    deriving stock (Eq, Ord, Generic)
JsonTH.derivePrefixed "_s" ''Settings
Lens.makeLenses ''Settings
