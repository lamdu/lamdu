-- | Widget to edit the settings
module Lamdu.GUI.Settings
     ( makeStatusWidget
     ) where

import           Data.Property (Property, composeLens)
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.Hover as Hover
import qualified GUI.Momentu.State as GuiState
import           GUI.Momentu.Widgets.Spacer (HasStdSpacing)
import           Lamdu.Config (HasConfig)
import qualified Lamdu.Config as Config
import           Lamdu.Config.Theme (HasTheme)
import qualified Lamdu.GUI.StatusBar.Common as StatusBar
import qualified Lamdu.GUI.Themes as Themes
import           Lamdu.Settings (Settings)
import qualified Lamdu.Settings as Settings

import           Lamdu.Prelude

makeStatusWidget ::
    ( MonadReader env m, Applicative f
    , HasConfig env, HasTheme env, HasStdSpacing env
    , Element.HasAnimIdPrefix env, GuiState.HasCursor env
    , Hover.HasStyle env
    ) =>
    [Themes.Selection] -> Property f Settings -> m (StatusBar.StatusWidget f)
makeStatusWidget themeNames prop =
    StatusBar.combine
    <*>
    sequenceA
    [ StatusBar.makeBoundedSwitchStatusWidget "Annotations"
        Config.nextAnnotationModeKeys annotationModeProp
    , Themes.makeStatusWidget themeNames themeProp
    ]
    where
        themeProp = composeLens Settings.sSelectedTheme prop
        annotationModeProp = composeLens Settings.sAnnotationMode prop
