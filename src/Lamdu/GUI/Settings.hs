-- | Widget to edit the settings
module Lamdu.GUI.Settings
     ( forStatusBar
     , eventMap
     ) where

import           Data.Property (Property, composeLens)
import           GUI.Momentu.Align (WithTextPos(..))
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.EventMap (EventMap)
import           GUI.Momentu.Glue ((/|/))
import qualified GUI.Momentu.Hover as Hover
import qualified GUI.Momentu.State as GuiState
import           GUI.Momentu.Widget (Widget)
import qualified GUI.Momentu.Widgets.Spacer as Spacer
import qualified Lamdu.Config as Config
import qualified Lamdu.Config.Theme as Theme
import qualified Lamdu.GUI.CodeEdit.AnnotationMode.Widget as AnnotationModeWidget
import qualified Lamdu.GUI.StatusBar.Common as StatusBar
import qualified Lamdu.GUI.Themes as Themes
import           Lamdu.Settings (Settings)
import qualified Lamdu.Settings as Settings

import           Lamdu.Prelude

-- | For the status bar
forStatusBar ::
    ( MonadReader env m, Element.HasAnimIdPrefix env
    , Applicative f, GuiState.HasCursor env, Hover.HasStyle env
    , Theme.HasTheme env, Spacer.HasStdSpacing env
    ) =>
    [Themes.Selection] -> Property f Settings -> m (WithTextPos (Widget (f GuiState.Update)))
forStatusBar themeNames prop =
    do
        hspace <- StatusBar.hspacer
        annotationMode <- AnnotationModeWidget.forStatusBar annotationModeProp
        theme <- Themes.widgetForStatusBar themeNames themeProp
        annotationMode /|/ hspace /|/ theme & pure
    where
        themeProp = composeLens Settings.sSelectedTheme prop
        annotationModeProp = composeLens Settings.sAnnotationMode prop

eventMap ::
    (MonadReader env m, Config.HasConfig env) =>
    [Themes.Selection] -> Property IO Settings ->
    m (EventMap (IO GuiState.Update))
eventMap themeNames prop =
    do
        themeSwitch <- Themes.switchEventMap themeNames themeProp
        switchAnnotationMode <- AnnotationModeWidget.switchEventMap annotationModeProp
        themeSwitch <> switchAnnotationMode & pure
    where
        themeProp = composeLens Settings.sSelectedTheme prop
        annotationModeProp = composeLens Settings.sAnnotationMode prop
