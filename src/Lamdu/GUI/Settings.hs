-- | Widget to edit the settings
{-# LANGUAGE NoImplicitPrelude #-}
module Lamdu.GUI.Settings
     ( forStatusBar
     , eventMap
     ) where

import           Data.Property (Property)
import qualified Data.Property as Property
import           GUI.Momentu.Align (WithTextPos(..))
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.Hover as Hover
import qualified GUI.Momentu.State as GuiState
import           GUI.Momentu.Widget (Widget)
import qualified GUI.Momentu.Widgets.TextView as TextView
import qualified Lamdu.Config as Config
import qualified Lamdu.Config.Sampler as ConfigSampler
import qualified Lamdu.Config.Theme as Theme
import qualified Lamdu.GUI.CodeEdit.AnnotationMode.Widget as AnnotationModeWidget
import           Lamdu.Settings (Settings)
import qualified Lamdu.Settings as Settings
import qualified Lamdu.Themes as Themes

import           Lamdu.Prelude

-- | For the status bar
forStatusBar ::
    ( MonadReader env m, TextView.HasStyle env, Element.HasAnimIdPrefix env
    , Applicative f, GuiState.HasCursor env, Hover.HasStyle env
    , Theme.HasTheme env
    ) =>
    Property f Settings -> m (WithTextPos (Widget (f GuiState.Update)))
forStatusBar prop =
    AnnotationModeWidget.forStatusBar annotationModeProp
    where
        annotationModeProp = Property.composeLens Settings.sAnnotationMode prop

eventMap ::
    (MonadReader env m, Config.HasConfig env) =>
    ConfigSampler.Sampler -> Property IO Settings ->
    m (EventMap (IO GuiState.Update))
eventMap configSampler settingsProp =
    do
        themeSwitch <- Themes.switchEventMap configSampler themeProp
        switchAnnotationMode <- AnnotationModeWidget.switchEventMap annotationModeProp
        themeSwitch <> switchAnnotationMode & pure
    where
        themeProp = Property.composeLens Settings.sSelectedTheme settingsProp
        annotationModeProp =
            Property.composeLens Settings.sAnnotationMode settingsProp
