-- | Widget to edit the settings
{-# LANGUAGE TemplateHaskell #-}
module Lamdu.GUI.Settings
     ( StatusWidgets(..), annotationWidget, themeWidget, helpWidget
     , hoist
     , makeStatusWidgets
     ) where

import qualified Control.Lens as Lens
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

data StatusWidgets f = StatusWidgets
    { _annotationWidget :: StatusBar.StatusWidget f
    , _themeWidget :: StatusBar.StatusWidget f
    , _helpWidget :: StatusBar.StatusWidget f
    }
Lens.makeLenses ''StatusWidgets

hoist ::
    (f GuiState.Update -> g GuiState.Update) ->
    StatusWidgets f -> StatusWidgets g
hoist f (StatusWidgets x y z) =
    StatusWidgets (h x) (h y) (h z)
    where
        h = StatusBar.hoist f

makeStatusWidgets ::
    ( MonadReader env m, Applicative f
    , HasConfig env, HasTheme env, HasStdSpacing env
    , Element.HasAnimIdPrefix env, GuiState.HasCursor env
    , Hover.HasStyle env
    ) =>
    [Themes.Selection] -> Property f Settings -> m (StatusWidgets f)
makeStatusWidgets themeNames prop =
    StatusWidgets
    <$> StatusBar.makeBoundedSwitchStatusWidget "Annotations"
        Config.nextAnnotationModeKeys annotationModeProp
    <*> Themes.makeStatusWidget themeNames themeProp
    <*> StatusBar.makeBoundedSwitchStatusWidget "Help" Config.helpKeys helpProp
    where
        themeProp = composeLens Settings.sSelectedTheme prop
        annotationModeProp = composeLens Settings.sAnnotationMode prop
        helpProp = composeLens Settings.sHelpShown prop
