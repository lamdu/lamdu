{-# LANGUAGE NoImplicitPrelude, TemplateHaskell, OverloadedStrings #-}
module Lamdu.GUI.CodeEdit.Settings
    ( AnnotationMode(..)
    , Settings(..), sAnnotationMode, sSelectedTheme
    , HasSettings(..)
    , statusWidget
    , eventMap
    , initial
    ) where

import qualified Control.Lens as Lens
import           Data.Property (Property)
import qualified Data.Property as Property
import           GUI.Momentu.Align (WithTextPos(..))
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.State as GuiState
import           GUI.Momentu.Widget (Widget)
import qualified GUI.Momentu.Widgets.TextView as TextView
import qualified Lamdu.Config as Config
import qualified Lamdu.Config.Sampler as ConfigSampler
import           Lamdu.GUI.CodeEdit.AnnotationMode (AnnotationMode)
import qualified Lamdu.GUI.CodeEdit.AnnotationMode as AnnotationMode
import qualified Lamdu.GUI.CodeEdit.AnnotationMode.Widget as AnnotationModeWidget
import qualified Lamdu.Themes as Themes

import           Lamdu.Prelude

data Settings = Settings
    { _sAnnotationMode :: AnnotationMode
    , _sSelectedTheme :: Text
    }
Lens.makeLenses ''Settings

initial :: Settings
initial =
    Settings
    { _sAnnotationMode = AnnotationMode.initial
    , _sSelectedTheme = Themes.initial
    }

class HasSettings env where settings :: Lens' env Settings

-- | For the status bar
statusWidget ::
    (MonadReader env m, TextView.HasStyle env, Element.HasAnimIdPrefix env) =>
    Property f Settings -> m (WithTextPos (Widget (f a)))
statusWidget prop =
    AnnotationModeWidget.forStatusBar annotationModeProp
    where
        annotationModeProp = Property.composeLens sAnnotationMode prop

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
        themeProp = Property.composeLens sSelectedTheme settingsProp
        annotationModeProp = Property.composeLens sAnnotationMode settingsProp
