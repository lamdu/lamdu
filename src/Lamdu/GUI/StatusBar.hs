-- | The Lamdu status bar
{-# LANGUAGE FlexibleContexts #-}
module Lamdu.GUI.StatusBar
    ( module Lamdu.GUI.StatusBar.Common
    , make
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.Transaction (MonadTransaction(..))
import           Data.Property (Property)
import           Data.Vector.Vector2 (Vector2(..))
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.Draw as Draw
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.Hover as Hover
import qualified GUI.Momentu.State as GuiState
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Spacer as Spacer
import qualified GUI.Momentu.Widgets.TextEdit as TextEdit
import           Lamdu.Config (HasConfig)
import           Lamdu.Config.Folder (Selection)
import           Lamdu.Config.Theme (Theme)
import qualified Lamdu.Config.Theme as Theme
import           Lamdu.GUI.IOTrans (IOTrans(..))
import qualified Lamdu.GUI.IOTrans as IOTrans
import qualified Lamdu.GUI.Settings as SettingsGui
import           Lamdu.GUI.StatusBar.Common
import qualified Lamdu.GUI.StatusBar.Common as StatusBar
import qualified Lamdu.GUI.VersionControl as VersionControlGUI
import qualified Lamdu.GUI.VersionControl.Config as VCConfig
import           Lamdu.I18N.Texts (Language, HasTexts)
import qualified Lamdu.I18N.Texts as Texts
import           Lamdu.Settings (Settings)
import qualified Lamdu.VersionControl.Actions as VCActions

import           Lamdu.Prelude

make ::
    ( MonadReader env m, MonadTransaction n m
    , TextEdit.HasStyle env, Theme.HasTheme env, Hover.HasStyle env
    , GuiState.HasState env, Element.HasAnimIdPrefix env
    , VCConfig.HasConfig env, VCConfig.HasTheme env, Spacer.HasStdSpacing env
    , HasConfig env, Element.HasLayoutDir env, HasTexts env
    ) =>
    StatusWidget (IOTrans n) ->
    [Selection Theme] -> [Selection Language] -> Property IO Settings ->
    Widget.R -> VCActions.Actions n (IOTrans n) ->
    m (StatusWidget (IOTrans n))
make gotoDefinition themeNames langNames settingsProp width vcActions =
    do
        branchChoice <-
            VersionControlGUI.makeBranchSelector
            IOTrans.liftTrans transaction vcActions
        branchSelector <- StatusBar.makeStatusWidget (Texts.statusBar . Texts.branch) branchChoice

        statusWidgets <-
            SettingsGui.makeStatusWidgets themeNames langNames settingsProp
            <&> SettingsGui.hoist IOTrans.liftIO

        theTheme <- Lens.view Theme.theme
        bgColor <-
            Draw.backgroundColor ?? theTheme ^. Theme.statusBar . Theme.statusBarBGColor
        padToSize <- Element.padToSize
        (StatusBar.combineEdges ?? width ?? gotoDefinition)
            <*> ( StatusBar.combine ??
                    [ statusWidgets ^. SettingsGui.annotationWidget
                    , statusWidgets ^. SettingsGui.themeWidget
                    , branchSelector
                    , statusWidgets ^. SettingsGui.languageWidget
                    , statusWidgets ^. SettingsGui.helpWidget
                    ]
                )
            <&> StatusBar.widget . Align.tValue %~ padToSize (Vector2 width 0) 0
            <&> StatusBar.widget %~ bgColor
