{-# LANGUAGE FlexibleContexts #-}
module Lamdu.GUI.Main
    ( make
    , CodeEdit.ExportRepl(..)
    , CodeEdit.ExportActions(..)
    , CodeEdit.HasEvalResults(..)
    , CodeEdit.HasExportActions(..)
    , EvalResults
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.Reader (ReaderT(..))
import qualified Control.Monad.Reader as Reader
import           Data.CurAndPrev (CurAndPrev)
import           Data.Property (Property)
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.Glue ((/-/))
import qualified GUI.Momentu.Hover as Hover
import qualified GUI.Momentu.Main as MainLoop
import qualified GUI.Momentu.Scroll as Scroll
import qualified GUI.Momentu.State as GuiState
import           GUI.Momentu.Widget (Widget)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Spacer as Spacer
import qualified Lamdu.Config as Config
import qualified Lamdu.Config.Theme as Theme
import           Lamdu.Data.Db.Layout (DbM, ViewM)
import qualified Lamdu.Data.Db.Layout as DbLayout
import qualified Lamdu.Eval.Results as Results
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.GUI.CodeEdit as CodeEdit
import           Lamdu.GUI.IOTrans (IOTrans(..), ioTrans)
import qualified Lamdu.GUI.IOTrans as IOTrans
import qualified Lamdu.GUI.Settings as SettingsWidget
import qualified Lamdu.GUI.StatusBar as StatusBar
import qualified Lamdu.GUI.VersionControl as VersionControlGUI
import qualified Lamdu.GUI.VersionControl.Config as VCConfig
import           Lamdu.Settings (Settings)
import qualified Lamdu.Settings as Settings
import qualified Lamdu.Style as Style
import qualified Lamdu.Themes as Themes
import qualified Lamdu.VersionControl as VersionControl
import qualified Lamdu.VersionControl.Actions as VCActions
import           Revision.Deltum.Transaction (Transaction)

import           Lamdu.Prelude

type T = Transaction

type EvalResults = CurAndPrev (Results.EvalResults (ExprIRef.ValI ViewM))

layout ::
    ( MainLoop.HasMainLoopEnv env
    , Style.HasStyle env
    , Hover.HasStyle env
    , Settings.HasSettings env
    , Spacer.HasStdSpacing env
    , GuiState.HasState env
    , Theme.HasTheme env
    , Config.HasConfig env
    , Element.HasAnimIdPrefix env
    , CodeEdit.HasEvalResults env ViewM
    , CodeEdit.HasExportActions env ViewM
    , VCConfig.HasConfig env, VCConfig.HasTheme env
    ) =>
    [Themes.Selection] -> Property IO Settings -> VCActions.Actions DbM (IOTrans DbM) ->
    ReaderT env (T DbM) (Widget (IOTrans DbM GuiState.Update))
layout themeNames settingsProp vcActions =
    do
        theTheme <- Lens.view Theme.theme
        fullSize <- Lens.view (MainLoop.mainLoopEnv . MainLoop.eWindowSize)
        statusBar <- StatusBar.make themeNames settingsProp (fullSize ^. _1) vcActions
        state <- Lens.view GuiState.state
        codeEdit <-
            CodeEdit.make DbLayout.codeAnchors DbLayout.guiAnchors (fullSize ^. _1)
            & Reader.mapReaderT VersionControl.runAction
            <&> Lens.mapped . ioTrans . Lens._Wrapped . Lens.mapped %~ VersionControl.runEvent state
        topPadding <- Spacer.vspaceLines (theTheme ^. Theme.topPadding)
        statusBar /-/ topPadding /-/ codeEdit
            & Scroll.focusAreaInto fullSize & pure

make ::
    ( MainLoop.HasMainLoopEnv env
    , Style.HasStyle env
    , Hover.HasStyle env
    , Element.HasAnimIdPrefix env
    , Config.HasConfig env
    , Spacer.HasStdSpacing env
    , Theme.HasTheme env
    , Settings.HasSettings env
    , GuiState.HasState env
    , CodeEdit.HasEvalResults env ViewM
    , CodeEdit.HasExportActions env ViewM
    , VCConfig.HasConfig env, VCConfig.HasTheme env
    ) =>
    [Themes.Selection] -> Property IO Settings -> env ->
    T DbM (Widget (IOTrans DbM GuiState.Update))
make themeNames settingsProp env =
    do
        vcActions <-
            VersionControl.makeActions
            <&> VCActions.hoist IOTrans.liftTrans
        let vcEventMap = VersionControlGUI.eventMap versionControlCfg vcActions
        layout themeNames settingsProp vcActions
            <&> Widget.weakerEventsWithoutPreevents
                (settingsEventMap <> quitEventMap <> vcEventMap)
            & (`runReaderT` env)
    where
        settingsEventMap =
            SettingsWidget.eventMap themeNames settingsProp config
            <&> IOTrans.liftIO
        config = env ^. Config.config
        versionControlCfg = config ^. Config.versionControl
        quitEventMap =
            E.keysEventMap (config ^. Config.quitKeys) (E.Doc ["Quit"]) (error "Quit")
