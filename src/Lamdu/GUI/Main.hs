{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, FlexibleContexts #-}
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
import           Control.Monad.Transaction (MonadTransaction(..))
import           Data.CurAndPrev (CurAndPrev)
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.Draw as Draw
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.Glue ((/-/), (/|/))
import qualified GUI.Momentu.Hover as Hover
import qualified GUI.Momentu.Main as MainLoop
import qualified GUI.Momentu.Scroll as Scroll
import qualified GUI.Momentu.State as GuiState
import           GUI.Momentu.Widget (Widget)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Spacer as Spacer
import qualified GUI.Momentu.Widgets.TextEdit as TextEdit
import qualified GUI.Momentu.Widgets.TextView as TextView
import qualified Lamdu.Config as Config
import qualified Lamdu.Config.Theme as Theme
import           Lamdu.Data.Db.Layout (DbM, ViewM)
import qualified Lamdu.Data.Db.Layout as DbLayout
import qualified Lamdu.Eval.Results as Results
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.GUI.CodeEdit as CodeEdit
import qualified Lamdu.GUI.CodeEdit.Settings as Settings
import           Lamdu.GUI.IOTrans (IOTrans, ioTrans)
import qualified Lamdu.GUI.IOTrans as IOTrans
import qualified Lamdu.GUI.VersionControl as VersionControlGUI
import qualified Lamdu.GUI.VersionControl.Config as VCConfig
import qualified Lamdu.Style as Style
import qualified Lamdu.VersionControl as VersionControl
import qualified Lamdu.VersionControl.Actions as VCActions
import           Revision.Deltum.Transaction (Transaction)

import           Lamdu.Prelude

type T = Transaction

type EvalResults = CurAndPrev (Results.EvalResults (ExprIRef.ValI ViewM))

makeStatusBar ::
    ( MonadReader env m, MonadTransaction DbM m
    , TextEdit.HasStyle env, Theme.HasTheme env, Hover.HasStyle env
    , GuiState.HasCursor env, Element.HasAnimIdPrefix env
    , VCConfig.HasConfig env, VCConfig.HasTheme env
    ) =>
    Widget.R -> VCActions.Actions DbM (IOTrans DbM) ->
    m (Widget (IOTrans DbM GuiState.Update))
makeStatusBar width vcActions =
    do
        theTheme <- Lens.view Theme.theme
        branchChoice <-
            VersionControlGUI.makeBranchSelector
            IOTrans.liftTrans transaction vcActions
        branchLabel <- TextView.make ?? "Branch: " ?? ["BranchHeader"]
        let rawStatusBar =
                (branchLabel /|/ branchChoice) ^. Align.tValue
                & Element.width .~ width
        Draw.backgroundColor
            ?? Theme.statusBarBGColor theTheme
            ?? rawStatusBar

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
    VCActions.Actions DbM (IOTrans DbM) ->
    ReaderT env (T DbM) (Widget (IOTrans DbM GuiState.Update))
layout vcActions =
    do
        theTheme <- Lens.view Theme.theme
        fullSize <- Lens.view (MainLoop.mainLoopEnv . MainLoop.eWindowSize)
        statusBar <- makeStatusBar (fullSize ^. _1) vcActions
        state <- Lens.view GuiState.state
        codeEdit <-
            CodeEdit.make DbLayout.codeAnchors (fullSize ^. _1)
            & Reader.mapReaderT VersionControl.runAction
            <&> Lens.mapped . ioTrans . Lens.mapped %~ VersionControl.runEvent state
        topPadding <- Spacer.vspaceLines (Theme.topPadding theTheme)
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
    env -> T DbM (Widget (IOTrans DbM GuiState.Update))
make env =
    do
        vcActions <-
            VersionControl.makeActions
            <&> VCActions.hoist IOTrans.liftTrans
        let vcEventMap = VersionControlGUI.eventMap versionControlCfg vcActions
        layout vcActions
            <&> Widget.weakerEventsWithoutPreevents (quitEventMap <> vcEventMap)
            & (`runReaderT` env)
    where
        versionControlCfg = Config.versionControl (env ^. Config.config)
        quitEventMap =
            E.keysEventMap (Config.quitKeys (env ^. Config.config))
            (E.Doc ["Quit"]) (error "Quit")
