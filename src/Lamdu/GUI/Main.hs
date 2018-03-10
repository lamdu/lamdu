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
import           Data.CurAndPrev (CurAndPrev)
import           Data.Vector.Vector2 (Vector2(..))
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
import qualified Lamdu.Style as Style
import qualified Lamdu.VersionControl as VersionControl
import qualified Lamdu.VersionControl.Actions as VersionControl.Actions
import           Revision.Deltum.Transaction (Transaction)

import           Lamdu.Prelude

type T = Transaction

type EvalResults = CurAndPrev (Results.EvalResults (ExprIRef.ValI ViewM))

layout ::
    ( MainLoop.HasMainLoopEnv env
    , Style.HasStyle env
    , Settings.HasSettings env
    , Spacer.HasStdSpacing env
    , GuiState.HasState env
    , Theme.HasTheme env
    , Config.HasConfig env
    , Element.HasAnimIdPrefix env
    , CodeEdit.HasEvalResults env ViewM
    , CodeEdit.HasExportActions env ViewM
    ) =>
    Widget (IOTrans DbM GuiState.Update) ->
    ReaderT env (T DbM) (Widget (IOTrans DbM GuiState.Update))
layout branchChoice =
    do
        theTheme <- Lens.view Theme.theme
        fullSize <- Lens.view (MainLoop.mainLoopEnv . MainLoop.eWindowSize)
        branchLabel <- TextView.make ?? "Branch: " ?? ["BranchHeader"]
        let rawStatusBar =
                (branchLabel /|/ branchChoice) ^. Align.tValue
                & Element.width .~ fullSize ^. _1
        statusBar <-
            Draw.backgroundColor
            ?? Theme.statusBarBGColor theTheme
            ?? rawStatusBar
        let codeSize = fullSize - Vector2 0 (statusBar ^. Element.height)
        state <- Lens.view GuiState.state
        codeEdit <-
            CodeEdit.make DbLayout.codeAnchors (codeSize ^. _1)
            & Reader.mapReaderT VersionControl.runAction
            <&> Lens.mapped . ioTrans . Lens.mapped %~ VersionControl.runEvent state
        topPadding <- Spacer.vspaceLines (Theme.topPadding theTheme)
        let scrollBox = Scroll.focusAreaInto codeSize codeEdit
        statusBar /-/ topPadding /-/ scrollBox & pure

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
    ) =>
    env -> T DbM (Widget (IOTrans DbM GuiState.Update))
make env =
    do
        actions <-
            VersionControl.makeActions
            <&> VersionControl.Actions.hoist IOTrans.liftTrans
        let vcEventMap = VersionControlGUI.eventMap versionControlCfg actions
        VersionControlGUI.makeBranchSelector versionControlCfg versionControlThm
            IOTrans.liftTrans lift actions
            >>= layout
            <&> Widget.weakerEvents (quitEventMap <> vcEventMap)
            & (`runReaderT` env)
    where
        versionControlCfg = Config.versionControl (env ^. Config.config)
        versionControlThm = Theme.versionControl (env ^. Theme.theme)
        quitEventMap =
            E.keysEventMap (Config.quitKeys (env ^. Config.config))
            (E.Doc ["Quit"]) (error "Quit")
