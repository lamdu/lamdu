{-# LANGUAGE NoImplicitPrelude, RankNTypes, DisambiguateRecordFields, NamedFieldPuns, OverloadedStrings, FlexibleContexts #-}
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
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.Glue ((/-/))
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

makeInnerGui ::
    ( MainLoop.HasMainLoopEnv env
    , Style.HasStyle env
    , Settings.HasSettings env
    , Spacer.HasStdSpacing env
    , GuiState.HasState env
    , Theme.HasTheme env
    , Config.HasConfig env
    , CodeEdit.HasEvalResults env ViewM
    , CodeEdit.HasExportActions env ViewM
    ) =>
    Widget (IOTrans DbM GuiState.Update) ->
    ReaderT env (T DbM) (Widget (IOTrans DbM GuiState.Update))
makeInnerGui branchSelector =
    do
        fullSize <- Lens.view (MainLoop.mainLoopEnv . MainLoop.eWindowSize)
        let codeSize = fullSize - Vector2 0 (branchSelector ^. Element.height)
        state <- Lens.view GuiState.state
        codeEdit <-
            CodeEdit.make DbLayout.codeAnchors (codeSize ^. _1)
            & Reader.mapReaderT VersionControl.runAction
            <&> Lens.mapped . ioTrans . Lens.mapped %~
                VersionControl.runEvent state
        theTheme <- Lens.view Theme.theme
        topPadding <- Spacer.vspaceLines (Theme.topPadding theTheme)
        let scrollBox =
                topPadding /-/ codeEdit
                & Scroll.focusAreaInto codeSize
                & Element.size .~ codeSize
        scrollBox /-/ Element.hoverLayers branchSelector
            & pure

make ::
    ( MainLoop.HasMainLoopEnv env
    , Style.HasStyle env
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
        VersionControlGUI.make versionControlCfg versionControlThm
            IOTrans.liftTrans lift actions makeInnerGui
            & (`runReaderT` env)
            <&> Widget.eventMapMaker . Lens.mapped %~ (quitEventMap <>)
    where
        versionControlCfg = Config.versionControl (env ^. Config.config)
        versionControlThm = Theme.versionControl (env ^. Theme.theme)
        quitEventMap =
            E.keysEventMap (Config.quitKeys (env ^. Config.config))
            (E.Doc ["Quit"]) (error "Quit")
