{-# LANGUAGE NoImplicitPrelude, RankNTypes, DisambiguateRecordFields, NamedFieldPuns, OverloadedStrings, FlexibleContexts #-}
module Lamdu.GUI.Main
    ( make
    , defaultCursor
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
import           Data.Store.Transaction (Transaction)
import           Data.Vector.Vector2 (Vector2(..))
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.EventMap as EventMap
import           GUI.Momentu.Glue ((/-/))
import qualified GUI.Momentu.Main as MainLoop
import qualified GUI.Momentu.Scroll as Scroll
import           GUI.Momentu.Widget (Widget)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Spacer as Spacer
import qualified Lamdu.Config as Config
import qualified Lamdu.Config.Theme as Theme
import           Lamdu.Data.DbLayout (DbM, ViewM)
import qualified Lamdu.Data.DbLayout as DbLayout
import qualified Lamdu.Eval.Results as Results
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.GUI.CodeEdit as CodeEdit
import qualified Lamdu.GUI.CodeEdit.Settings as Settings
import           Lamdu.GUI.IOTrans (IOTrans, ioTrans)
import qualified Lamdu.GUI.IOTrans as IOTrans
import qualified Lamdu.GUI.VersionControl as VersionControlGUI
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.Style as Style
import qualified Lamdu.VersionControl as VersionControl
import qualified Lamdu.VersionControl.Actions as VersionControl.Actions

import           Lamdu.Prelude

type T = Transaction

defaultCursor :: Widget.Id
defaultCursor = WidgetIds.replId

type EvalResults = CurAndPrev (Results.EvalResults (ExprIRef.ValI ViewM))

makeInnerGui ::
    ( MainLoop.HasMainLoopEnv env
    , Style.HasStyle env
    , Settings.HasSettings env
    , Spacer.HasStdSpacing env
    , Widget.HasCursor env
    , Theme.HasTheme env
    , Config.HasConfig env
    , CodeEdit.HasEvalResults env ViewM
    , CodeEdit.HasExportActions env ViewM
    ) =>
    Widget (IOTrans DbM Widget.EventResult) ->
    ReaderT env (T DbM) (Widget (IOTrans DbM Widget.EventResult))
makeInnerGui branchSelector =
    do
        fullSize <- Lens.view (MainLoop.mainLoopEnv . MainLoop.eWindowSize)
        let codeSize = fullSize - Vector2 0 (branchSelector ^. Element.height)
        theCursor <- Lens.view Widget.cursor
        codeEdit <-
            CodeEdit.make DbLayout.codeAnchors (codeSize ^. _1)
            & Reader.mapReaderT VersionControl.runAction
            <&> Widget.events . ioTrans . Lens.mapped %~
                VersionControl.runEvent theCursor
        theTheme <- Lens.view Theme.theme
        topPadding <- Spacer.vspaceLines (Theme.topPadding theTheme)
        let scrollBox =
                topPadding /-/ codeEdit
                & Scroll.focusAreaInto codeSize
                & Element.size .~ codeSize
        scrollBox /-/ Element.hoverLayers branchSelector
            & return

make ::
    ( MainLoop.HasMainLoopEnv env
    , Style.HasStyle env
    , Config.HasConfig env
    , Spacer.HasStdSpacing env
    , Theme.HasTheme env
    , Settings.HasSettings env
    , CodeEdit.HasEvalResults env ViewM
    , CodeEdit.HasExportActions env ViewM
    ) =>
    env -> T DbM (Widget (IOTrans DbM Widget.EventResult))
make env =
    do
        actions <-
            VersionControl.makeActions
            <&> VersionControl.Actions.hoist IOTrans.liftTrans
        let versionControlCfg = Config.versionControl (env ^. Config.config)
        let versionControlThm = Theme.versionControl (env ^. Theme.theme)
        do
            branchGui <-
                VersionControlGUI.make versionControlCfg versionControlThm
                IOTrans.liftTrans lift actions makeInnerGui
                & (`runReaderT` env)
            let quitEventMap =
                    Widget.keysEventMap (Config.quitKeys (env ^. Config.config))
                    (EventMap.Doc ["Quit"]) (error "Quit")
            EventMap.strongerEvents quitEventMap branchGui & return
