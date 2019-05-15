{-# LANGUAGE FlexibleContexts, ConstraintKinds #-}
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
import           Data.Has (Has(..))
import           Data.Property (Property)
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.Glue ((/-/))
import qualified GUI.Momentu.Hover as Hover
import qualified GUI.Momentu.Main as MainLoop
import qualified GUI.Momentu.Scroll as Scroll
import           GUI.Momentu.State (Gui)
import qualified GUI.Momentu.State as GuiState
import           GUI.Momentu.Widget (Widget)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Menu as Menu
import qualified GUI.Momentu.Widgets.Menu.Search as SearchMenu
import qualified GUI.Momentu.Widgets.Spacer as Spacer
import qualified Lamdu.Cache as Cache
import qualified Lamdu.Config as Config
import           Lamdu.Config.Folder (Selection)
import           Lamdu.Config.Theme (Theme)
import qualified Lamdu.Config.Theme as Theme
import           Lamdu.Data.Db.Layout (DbM, ViewM)
import qualified Lamdu.Data.Db.Layout as DbLayout
import qualified Lamdu.Debug as Debug
import qualified Lamdu.Eval.Results as Results
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.GUI.CodeEdit as CodeEdit
import           Lamdu.GUI.IOTrans (IOTrans(..))
import qualified Lamdu.GUI.IOTrans as IOTrans
import qualified Lamdu.GUI.StatusBar as StatusBar
import qualified Lamdu.GUI.VersionControl as VersionControlGUI
import qualified Lamdu.GUI.VersionControl.Config as VCConfig
import           Lamdu.GUI.WidgetIds (defaultCursor)
import           Lamdu.I18N.Language (Language)
import qualified Lamdu.I18N.Language as Language
import qualified Lamdu.I18N.Texts as Texts
import           Lamdu.Settings (Settings)
import qualified Lamdu.Settings as Settings
import           Lamdu.Style (HasStyle)
import qualified Lamdu.VersionControl as VersionControl
import qualified Lamdu.VersionControl.Actions as VCActions
import           Revision.Deltum.Transaction (Transaction)

import           Lamdu.Prelude

type T = Transaction

type EvalResults = CurAndPrev (Results.EvalResults (ExprIRef.ValI ViewM))

type Ctx env =
    ( HasCallStack
    , MainLoop.HasMainLoopEnv env
    , Cache.HasFunctions env
    , Debug.HasMonitors env
    , HasStyle env
    , Has Hover.Style env
    , Settings.HasSettings env
    , Spacer.HasStdSpacing env
    , GuiState.HasState env
    , Theme.HasTheme env
    , Config.HasConfig env
    , Element.HasAnimIdPrefix env
    , CodeEdit.HasEvalResults env ViewM
    , CodeEdit.HasExportActions env ViewM
    , VCConfig.HasConfig env, VCConfig.HasTheme env
    , Menu.HasConfig env
    , SearchMenu.HasTermStyle env
    , Language.HasLanguage env
    , E.HasTexts env
    )

layout ::
    Ctx env =>
    [Selection Theme] -> [Selection Language] -> Property IO Settings ->
    ReaderT env (T DbM) (Gui Widget (IOTrans DbM))
layout themeNames langNames settingsProp =
    do
        vcActions <-
            VersionControl.makeActions <&> VCActions.hoist IOTrans.liftTrans & lift
        theTheme <- Lens.view Theme.theme
        fullSize <- Lens.view (MainLoop.mainLoopEnv . MainLoop.eWindowSize)
        state <- Lens.view has
        let viewToDb x = x & IOTrans.trans %~ VersionControl.runEvent state
        (gotoDefinition, codeEdit) <-
            CodeEdit.make DbLayout.codeAnchors DbLayout.guiAnchors (fullSize ^. _1)
            & Reader.mapReaderT VersionControl.runAction
            <&> _1 %~ StatusBar.hoist viewToDb
            <&> _2 . Lens.mapped %~ viewToDb
        statusBar <-
            StatusBar.make gotoDefinition themeNames langNames settingsProp
            (fullSize ^. _1) vcActions
        let statusBarWidget = statusBar ^. StatusBar.widget . Align.tValue

        versionControlCfg <- Lens.view (Config.config . Config.versionControl)
        vcEventMap <- VersionControlGUI.eventMap ?? versionControlCfg ?? vcActions

        quitKeys <- Lens.view (Config.config . Config.quitKeys)
        quitTxt <- Lens.view (Language.texts . Texts.quit)
        let quitEventMap = E.keysEventMap quitKeys (E.Doc [quitTxt]) (error "Quit")

        pure statusBarWidget
            /-/ Spacer.vspaceLines (theTheme ^. Theme.topPadding)
            /-/ pure codeEdit
            <&> Scroll.focusAreaInto fullSize
            <&> Widget.weakerEventsWithoutPreevents
                (statusBar ^. StatusBar.globalEventMap <> quitEventMap
                    <> vcEventMap)

make ::
    Ctx env =>
    [Selection Theme] -> [Selection Language] -> Property IO Settings -> env ->
    T DbM (Gui Widget (IOTrans DbM))
make themeNames langNames settingsProp env =
    layout themeNames langNames settingsProp
    & GuiState.assignCursor mempty defaultCursor
    & (`runReaderT` env)
