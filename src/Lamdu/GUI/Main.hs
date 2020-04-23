{-# LANGUAGE ConstraintKinds #-}
module Lamdu.GUI.Main
    ( make
    , CodeEdit.ExportRepl(..)
    , CodeEdit.ExportActions(..)
    , CodeEdit.EvalResults
    , TitledSelection(..), title, selection
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.Reader (ReaderT(..))
import qualified Control.Monad.Reader as Reader
import           Data.Property (Property)
import qualified GUI.Momentu.Align as Align
import           GUI.Momentu.Draw (Sprite)
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.Glue ((/-/))
import qualified GUI.Momentu.Hover as Hover
import qualified GUI.Momentu.Main as MainLoop
import qualified GUI.Momentu.Scroll as Scroll
import qualified GUI.Momentu.State as GuiState
import           GUI.Momentu.Widget (Widget)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Menu as Menu
import qualified GUI.Momentu.Widgets.Menu.Search as SearchMenu
import qualified GUI.Momentu.Widgets.Spacer as Spacer
import qualified Lamdu.Annotations as Annotations
import qualified Lamdu.Cache as Cache
import           Lamdu.Config (Config)
import qualified Lamdu.Config as Config
import qualified Lamdu.Config.Folder as Folder
import           Lamdu.Config.Theme (Theme)
import qualified Lamdu.Config.Theme as Theme
import           Lamdu.Config.Theme.Sprites (Sprites)
import           Lamdu.Data.Db.Layout (DbM, ViewM)
import qualified Lamdu.Data.Db.Layout as DbLayout
import qualified Lamdu.Data.Tag as Tag
import qualified Lamdu.Debug as Debug
import qualified Lamdu.GUI.CodeEdit as CodeEdit
import           Lamdu.GUI.IOTrans (IOTrans(..))
import qualified Lamdu.GUI.IOTrans as IOTrans
import           Lamdu.GUI.StatusBar (TitledSelection(..), title, selection)
import qualified Lamdu.GUI.StatusBar as StatusBar
import qualified Lamdu.GUI.VersionControl as VersionControlGUI
import qualified Lamdu.GUI.VersionControl.Config as VCConfig
import           Lamdu.GUI.WidgetIds (defaultCursor)
import qualified Lamdu.I18N.Language as Language
import qualified Lamdu.I18N.StatusBar as Texts
import           Lamdu.Settings (Settings)
import           Lamdu.Style (HasStyle)
import           Lamdu.Sugar (sugarWorkArea)
import qualified Lamdu.Sugar.Config as SugarConfig
import qualified Lamdu.VersionControl as VersionControl
import qualified Lamdu.VersionControl.Actions as VCActions
import           Revision.Deltum.Transaction (Transaction)

import           Lamdu.Prelude

type T = Transaction

type Ctx env =
    ( HasCallStack
    , MainLoop.HasMainLoopEnv env
    , Has (MainLoop.Texts Text) env
    , Has Cache.Functions env
    , Has Debug.Monitors env
    , HasStyle env
    , Has Hover.Style env
    , Has Settings env
    , Spacer.HasStdSpacing env
    , GuiState.HasState env
    , Has Theme env
    , Has Config env
    , Has SugarConfig.Config env
    , Element.HasAnimIdPrefix env
    , Has CodeEdit.EvalResults env
    , Has (CodeEdit.ExportActions ViewM) env
    , Has VCConfig.Config env, Has VCConfig.Theme env
    , Has Menu.Config env
    , Has Annotations.Mode env
    , Has SearchMenu.TermStyle env
    , Has (Texts.StatusBar Text) env
    , Language.HasLanguage env
    , Has (Sprites Sprite) env
    )

make ::
    Ctx env =>
    [TitledSelection Folder.Theme] -> [TitledSelection Folder.Language] ->
    Property IO Settings -> env ->
    T DbM (Widget (IOTrans DbM))
make themeNames langNames settingsProp env =
    do
        vcActions <-
            VersionControl.makeActions <&> VCActions.hoist IOTrans.liftTrans & lift
        theTheme <- Lens.view has
        fullSize <- Lens.view (has . MainLoop.eWindowSize)
        state <- Lens.view has
        let viewToDb x = x & IOTrans.trans %~ VersionControl.runEvent state
        (gotoDefinition, codeEdit) <-
            sugarWorkArea (Tag.getTagName env) env DbLayout.codeAnchors & lift
            >>= CodeEdit.make DbLayout.codeAnchors DbLayout.guiAnchors (fullSize ^. _1)
            & Reader.mapReaderT VersionControl.runAction
            <&> _1 %~ StatusBar.hoist viewToDb
            <&> _2 . Widget.updates %~ viewToDb
        statusBar <-
            StatusBar.make gotoDefinition themeNames langNames settingsProp
            (fullSize ^. _1) vcActions
        let statusBarWidget = statusBar ^. StatusBar.widget . Align.tValue

        versionControlCfg <- Lens.view (has . Config.versionControl)
        vcEventMap <- VersionControlGUI.eventMap ?? versionControlCfg ?? vcActions

        quitKeys <- Lens.view (has . Config.quitKeys)
        quitTxt <- Lens.view (has . MainLoop.textQuit)
        let quitEventMap = E.keysEventMap quitKeys (E.Doc [quitTxt]) (error "Quit")

        pure statusBarWidget
            /-/ Spacer.vspaceLines (theTheme ^. Theme.topPadding)
            /-/ pure codeEdit
            <&> Scroll.focusAreaInto fullSize
            <&> Widget.weakerEventsWithoutPreevents
                (statusBar ^. StatusBar.globalEventMap <> quitEventMap
                    <> vcEventMap)
    & GuiState.assignCursor mempty defaultCursor
    & (`runReaderT` env)
