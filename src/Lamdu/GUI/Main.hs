module Lamdu.GUI.Main
    ( make
    , CodeEdit.Model
    , CodeEdit.ExportActions(..)
    , CodeEdit.EvalResults
    , TitledSelection(..), title, selection
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.Once (OnceT, _OnceT)
import           Control.Monad.Reader (ReaderT(..))
import qualified Control.Monad.Reader as Reader
import           Control.Monad.Trans.State (mapStateT)
import           Control.Monad.Transaction (MonadTransaction(..))
import           Data.Property (Property)
import qualified GUI.Momentu as M
import qualified GUI.Momentu.EventMap as E
import qualified GUI.Momentu.Main as MainLoop
import qualified GUI.Momentu.Scroll as Scroll
import qualified GUI.Momentu.State as GuiState
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Spacer as Spacer
import qualified Lamdu.Config as Config
import qualified Lamdu.Config.Folder as Folder
import qualified Lamdu.Config.Theme as Theme
import           Lamdu.Data.Db.Layout (DbM, ViewM)
import qualified Lamdu.Data.Db.Layout as DbLayout
import qualified Lamdu.GUI.CodeEdit as CodeEdit
import           Lamdu.GUI.IOTrans (IOTrans(..))
import qualified Lamdu.GUI.IOTrans as IOTrans
import           Lamdu.GUI.StatusBar (TitledSelection(..), title, selection)
import qualified Lamdu.GUI.StatusBar as StatusBar
import qualified Lamdu.GUI.VersionControl as VersionControlGUI
import           Lamdu.GUI.WidgetIds (defaultCursor)
import           Lamdu.Settings (Settings)
import           Lamdu.Sugar.Config (Sugars)
import qualified Lamdu.VersionControl as VersionControl
import qualified Lamdu.VersionControl.Actions as VCActions
import           Revision.Deltum.Transaction (Transaction)

import           Lamdu.Prelude

type T = Transaction

make ::
    _ =>
    [TitledSelection Folder.Theme] -> [TitledSelection Folder.Language] ->
    Property IO Settings -> Property IO (Sugars Bool) ->
    env -> CodeEdit.Model env ViewM ->
    OnceT (T DbM) (M.Widget (IOTrans DbM))
make themeNames langNames settingsProp sugarsProp env mkWorkArea =
    do
        vcActions <-
            VersionControl.makeActions <&> VCActions.hoist IOTrans.liftTrans & transaction
        state <- Lens.view has
        let viewToDb x = x & IOTrans.trans %~ VersionControl.runEvent state
        (gotoDefinition, codeEdit) <-
            CodeEdit.make DbLayout.codeAnchors DbLayout.guiAnchors (fullSize ^. _1) mkWorkArea
            & Reader.mapReaderT (_OnceT %~ mapStateT VersionControl.runAction)
            <&> _1 %~ StatusBar.hoist viewToDb
            <&> _2 . Widget.updates %~ viewToDb
        statusBar <-
            StatusBar.make gotoDefinition themeNames langNames settingsProp sugarsProp
            (fullSize ^. _1) vcActions
        let statusBarWidget = statusBar ^. StatusBar.widget . M.tValue

        vcEventMap <-
            VersionControlGUI.eventMap ??
            env ^. Config.hasConfig . Config.versionControl ?? vcActions

        pure statusBarWidget
            M./-/ Spacer.vspaceLines (env ^. has . Theme.topPadding)
            M./-/ pure codeEdit
            <&> Scroll.focusAreaInto fullSize
            <&> Widget.weakerEventsWithoutPreevents
                (statusBar ^. StatusBar.globalEventMap <> quitEventMap
                    <> vcEventMap)
    & GuiState.assignCursor mempty defaultCursor
    & (`runReaderT` env)
    where
        fullSize = env ^. has . MainLoop.eWindowSize
        quitEventMap =
            E.keysEventMap (env ^. has . Config.quitKeys) (E.Doc [env ^. has . MainLoop.textQuit]) (error "Quit")
