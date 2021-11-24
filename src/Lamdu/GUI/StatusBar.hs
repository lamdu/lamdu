-- | The Lamdu status bar
module Lamdu.GUI.StatusBar
    ( module Lamdu.GUI.StatusBar.Common
    , TitledSelection(..), title, selection
    , make
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.Transaction (MonadTransaction(..))
import           Data.Property (Property)
import           Data.Vector.Vector2 (Vector2(..))
import qualified GUI.Momentu as M
import qualified GUI.Momentu.Element as Element
import qualified Lamdu.Config.Folder as Folder
import qualified Lamdu.Config.Theme as Theme
import           Lamdu.GUI.IOTrans (IOTrans(..))
import qualified Lamdu.GUI.IOTrans as IOTrans
import           Lamdu.GUI.Settings (TitledSelection(..), title, selection)
import qualified Lamdu.GUI.Settings as SettingsGui
import           Lamdu.GUI.StatusBar.Common
import qualified Lamdu.GUI.StatusBar.Common as StatusBar
import           Lamdu.GUI.Styled (info, label)
import qualified Lamdu.GUI.VersionControl as VersionControlGUI
import qualified Lamdu.I18N.StatusBar as Texts
import           Lamdu.Settings (Settings)
import qualified Lamdu.VersionControl.Actions as VCActions

import           Lamdu.Prelude

make ::
    _ =>
    StatusWidget m (IOTrans n) ->
    [TitledSelection Folder.Theme] -> [TitledSelection Folder.Language] ->
    Property IO Settings ->
    Double -> VCActions.Actions n (IOTrans n) ->
    m (StatusWidget m (IOTrans n))
make gotoDefinition themeNames langNames settingsProp width vcActions =
    do
        statusWidgets <-
            SettingsGui.makeStatusWidgets themeNames langNames settingsProp
            <&> Lens.mapped %~ StatusBar.hoist IOTrans.liftIO

        theTheme <- Lens.view has
        bgColor <-
            M.backgroundColor ?? theTheme ^. Theme.statusBar . Theme.statusBarBGColor
        padToSize <- Element.padToSize
        (StatusBar.combineEdges width gotoDefinition)
            ( StatusBar.combine
                [ statusWidgets ^. SettingsGui.annotationWidget
                , statusWidgets ^. SettingsGui.themeWidget
                , info (label Texts.sbBranch)
                  M./|/ VersionControlGUI.makeBranchSelector IOTrans.liftTrans transaction vcActions
                  & StatusBar.fromWidget
                , statusWidgets ^. SettingsGui.languageWidget
                , statusWidgets ^. SettingsGui.helpWidget
                ]
            )
            & StatusBar.widget . Lens.mapped . M.tValue %~ padToSize (Vector2 width 0) 0
            & StatusBar.widget . Lens.mapped %~ bgColor
            & pure
