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
import qualified Lamdu.GUI.StatusBar.Sugars as StatusBar.Sugars
import           Lamdu.GUI.Styled (info, label)
import qualified Lamdu.GUI.VersionControl as VersionControlGUI
import qualified Lamdu.I18N.StatusBar as Texts
import           Lamdu.Settings (Settings)
import           Lamdu.Sugar.Config (Sugars)
import qualified Lamdu.VersionControl.Actions as VCActions

import           Lamdu.Prelude

make ::
    _ =>
    StatusWidget (IOTrans n) ->
    [TitledSelection Folder.Theme] -> [TitledSelection Folder.Language] ->
    Property IO Settings -> Property IO (Sugars Bool) ->
    Double -> VCActions.Actions n (IOTrans n) ->
    m (StatusWidget (IOTrans n))
make gotoDefinition themeNames langNames settingsProp sugarsProp width vcActions =
    do
        branchSelector <-
            info (label Texts.sbBranch)
            M./|/ VersionControlGUI.makeBranchSelector IOTrans.liftTrans
                transaction vcActions
            <&> StatusBar.fromWidget
            & local (Element.elemIdPrefix <>~ "Branch Selector")

        statusWidgets <-
            SettingsGui.makeStatusWidgets themeNames langNames settingsProp
            <&> Lens.mapped %~ StatusBar.hoist IOTrans.liftIO

        theTheme <- Lens.view has

        sugarsWidget <- StatusBar.Sugars.make sugarsProp <&> StatusBar.hoist IOTrans.liftIO

        StatusBar.combineEdges width gotoDefinition
            [ statusWidgets ^. SettingsGui.annotationWidget
            , statusWidgets ^. SettingsGui.themeWidget
            , branchSelector
            , statusWidgets ^. SettingsGui.languageWidget
            , sugarsWidget
            , statusWidgets ^. SettingsGui.helpWidget
            ]
            >>= (StatusBar.widget . M.tValue) (Element.padToSize (Vector2 width 0) 0)
            >>= StatusBar.widget (M.backgroundColor (theTheme ^. Theme.statusBar . Theme.statusBarBGColor))
