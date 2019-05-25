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
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.Draw as Draw
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.Glue ((/|/))
import qualified GUI.Momentu.Hover as Hover
import qualified GUI.Momentu.State as GuiState
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Choice as Choice
import qualified GUI.Momentu.Widgets.Grid as Grid
import qualified GUI.Momentu.Widgets.Spacer as Spacer
import qualified GUI.Momentu.Widgets.TextEdit as TextEdit
import           Lamdu.Config (Config)
import qualified Lamdu.Config.Folder as Folder
import           Lamdu.Config.Theme (Theme)
import qualified Lamdu.Config.Theme as Theme
import           Lamdu.GUI.IOTrans (IOTrans(..))
import qualified Lamdu.GUI.IOTrans as IOTrans
import           Lamdu.GUI.Settings (TitledSelection(..), title, selection)
import qualified Lamdu.GUI.Settings as SettingsGui
import           Lamdu.GUI.StatusBar.Common
import qualified Lamdu.GUI.StatusBar.Common as StatusBar
import           Lamdu.GUI.Styled (info, label)
import qualified Lamdu.GUI.VersionControl as VersionControlGUI
import qualified Lamdu.GUI.VersionControl.Config as VCConfig
import qualified Lamdu.I18N.CodeUI as Texts
import qualified Lamdu.I18N.StatusBar as Texts
import qualified Lamdu.I18N.Versioning as Texts
import           Lamdu.Settings (Settings)
import qualified Lamdu.Style as Style
import qualified Lamdu.VersionControl.Actions as VCActions

import           Lamdu.Prelude

make ::
    ( MonadReader env m, MonadTransaction n m
    , TextEdit.HasStyle env, Has Theme env, Has Hover.Style env
    , GuiState.HasState env, Element.HasAnimIdPrefix env
    , TextEdit.HasTexts env
    , Grid.HasTexts env
    , Has VCConfig.Config env, Has VCConfig.Theme env, Spacer.HasStdSpacing env
    , Has Config env
    , Has Style.Style env
    , Has (Texts.StatusBar Text) env
    , Has (Choice.Texts Text) env
    , Has (Texts.Versioning Text) env
    , Has (Texts.CodeUI Text) env
    ) =>
    StatusWidget (IOTrans n) ->
    [TitledSelection Folder.Theme] -> [TitledSelection Folder.Language] ->
    Property IO Settings ->
    Widget.R -> VCActions.Actions n (IOTrans n) ->
    m (StatusWidget (IOTrans n))
make gotoDefinition themeNames langNames settingsProp width vcActions =
    do
        branchSelector <-
            info (label Texts.sbBranch)
            /|/ VersionControlGUI.makeBranchSelector IOTrans.liftTrans
                transaction vcActions
            <&> StatusBar.fromWidget

        statusWidgets <-
            SettingsGui.makeStatusWidgets themeNames langNames settingsProp
            <&> SettingsGui.hoist IOTrans.liftIO

        theTheme <- Lens.view has
        bgColor <-
            Draw.backgroundColor ?? theTheme ^. Theme.statusBar . Theme.statusBarBGColor
        padToSize <- Element.padToSize
        (StatusBar.combineEdges ?? width ?? gotoDefinition)
            <*> ( StatusBar.combine ??
                    [ statusWidgets ^. SettingsGui.annotationWidget
                    , statusWidgets ^. SettingsGui.themeWidget
                    , branchSelector
                    , statusWidgets ^. SettingsGui.languageWidget
                    , statusWidgets ^. SettingsGui.helpWidget
                    ]
                )
            <&> StatusBar.widget . Align.tValue %~ padToSize (Vector2 width 0) 0
            <&> StatusBar.widget %~ bgColor
