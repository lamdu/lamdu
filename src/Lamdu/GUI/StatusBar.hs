-- | The Lamdu status bar
{-# LANGUAGE FlexibleContexts #-}
module Lamdu.GUI.StatusBar
    ( make
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.Transaction (MonadTransaction(..))
import           Data.Property (Property)
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.Draw as Draw
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.Glue ((/|/))
import qualified GUI.Momentu.Hover as Hover
import qualified GUI.Momentu.State as GuiState
import           GUI.Momentu.Widget (Widget)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Spacer as Spacer
import qualified GUI.Momentu.Widgets.TextEdit as TextEdit
import qualified GUI.Momentu.Widgets.TextView as TextView
import qualified Lamdu.Config.Theme as Theme
import qualified Lamdu.Config.Theme.TextColors as TextColors
import           Lamdu.GUI.IOTrans (IOTrans(..))
import qualified Lamdu.GUI.IOTrans as IOTrans
import qualified Lamdu.GUI.Settings as SettingsWidget
import qualified Lamdu.GUI.StatusBar.Common as StatusBar
import qualified Lamdu.GUI.Styled as Styled
import qualified Lamdu.GUI.VersionControl as VersionControlGUI
import qualified Lamdu.GUI.VersionControl.Config as VCConfig
import           Lamdu.Settings (Settings)
import qualified Lamdu.Themes as Themes
import qualified Lamdu.VersionControl.Actions as VCActions

import           Lamdu.Prelude

make ::
    ( MonadReader env m, MonadTransaction n m
    , TextEdit.HasStyle env, Theme.HasTheme env, Hover.HasStyle env
    , GuiState.HasCursor env, Element.HasAnimIdPrefix env
    , VCConfig.HasConfig env, VCConfig.HasTheme env, Spacer.HasStdSpacing env
    ) =>
    [Themes.Selection] -> Property IO Settings ->
    Widget.R -> VCActions.Actions n (IOTrans n) ->
    m (Widget (IOTrans n GuiState.Update))
make themeNames settingsProp width vcActions =
    do
        branchChoice <-
            VersionControlGUI.makeBranchSelector
            IOTrans.liftTrans transaction vcActions
        branchLabel <-
            TextView.make ?? "Branch " ?? ["BranchHeader"]
            & Styled.withColor TextColors.infoTextColor
        let branchWidget = branchLabel /|/ branchChoice

        settings <-
            SettingsWidget.forStatusBar themeNames settingsProp
            <&> Align.tValue %~ fmap IOTrans.liftIO

        theTheme <- Lens.view Theme.theme
        hspace <- StatusBar.hspacer
        Draw.backgroundColor
            ?? Theme.statusBarBGColor (Theme.statusBar theTheme)
            ?? ((settings /|/ hspace /|/ branchWidget) ^. Align.tValue
                & Element.width .~ width)
