-- | The Lamdu status bar
{-# LANGUAGE TypeFamilies #-}

module Lamdu.GUI.StatusBar
    ( module Lamdu.GUI.StatusBar.Common
    , TitledSelection(..), title, selection
    , make
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.Transaction (MonadTransaction(..))
import qualified Data.ByteString.Char8 as BS8
import           Data.Property (Property, pVal, pureModify)
import           Data.Vector.Vector2 (Vector2(..))
import qualified GUI.Momentu as M
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.EventMap as E
import qualified GUI.Momentu.Glue as Glue
import qualified GUI.Momentu.Hover as Hover
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.TextView as TextView
import qualified Lamdu.Config as Config
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
import           Lamdu.Sugar.Config (Sugars)
import qualified Lamdu.VersionControl.Actions as VCActions

import           Lamdu.Prelude
import GUI.Momentu.State (isSubCursor)

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
            & local (Element.animIdPrefix <>~ ["Branch Selector"])

        statusWidgets <-
            SettingsGui.makeStatusWidgets themeNames langNames settingsProp
            <&> Lens.mapped %~ StatusBar.hoist IOTrans.liftIO

        theTheme <- Lens.view has
        bgColor <-
            M.backgroundColor ?? theTheme ^. Theme.statusBar . Theme.statusBarBGColor
        padToSize <- Element.padToSize

        sugarsWidget <- makeSugars sugarsProp <&> StatusBar.hoist IOTrans.liftIO

        StatusBar.combineEdges ?? width ?? gotoDefinition ??
            [ statusWidgets ^. SettingsGui.annotationWidget
            , statusWidgets ^. SettingsGui.themeWidget
            , branchSelector
            , statusWidgets ^. SettingsGui.languageWidget
            , sugarsWidget
            , statusWidgets ^. SettingsGui.helpWidget
            ]
            <&> StatusBar.widget . M.tValue %~ padToSize (Vector2 width 0) 0
            <&> StatusBar.widget %~ bgColor

makeSugars :: _ => Property IO (Sugars Bool) -> m (StatusWidget IO)
makeSugars prop =
    do
        top <-
            (Widget.makeFocusableView ?? sugarsId <> Widget.Id ["Header"] <&> fmap)
            <*> (TextView.make ?? "S" ?? ["Sugar", "Header"])
        showMenu <- isSubCursor ?? sugarsId
        if showMenu
            then
                do
                    anchor <- Hover.anchor
                    texts <- Lens.view has
                    hover <- Hover.hover
                    vbox <- Glue.vbox
                    elems <-
                        traverse (uncurry (mkSugarToggle prop)) (((,) <$> texts <*> prop ^. pVal) ^@.. Lens.traversed)
                        <&> Lens.mapped %~ (^. M.tValue)
                    Glue.Poly (///) <- Glue.mkPoly ?? Glue.Vertical
                    let opt x =
                            ( M.Aligned x (anchor (top ^. M.tValue))
                                /// Hover.sequenceHover (hover (vbox (elems <&> M.Aligned x)))
                            ) ^. Align.value
                    top & M.tValue %~ Hover.hoverInPlaceOf [opt 0, opt 1] . anchor & pure
            else pure top
    <&> (`StatusBar.StatusWidget` mempty)

sugarsId :: Widget.Id
sugarsId = Widget.Id ["Sugar"]

mkSugarToggle :: _ => Property IO (Sugars Bool) -> Int -> (Text, Bool) -> m (M.TextWidget IO)
mkSugarToggle prop idx (text, val) =
    do
        actionKeys <- Lens.view (has . Config.actionKeys)
        disabledCol <- Lens.view (has . Theme.disabledColor)
        (Widget.makeFocusableView ?? myId <&> (M.tValue %~))
            <*> (TextView.make ?? text ?? Widget.toAnimId myId)
            & (if val then id else local (TextView.color .~ disabledCol))
            <&> M.tValue %~ M.weakerEvents (E.keysEventMap actionKeys (E.Doc ["TODO"]) toggle)
    & local (Element.animIdPrefix .~ Widget.toAnimId myId)
    where
        myId = sugarsId <> Widget.Id [BS8.pack (show idx)]
        toggle =
            pureModify prop (Lens.element idx .~ not val)
