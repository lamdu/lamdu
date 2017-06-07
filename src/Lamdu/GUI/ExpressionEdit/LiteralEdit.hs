{-# LANGUAGE RecordWildCards, NoImplicitPrelude, OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.LiteralEdit
    ( make
    ) where

import qualified Control.Monad.Reader as Reader
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import           Data.UUID.Types (UUID)
import qualified Graphics.UI.Bottle.EventMap as E
import           Graphics.UI.Bottle.MetaKey (MetaKey(..), noMods)
import qualified Graphics.UI.Bottle.View as View
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widget.Aligned as AlignedWidget
import qualified Graphics.UI.Bottle.Widget.TreeLayout as TreeLayout
import qualified Graphics.UI.Bottle.Widgets.Box as Box
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Graphics.UI.Bottle.Widgets.TextEdit as TextEdit
import qualified Graphics.UI.Bottle.Widgets.TextEdit.Property as TextEdits
import qualified Graphics.UI.Bottle.Widgets.TextView as TextView
import qualified Graphics.UI.GLFW as GLFW
import qualified Lamdu.Config as Config
import           Lamdu.Formatting (Format(..))
import           Lamdu.GUI.ExpressionEdit.HoleEdit.State (HoleState(..), setHoleStateAndJump)
import           Lamdu.GUI.ExpressionGui (ExpressionGui)
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.ExpressionGui.Types as ExprGuiT
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Style (Style)
import qualified Lamdu.Style as Style
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

type T = Transaction.Transaction

mkEditEventMap ::
    Monad m =>
    Text -> T m (UUID, Sugar.EntityId) ->
    Widget.EventMap (T m Widget.EventResult)
mkEditEventMap valText setToHole =
    Widget.keysEventMapMovesCursor [MetaKey noMods GLFW.Key'Enter]
    (E.Doc ["Edit", "Value"]) $
    do
        (uuid, entityId) <- setToHole
        setHoleStateAndJump uuid (HoleState valText) entityId

genericEdit ::
    (Monad m, Format a) =>
    (Style -> TextEdit.Style) -> Transaction.Property m a ->
    Sugar.Payload m ExprGuiT.Payload -> ExprGuiM m (ExpressionGui m)
genericEdit getStyle prop pl =
    do
        style <- ExprGuiM.readStyle <&> getStyle
        TextView.makeFocusable ?? valText ?? myId
            & Reader.local (TextEdit.style .~ style)
            <&> Widget.weakerEvents editEventMap
            <&> TreeLayout.fromCenteredWidget
    where
        myId = WidgetIds.fromExprPayload pl
        editEventMap =
            case pl ^. Sugar.plActions . Sugar.setToHole of
            Sugar.SetToHole action -> mkEditEventMap valText action
            Sugar.SetWrapperToHole action -> mkEditEventMap valText action
            Sugar.AlreadyAHole -> error "Literal val is a hole?!"
        valText = prop ^. Property.pVal & format

fdConfig :: Config.LiteralText -> FocusDelegator.Config
fdConfig Config.LiteralText{..} = FocusDelegator.Config
    { FocusDelegator.focusChildKeys = literalTextStartEditingKeys
    , FocusDelegator.focusChildDoc = E.Doc ["Edit", "Literal Text", "Start editing"]
    , FocusDelegator.focusParentKeys = literalTextStopEditingKeys
    , FocusDelegator.focusParentDoc = E.Doc ["Edit", "Literal Text", "Stop editing"]
    }

textEdit ::
    Monad m => Transaction.Property m Text ->
    Sugar.Payload m ExprGuiT.Payload -> ExprGuiM m (ExpressionGui m)
textEdit prop pl =
    do
        config <- ExprGuiM.readConfig <&> Config.literalText
        style <- ExprGuiM.readStyle <&> (^. Style.styleText)
        edit <- do
            left <- TextView.makeLabel "“" <&> Widget.fromView
            text <- TextEdits.make ?? empty ?? prop ?? innerId
            right <- TextView.makeLabel "„" <&> Widget.fromView
            let quoteSize = text ^. View.size & _1 .~ 0
            Box.hboxCentered
                [ Widget.padToSizeAlign quoteSize 0 left
                , text
                , Widget.padToSizeAlign quoteSize 1 right
                ] & return
                <&> TreeLayout.fromCenteredWidget
                <&> TreeLayout.alignedWidget . AlignedWidget.absAlignedWidget . _1 . _2 .~
                    0.5 * (left ^. View.height)
            & Reader.local (TextEdit.style .~ style)
        FocusDelegator.make ?? fdConfig config
            ?? FocusDelegator.FocusEntryParent ?? WidgetIds.notDelegatingId myId
            <&> (TreeLayout.widget %~)
            ?? edit
    & Widget.assignCursor myId (WidgetIds.notDelegatingId myId)
    where
        empty = TextEdit.EmptyStrings "" ""
        innerId = WidgetIds.delegatingId myId
        myId = WidgetIds.fromExprPayload pl

make ::
    Monad m =>
    Sugar.Literal (Transaction.Property m) -> Sugar.Payload m ExprGuiT.Payload ->
    ExprGuiM m (ExpressionGui m)
make lit pl =
    ( case lit of
        Sugar.LiteralNum x -> genericEdit (^. Style.styleNum) x
        Sugar.LiteralBytes x -> genericEdit (^. Style.styleBytes) x
        Sugar.LiteralText x -> textEdit x
    ) pl
    & ExpressionGui.stdWrap pl
