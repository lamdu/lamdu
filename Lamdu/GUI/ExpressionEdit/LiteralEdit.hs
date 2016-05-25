{-# LANGUAGE RecordWildCards, NoImplicitPrelude, OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.LiteralEdit
    ( make
    ) where


import           Control.Lens.Operators
import           Control.Lens.Tuple
import           Data.UUID.Types (UUID)
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Graphics.UI.Bottle.EventMap as E
import           Graphics.UI.Bottle.ModKey (ModKey(..))
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets as BWidgets
import qualified Graphics.UI.Bottle.Widgets.Box as Box
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Graphics.UI.Bottle.Widgets.TextEdit as TextEdit
import qualified Graphics.UI.Bottle.WidgetsEnvT as WE
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

import           Prelude.Compat

type T = Transaction.Transaction

mkEditEventMap ::
    Monad m =>
    String -> T m (UUID, Sugar.EntityId) ->
    Widget.EventMap (T m Widget.EventResult)
mkEditEventMap valText setToHole =
    Widget.keysEventMapMovesCursor [ModKey mempty GLFW.Key'Enter]
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
        BWidgets.makeFocusableTextView valText myId
            & ExprGuiM.widgetEnv
            & ExprGuiM.localEnv (WE.envTextStyle .~ style)
            <&> Widget.weakerEvents editEventMap
            <&> ExpressionGui.fromValueWidget
    where
        myId = WidgetIds.fromExprPayload pl
        editEventMap =
            case pl ^. Sugar.plActions . Sugar.setToHole of
            Sugar.SetToHole action -> mkEditEventMap valText action
            Sugar.SetWrapperToHole action -> mkEditEventMap valText action
            Sugar.AlreadyAHole -> error "Literal val is a hole?!"
            Sugar.AlreadyAppliedToHole -> error "Literal val is an apply?!"
        valText = prop ^. Property.pVal & format

fdConfig :: Config.LiteralText -> FocusDelegator.Config
fdConfig Config.LiteralText{..} = FocusDelegator.Config
    { FocusDelegator.focusChildKeys = literalTextStartEditingKeys
    , FocusDelegator.focusChildDoc = E.Doc ["Edit", "Literal Text", "Start editing"]
    , FocusDelegator.focusParentKeys = literalTextStopEditingKeys
    , FocusDelegator.focusParentDoc = E.Doc ["Edit", "Literal Text", "Stop editing"]
    }

textEdit ::
    Monad m => Transaction.Property m String ->
    Sugar.Payload m ExprGuiT.Payload -> ExprGuiM m (ExpressionGui m)
textEdit prop pl =
    do
        config <- ExprGuiM.readConfig <&> Config.literalText
        style <- ExprGuiM.readStyle <&> Style.styleText
        edit <- do
            text <- BWidgets.makeTextEditor prop innerId
            let quoteSize = text ^. Widget.size & _1 .~ 0
            left <-
                BWidgets.makeLabel "“" (Widget.toAnimId myId)
                <&> Widget.padToSizeAlign quoteSize 0
            right <-
                BWidgets.makeLabel "„" (Widget.toAnimId myId)
                <&> Widget.padToSizeAlign quoteSize 1
            Box.hboxCentered [left, text, right] & return
            <&> ExpressionGui.fromValueWidget
            & ExprGuiM.widgetEnv
            & ExprGuiM.localEnv (WE.envTextStyle .~ style)
        ExpressionGui.makeFocusDelegator (fdConfig config)
            FocusDelegator.FocusEntryParent (WidgetIds.notDelegatingId myId)
            ?? edit
    & ExprGuiM.assignCursor myId (WidgetIds.notDelegatingId myId)
    where
        innerId = WidgetIds.delegatingId myId
        myId = WidgetIds.fromExprPayload pl

make ::
    Monad m =>
    Sugar.Literal (Transaction.Property m) -> Sugar.Payload m ExprGuiT.Payload ->
    ExprGuiM m (ExpressionGui m)
make lit pl =
    ExpressionGui.stdWrap pl
    <*>
    ( case lit of
        Sugar.LiteralNum x -> genericEdit Style.styleNum x
        Sugar.LiteralBytes x -> genericEdit Style.styleBytes x
        Sugar.LiteralText x -> textEdit x
    ) pl
