{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.LiteralEdit
    ( make
    ) where


import           Control.Lens.Operators
import           Control.MonadA (MonadA)
import           Data.Store.Guid (Guid)
import qualified Data.Store.Transaction as Transaction
import qualified Graphics.UI.Bottle.EventMap as E
import           Graphics.UI.Bottle.ModKey (ModKey(..))
import           Graphics.UI.Bottle.Widget (Widget)
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets as BWidgets
import qualified Graphics.UI.Bottle.Widgets.TextEdit as TextEdit
import qualified Graphics.UI.Bottle.WidgetsEnvT as WE
import qualified Graphics.UI.GLFW as GLFW
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
    MonadA m => String -> T m (Guid, Sugar.EntityId) -> Widget.EventHandlers (T m)
mkEditEventMap valText setToHole =
    Widget.keysEventMapMovesCursor [ModKey mempty GLFW.Key'Enter]
    (E.Doc ["Edit", "Double"]) $
    do
        (guid, entityId) <- setToHole
        setHoleStateAndJump guid (HoleState valText) entityId

genericView ::
    (MonadA m, Format a) =>
    (Style -> TextEdit.Style) -> a -> Widget.Id ->
    ExprGuiM m (String, Widget (T m))
genericView getStyle val myId =
    do
        style <- ExprGuiM.readStyle
        BWidgets.makeFocusableTextView valText myId
            & ExprGuiM.widgetEnv
            & ExprGuiM.localEnv (WE.envTextStyle .~ getStyle style)
    <&> (,) valText
    where
        valText = format val

make ::
    MonadA m =>
    Sugar.Literal -> Sugar.Payload m ExprGuiT.Payload ->
    ExprGuiM m (ExpressionGui m)
make lit pl =
    do
        (holeText, view) <-
            WidgetIds.fromExprPayload pl &
            case lit of
            Sugar.LiteralNum x -> genericView Style.styleNum x
            Sugar.LiteralBytes x -> genericView Style.styleBytes x
            Sugar.LiteralText x -> genericView Style.styleText x
        let editEventMap =
                case pl ^. Sugar.plActions . Sugar.setToHole of
                Sugar.SetToHole action -> mkEditEventMap holeText action
                Sugar.SetWrapperToHole action -> mkEditEventMap holeText action
                Sugar.AlreadyAHole -> error "Literal val is a hole?!"
                Sugar.AlreadyAppliedToHole -> error "Literal val is an apply?!"
        view
            & Widget.weakerEvents editEventMap
            & ExpressionGui.fromValueWidget
            & return
    & ExpressionGui.stdWrap pl
