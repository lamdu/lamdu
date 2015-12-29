{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.LiteralEdit
    ( makeNum, makeBytes
    ) where

import           Prelude.Compat

import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.MonadA (MonadA)
import qualified Data.ByteString as SBS
import           Data.Store.Guid (Guid)
import qualified Data.Store.Transaction as Transaction
import qualified Graphics.UI.Bottle.EventMap as E
import           Graphics.UI.Bottle.ModKey (ModKey(..))
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets as BWidgets
import qualified Graphics.UI.GLFW as GLFW
import qualified Lamdu.Config as Config
import           Lamdu.Formatting (formatNum, formatBytes)
import           Lamdu.GUI.ExpressionEdit.HoleEdit.State (HoleState(..), setHoleStateAndJump)
import           Lamdu.GUI.ExpressionGui (ExpressionGui)
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.ExpressionGui.Types as ExprGuiT
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.Sugar.Types as Sugar

type T = Transaction.Transaction

setColor :: MonadA m => ExprGuiM m a -> ExprGuiM m a
setColor action =
    do
        config <- ExprGuiM.readConfig
        ExprGuiM.withFgColor (Config.literalColor config) action

mkEditEventMap ::
    MonadA m => String -> T m (Guid, Sugar.EntityId) -> Widget.EventHandlers (T m)
mkEditEventMap valText setToHole =
    Widget.keysEventMapMovesCursor [ModKey mempty GLFW.Key'Enter]
    (E.Doc ["Edit", "Double"]) $
    do
        (guid, entityId) <- setToHole
        setHoleStateAndJump guid (HoleState valText) entityId

makeGeneric ::
    MonadA m =>
    String -> Sugar.Payload m ExprGuiT.Payload ->
    ExprGuiM m (ExpressionGui m)
makeGeneric valText pl =
    BWidgets.makeFocusableTextView valText myId
    & setColor . ExprGuiM.widgetEnv
    <&> Widget.weakerEvents editEventMap
    <&> ExpressionGui.fromValueWidget
    & ExpressionGui.stdWrap pl
    where
        myId = WidgetIds.fromExprPayload pl
        editEventMap =
            case pl ^? Sugar.plActions . Lens._Just . Sugar.setToHole of
            Just (Sugar.SetToHole action) -> mkEditEventMap valText action
            Just (Sugar.SetWrapperToHole action) -> mkEditEventMap valText action
            Just Sugar.AlreadyAHole -> error "Literal val is a hole?!"
            Just Sugar.AlreadyAppliedToHole -> error "Literal val is an apply?!"
            Nothing -> mempty -- not modifiable

makeNum ::
    MonadA m =>
    Double -> Sugar.Payload m ExprGuiT.Payload ->
    ExprGuiM m (ExpressionGui m)
makeNum = makeGeneric . formatNum

makeBytes ::
    MonadA m =>
    SBS.ByteString -> Sugar.Payload m ExprGuiT.Payload ->
    ExprGuiM m (ExpressionGui m)
makeBytes = makeGeneric . formatBytes
