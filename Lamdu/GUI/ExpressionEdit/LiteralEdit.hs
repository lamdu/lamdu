{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.LiteralEdit
    ( makeInt
    ) where

import           Prelude.Compat

import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.MonadA (MonadA)
import           Data.Store.Guid (Guid)
import qualified Data.Store.Transaction as Transaction
import qualified Graphics.UI.Bottle.EventMap as E
import           Graphics.UI.Bottle.ModKey (ModKey(..))
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.GLFW as GLFW
import qualified Lamdu.Config as Config
import qualified Graphics.UI.Bottle.Widgets as BWidgets
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
        ExprGuiM.withFgColor (Config.literalIntColor config) action

mkEditEventMap ::
    MonadA m => Integer -> T m (Guid, Sugar.EntityId) -> Widget.EventHandlers (T m)
mkEditEventMap integer setToHole =
    Widget.keysEventMapMovesCursor [ModKey mempty GLFW.Key'Enter]
    (E.Doc ["Edit", "Integer"]) $
    do
        (guid, entityId) <- setToHole
        setHoleStateAndJump guid (HoleState (show integer)) entityId

makeInt ::
    MonadA m =>
    Integer -> Sugar.Payload m ExprGuiT.Payload ->
    ExprGuiM m (ExpressionGui m)
makeInt integer pl =
    BWidgets.makeFocusableTextView (show integer) myId
    & setColor . ExprGuiM.widgetEnv
    <&> Widget.weakerEvents editEventMap
    <&> ExpressionGui.fromValueWidget
    & ExpressionGui.stdWrap pl
    where
        myId = WidgetIds.fromExprPayload pl
        editEventMap =
            maybe mempty (mkEditEventMap integer) $
            pl ^? Sugar.plActions . Lens._Just . Sugar.setToHole . Sugar._SetToHole
