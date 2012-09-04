{-# LANGUAGE OverloadedStrings #-}
module Editor.CodeEdit.ExpressionEdit.PolymorphicEdit(make) where

import Control.Monad (liftM)
import Editor.CodeEdit.ExpressionEdit.ExpressionGui (ExpressionGui)
import Editor.CodeEdit.VarAccess (VarAccess)
import Editor.MonadF (MonadF)
import qualified Editor.BottleWidgets as BWidgets
import qualified Editor.CodeEdit.ExpressionEdit.ExpressionGui as ExpressionGui
import qualified Editor.CodeEdit.Sugar as Sugar
import qualified Editor.CodeEdit.VarAccess as VarAccess
import qualified Editor.Config as Config
import qualified Editor.WidgetIds as WidgetIds
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator

-- make without the focus delegator
makeInner ::
  MonadF m =>
  ExpressionGui.Maker m -> Sugar.Polymorphic m ->
  Widget.Id -> VarAccess m (ExpressionGui m)
makeInner makeExpressionEdit poly myId =
  assignCursor $ do
    -- TODO: This is just to detect whether cursor is in the full expression.
    -- Even when it's not displayed, which is wasteful.
    fullExprEdit <- makeExpressionEdit $ Sugar.pFullExpression poly
    -- We are inside a non-delegating focus delegator made by makeExpressionEdit,
    -- so if the cursor is on us it means user enterred our widget.
    case (Widget.wIsFocused (ExpressionGui.egWidget fullExprEdit), Sugar.pCompact poly) of
      (False, Just compact) ->
        (liftM . bg) Config.polymorphicCompactBGColor .
        VarAccess.otransaction .
        ExpressionGui.atEgWidgetM (BWidgets.makeFocusableView myId) =<<
        makeExpressionEdit compact
      _ -> return $ bg Config.polymorphicFullBGColor fullExprEdit
  where
    bg = ExpressionGui.atEgWidget . Widget.backgroundColor 25 (Widget.toAnimId myId ++ ["bg"])
    assignCursor =
      maybe id (VarAccess.assignCursor myId . WidgetIds.fromGuid . Sugar.rGuid) $
      Sugar.pCompact poly

polymorphicFDConfig :: FocusDelegator.Config
polymorphicFDConfig = FocusDelegator.Config
  { FocusDelegator.startDelegatingKey = Config.expandPolymorphicKey
  , FocusDelegator.startDelegatingDoc = "Expand polymorphic"
  , FocusDelegator.stopDelegatingKey = Config.collapsePolymorphicKey
  , FocusDelegator.stopDelegatingDoc = "Collapse polymorphic"
  }

make ::
  MonadF m =>
  ExpressionGui.Maker m -> Sugar.Polymorphic m ->
  Widget.Id -> VarAccess m (ExpressionGui m)
make makeExpressionEdit poly =
  focusDelegate $ makeInner makeExpressionEdit poly
  where
    focusDelegate =
      case Sugar.pCompact poly of
      Nothing -> id
      Just _ ->
        BWidgets.wrapDelegatedVA polymorphicFDConfig
        FocusDelegator.NotDelegating ExpressionGui.atEgWidget
