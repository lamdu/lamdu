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

make
  :: MonadF m
  => ExpressionGui.Maker m -> Sugar.Polymorphic m
  -> Widget.Id
  -> VarAccess m (ExpressionGui m)
make makeExpressionEdit poly myId =
  VarAccess.assignCursor myId compactId $ do
    -- TODO: This is just to detect whether cursor is in the full expression.
    -- Even when it's not displayed, which is wasteful.
    fullExprEdit <- makeExpressionEdit $ Sugar.pFullExpression poly
    -- We are inside a non-delegating focus delegator made by makeExpressionEdit,
    -- so if the cursor is on us it means user enterred our widget.
    if Widget.wIsFocused $ ExpressionGui.egWidget fullExprEdit
      then return $ bg Config.polymorphicFullBGColor fullExprEdit
      else
        (liftM . bg) Config.polymorphicCompactBGColor .
        VarAccess.otransaction .
        ExpressionGui.atEgWidgetM (BWidgets.makeFocusableView myId) =<<
        makeExpressionEdit (Sugar.pCompact poly)
  where
    bg = ExpressionGui.atEgWidget . Widget.backgroundColor 13 (Widget.toAnimId myId ++ ["bg"])
    compactId = WidgetIds.fromGuid . Sugar.rGuid $ Sugar.pCompact poly
