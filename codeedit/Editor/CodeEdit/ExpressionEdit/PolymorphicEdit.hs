module Editor.CodeEdit.ExpressionEdit.PolymorphicEdit(make) where

import Control.Monad (liftM)
import Editor.Anchors (ViewTag)
import Editor.CodeEdit.ExpressionEdit.ExpressionGui (ExpressionGui)
import Editor.MonadF (MonadF)
import Editor.OTransaction (OTransaction)
import qualified Editor.BottleWidgets as BWidgets
import qualified Editor.CodeEdit.ExpressionEdit.ExpressionGui as ExpressionGui
import qualified Editor.CodeEdit.Sugar as Sugar
import qualified Editor.Config as Config
import qualified Editor.OTransaction as OT
import qualified Editor.WidgetIds as WidgetIds
import qualified Graphics.UI.Bottle.Widget as Widget

make
  :: MonadF m
  => ExpressionGui.Maker m -> Sugar.Polymorphic m
  -> Widget.Id
  -> OTransaction ViewTag m (ExpressionGui m)
make makeExpressionEdit poly myId =
  OT.assignCursor myId compactId $ do
    -- TODO: This is just to detect whether cursor is in the full expression.
    -- Even when it's not displayed, which is wasteful.
    fullExprEdit <- makeExpressionEdit $ Sugar.pFullExpression poly
    -- We are inside a non-delegating focus delegator made by makeExpressionEdit,
    -- so if the cursor is on us it means user enterred our widget.
    if Widget.wIsFocused $ ExpressionGui.egWidget fullExprEdit
      then return fullExprEdit
      else
        (liftM . ExpressionGui.atEgWidget . Widget.tint) Config.polymorphicCompactTint .
        ExpressionGui.atEgWidgetM (BWidgets.makeFocusableView myId) =<<
        makeExpressionEdit (Sugar.pCompact poly)
  where
    compactId = WidgetIds.fromGuid . Sugar.rGuid $ Sugar.pCompact poly