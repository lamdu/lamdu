module Editor.CodeEdit.ExpressionEdit.InferredEdit(make) where

import Control.Monad (liftM)
import Data.Store.Guid (Guid)
import Editor.CodeEdit.ExpressionEdit.ExpressionGui (ExpressionGui)
import Editor.CodeEdit.VarAccess (VarAccess)
import Editor.MonadF (MonadF)
import qualified Editor.BottleWidgets as BWidgets
import qualified Editor.CodeEdit.ExpressionEdit.ExpressionGui as ExpressionGui
import qualified Editor.CodeEdit.ExpressionEdit.HoleEdit as HoleEdit
import qualified Editor.CodeEdit.Sugar as Sugar
import qualified Editor.CodeEdit.VarAccess as VarAccess
import qualified Editor.Config as Config
import qualified Editor.OTransaction as OT
import qualified Editor.WidgetIds as WidgetIds
import qualified Graphics.UI.Bottle.Widget as Widget

make
  :: MonadF m
  => ExpressionGui.Maker m -> Sugar.Inferred m -> Guid
  -> Widget.Id
  -> VarAccess m (Maybe (HoleEdit.ResultPicker m), ExpressionGui m)
make makeExpressionEdit inferred guid myId = do
  -- We are inside a non-delegating focus delegator made by makeExpressionEdit,
  -- so if the cursor is on us it means user enterred our widget.
  mInnerCursor <- VarAccess.otransaction $ OT.subCursor myId
  case mInnerCursor of
    Nothing ->
      liftM ((,) Nothing) .
      ExpressionGui.atEgWidgetM
      ( VarAccess.otransaction
      . BWidgets.makeFocusableView (WidgetIds.searchTermId myId)
      . Widget.tint Config.inferredValueTint
      . Widget.scale Config.inferredValueScaleFactor
      ) =<<
      makeExpressionEdit (Sugar.iValue inferred)
    Just _ ->
      HoleEdit.make makeExpressionEdit (Sugar.iHole inferred) guid myId
