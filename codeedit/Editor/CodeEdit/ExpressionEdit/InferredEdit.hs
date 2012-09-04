module Editor.CodeEdit.ExpressionEdit.InferredEdit(make) where

import Control.Arrow (second)
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
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator

fDConfig :: FocusDelegator.Config
fDConfig = FocusDelegator.Config
  { FocusDelegator.startDelegatingKey = Config.replaceInferredValueKey
  , FocusDelegator.startDelegatingDoc = "Replace inferred value"
  , FocusDelegator.stopDelegatingKey = Config.keepInferredValueKey
  , FocusDelegator.stopDelegatingDoc = "Keep inferred value"
  }

make
  :: MonadF m
  => ExpressionGui.Maker m -> Sugar.Inferred m -> Guid
  -> Widget.Id
  -> VarAccess m (Maybe (HoleEdit.ResultPicker m), ExpressionGui m)
make makeExpressionEdit inferred guid =
  BWidgets.wrapDelegatedVA fDConfig FocusDelegator.NotDelegating
  (second . ExpressionGui.atEgWidget) $
  makeUnwrapped makeExpressionEdit inferred guid

makeUnwrapped
  :: MonadF m
  => ExpressionGui.Maker m -> Sugar.Inferred m -> Guid
  -> Widget.Id
  -> VarAccess m (Maybe (HoleEdit.ResultPicker m), ExpressionGui m)
makeUnwrapped makeExpressionEdit inferred guid myId = do
  mInnerCursor <- VarAccess.otransaction $ OT.subCursor myId
  case mInnerCursor of
    Nothing ->
      liftM ((,) Nothing) .
      ExpressionGui.atEgWidgetM
      ( VarAccess.otransaction
      . BWidgets.makeFocusableView myId
      . Widget.tint Config.inferredValueTint
      . Widget.scale Config.inferredValueScaleFactor
      ) =<<
      makeExpressionEdit (Sugar.iValue inferred)
    Just _ ->
      HoleEdit.makeUnwrapped makeExpressionEdit (Sugar.iHole inferred) guid myId
