{-# LANGUAGE OverloadedStrings #-}
module Editor.CodeEdit.ExpressionEdit.ApplyEdit(make) where

import Editor.Anchors (ViewTag)
import Editor.OTransaction (OTransaction)
import Editor.CodeEdit.ExpressionEdit.ExpressionGui (ExpressionGui)
import Editor.MonadF (MonadF)
import qualified Editor.CodeEdit.ExpressionEdit.ExpressionGui as ExpressionGui
import qualified Editor.OTransaction as OT
import qualified Editor.CodeEdit.Sugar as Sugar
import qualified Editor.WidgetIds as WidgetIds
import qualified Graphics.UI.Bottle.Widget as Widget

make
  :: MonadF m
  => ExpressionGui.Maker m
  -> Sugar.Apply m
  -> Widget.Id
  -> OTransaction ViewTag m (ExpressionGui m)
make makeExpressionEdit (Sugar.Apply func arg) myId =
  (OT.assignCursor myId . WidgetIds.fromGuid . Sugar.rGuid) arg $ do
    funcEdit <- makeExpressionEdit func
    argEdit <- makeExpressionEdit arg
    return $ ExpressionGui.hboxSpaced [funcEdit, argEdit]
