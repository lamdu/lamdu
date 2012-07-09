{-# LANGUAGE OverloadedStrings #-}
module Editor.CodeEdit.ExpressionEdit.ApplyEdit(make) where

import Editor.Anchors (ViewTag)
import Editor.OTransaction (TWidget)
import Editor.MonadF (MonadF)
import qualified Editor.BottleWidgets as BWidgets
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
  -> TWidget ViewTag m
make makeExpressionEdit (Sugar.Apply func arg) myId =
  (OT.assignCursor myId . WidgetIds.fromGuid . Sugar.guid . Sugar.rEntity) arg $ do
    funcEdit <- makeExpressionEdit func
    argEdit <- makeExpressionEdit arg
    return $ BWidgets.hboxCenteredSpaced [funcEdit, argEdit]
