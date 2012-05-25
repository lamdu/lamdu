{-# LANGUAGE OverloadedStrings #-}
module Editor.CodeEdit.ExpressionEdit.ApplyEdit(make) where

import Editor.Anchors (ViewTag)
import Editor.CTransaction (TWidget, assignCursor)
import Editor.CodeEdit.ExpressionEdit.ExpressionMaker (ExpressionEditMaker)
import Editor.MonadF (MonadF)
import qualified Editor.BottleWidgets as BWidgets
import qualified Editor.CodeEdit.Sugar as Sugar
import qualified Editor.WidgetIds as WidgetIds
import qualified Graphics.UI.Bottle.Widget as Widget

make
  :: MonadF m
  => ExpressionEditMaker m
  -> Sugar.Apply m
  -> Widget.Id
  -> TWidget ViewTag m
make makeExpressionEdit (Sugar.Apply func arg) myId =
  (assignCursor myId . WidgetIds.fromGuid . Sugar.guid . Sugar.rActions) func $ do
    funcEdit <- makeExpressionEdit func
    argEdit <- makeExpressionEdit arg
    return $ BWidgets.hboxSpaced [funcEdit, argEdit]
