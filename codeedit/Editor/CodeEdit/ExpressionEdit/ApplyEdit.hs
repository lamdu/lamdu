{-# LANGUAGE OverloadedStrings #-}
module Editor.CodeEdit.ExpressionEdit.ApplyEdit(make) where

import Editor.CodeEdit.ExpressionEdit.ExpressionGui (ExpressionGui, ExprGuiM)
import Editor.MonadF (MonadF)
import qualified Editor.CodeEdit.ExpressionEdit.ExpressionGui as ExpressionGui
import qualified Editor.CodeEdit.ExpressionEdit.ExpressionGui.Monad as ExprGuiM
import qualified Editor.CodeEdit.Parens as Parens
import qualified Editor.CodeEdit.Sugar as Sugar
import qualified Editor.Data as Data
import qualified Editor.WidgetIds as WidgetIds
import qualified Graphics.UI.Bottle.Widget as Widget

make
  :: MonadF m
  => Sugar.HasParens
  -> Data.Apply (Sugar.Expression m)
  -> Widget.Id
  -> ExprGuiM m (ExpressionGui m)
make hasParens (Data.Apply func arg) =
  ExpressionGui.wrapParenify hasParens Parens.addHighlightedTextParens $ \myId ->
  (ExprGuiM.assignCursor myId . WidgetIds.fromGuid . Sugar.rGuid) arg $ do
    funcEdit <- ExpressionGui.makeSubexpresion func
    argEdit <- ExpressionGui.makeSubexpresion arg
    return $ ExpressionGui.hboxSpaced [funcEdit, argEdit]
