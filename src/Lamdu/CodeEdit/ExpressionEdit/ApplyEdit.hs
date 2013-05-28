{-# LANGUAGE OverloadedStrings #-}
module Lamdu.CodeEdit.ExpressionEdit.ApplyEdit(make) where

import Control.Lens ((^.))
import Control.MonadA (MonadA)
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui (ExpressionGui)
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad (ExprGuiM)
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Lamdu.CodeEdit.ExpressionEdit.ExpressionGui as ExpressionGui
import qualified Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.CodeEdit.ExpressionEdit.Parens as Parens
import qualified Lamdu.CodeEdit.Sugar as Sugar
import qualified Lamdu.Data.Expression as Expr
import qualified Lamdu.WidgetIds as WidgetIds

make ::
  MonadA m =>
  ExpressionGui.ParentPrecedence ->
  Expr.Apply (Sugar.ExpressionN m) ->
  Widget.Id -> ExprGuiM m (ExpressionGui m)
make parentPrecedence (Expr.Apply func arg) =
  ExpressionGui.wrapParenify parentPrecedence (ExpressionGui.MyPrecedence 10)
  Parens.addHighlightedTextParens $ \myId ->
  (ExprGuiM.assignCursor myId . WidgetIds.fromGuid) (arg ^. Sugar.rGuid) $ do
    funcEdit <- ExprGuiM.makeSubexpresion 10 func
    argEdit <- ExprGuiM.makeSubexpresion 11 arg
    return $ ExpressionGui.hboxSpaced [funcEdit, argEdit]
