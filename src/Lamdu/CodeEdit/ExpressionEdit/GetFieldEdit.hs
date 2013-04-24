{-# LANGUAGE OverloadedStrings #-}
module Lamdu.CodeEdit.ExpressionEdit.GetFieldEdit(make) where

import Control.Applicative ((<$>))
import Control.MonadA (MonadA)
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui (ExpressionGui)
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad (ExprGuiM)
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Lamdu.BottleWidgets as BWidgets
import qualified Lamdu.CodeEdit.ExpressionEdit.ExpressionGui as ExpressionGui
import qualified Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.CodeEdit.Sugar as Sugar

make ::
  MonadA m =>
  Sugar.GetField (Sugar.Expression m) -> Widget.Id -> ExprGuiM m (ExpressionGui m)
make (Sugar.GetField tagExpr recExpr) =
  ExpressionGui.wrapExpression $ \myId -> do
    recExprEdit <- ExprGuiM.makeSubexpresion recExpr
    tagEdit <- ExprGuiM.makeSubexpresion tagExpr
    dotLabel <-
      ExpressionGui.fromValueWidget <$>
      (ExprGuiM.widgetEnv . BWidgets.makeLabel "." . Widget.toAnimId) myId
    return $ ExpressionGui.hbox [recExprEdit, dotLabel, tagEdit]
