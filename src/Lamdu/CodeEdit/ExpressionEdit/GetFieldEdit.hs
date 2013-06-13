{-# LANGUAGE OverloadedStrings #-}
module Lamdu.CodeEdit.ExpressionEdit.GetFieldEdit(make) where

import Control.Applicative ((<$>))
import Control.Lens.Operators
import Control.MonadA (MonadA)
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui (ExpressionGui)
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad (ExprGuiM)
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Lamdu.BottleWidgets as BWidgets
import qualified Lamdu.CodeEdit.ExpressionEdit.ExpressionGui as ExpressionGui
import qualified Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.CodeEdit.Sugar as Sugar
import qualified Lamdu.WidgetIds as WidgetIds

make ::
  MonadA m =>
  Sugar.GetField (Sugar.ExpressionN m) -> Widget.Id -> ExprGuiM m (ExpressionGui m)
make (Sugar.GetField recExpr tagExpr) =
  ExpressionGui.wrapExpression $ \myId ->
  ExprGuiM.assignCursor myId destId $ do
    recExprEdit <- ExprGuiM.makeSubexpresion 11 recExpr
    tagEdit <- ExprGuiM.makeSubexpresion 12 tagExpr
    dotLabel <-
      ExpressionGui.fromValueWidget <$>
      (ExprGuiM.widgetEnv . BWidgets.makeLabel "." . Widget.toAnimId) myId
    return $ ExpressionGui.hbox [recExprEdit, dotLabel, tagEdit]
  where
    destId = WidgetIds.fromGuid $ tagExpr ^. Sugar.rPayload . Sugar.plGuid
