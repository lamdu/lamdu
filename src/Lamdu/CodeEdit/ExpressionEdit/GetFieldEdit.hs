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
import qualified Lamdu.CodeEdit.Sugar.Types as Sugar
import qualified Lamdu.WidgetIds as WidgetIds

make ::
  MonadA m =>
  Sugar.Payload Sugar.Name m a ->
  Sugar.GetField (ExprGuiM.SugarExpr m) -> Widget.Id -> ExprGuiM m (ExpressionGui m)
make pl (Sugar.GetField recExpr tagExpr) =
  ExpressionGui.wrapExpression pl $ \myId ->
  ExprGuiM.assignCursor myId destId $ do
    recExprEdit <- ExprGuiM.makeSubexpression 11 recExpr
    tagEdit <- ExprGuiM.makeSubexpression 12 tagExpr
    dotLabel <-
      ExpressionGui.fromValueWidget <$>
      (ExprGuiM.widgetEnv . BWidgets.makeLabel "." . Widget.toAnimId) myId
    return $ ExpressionGui.hbox [recExprEdit, dotLabel, tagEdit]
  where
    destId = WidgetIds.fromGuid $ tagExpr ^. Sugar.rPayload . Sugar.plGuid
