module Lamdu.GUI.ExpressionEdit.GetFieldEdit(make) where

import Control.Applicative ((<$>))
import Control.MonadA (MonadA)
import Graphics.UI.Bottle.WidgetId (augmentId)
import Lamdu.GUI.ExpressionGui (ExpressionGui)
import Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Lamdu.GUI.BottleWidgets as BWidgets
import qualified Lamdu.GUI.ExpressionEdit.TagEdit as TagEdit
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.Sugar.Types as Sugar

make ::
  MonadA m =>
  Sugar.GetField Sugar.Name (ExprGuiM.SugarExpr m) ->
  Sugar.Payload Sugar.Name m ExprGuiM.Payload ->
  Widget.Id -> ExprGuiM m (ExpressionGui m)
make (Sugar.GetField recExpr tagG) pl =
  ExpressionGui.stdWrapParentExpr pl $ \myId ->
  let tagId = augmentId "getFieldTag" myId
  in
    ExprGuiM.assignCursor myId tagId $ do
      recExprEdit <- ExprGuiM.makeSubexpression 11 recExpr
      tagEdit <- TagEdit.make tagG tagId
      dotLabel <-
        ExpressionGui.fromValueWidget <$>
        (ExprGuiM.widgetEnv . BWidgets.makeLabel "." . Widget.toAnimId) myId
      return $ ExpressionGui.hbox [recExprEdit, dotLabel, tagEdit]
