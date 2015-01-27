{-# LANGUAGE OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.GetFieldEdit
  ( make
  ) where

import           Control.Lens.Operators
import           Control.MonadA (MonadA)
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Lamdu.GUI.ExpressionEdit.TagEdit as TagEdit
import           Lamdu.GUI.ExpressionGui (ExpressionGui)
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import           Lamdu.Sugar.AddNames.Types (Name(..))
import qualified Lamdu.Sugar.Types as Sugar
import qualified Lamdu.GUI.WidgetIds as WidgetIds

make ::
  MonadA m =>
  Sugar.GetField (Name m) (ExprGuiM.SugarExpr m) ->
  Sugar.Payload m ExprGuiM.Payload ->
  ExprGuiM m (ExpressionGui m)
make (Sugar.GetField recExpr tagG) pl =
  ($ WidgetIds.fromExprPayload pl) $
  ExpressionGui.stdWrapParentExpr pl $ \myId ->
  let tagId = WidgetIds.fromEntityId (tagG ^. Sugar.tagInstance)
  in
    ExprGuiM.assignCursor myId tagId $ do
      recExprEdit <- ExprGuiM.makeSubexpression 11 recExpr
      tagEdit <-
        TagEdit.makeRecordTag (pl ^. Sugar.plData . ExprGuiM.plNearestHoles) tagG
      dotLabel <- ExpressionGui.makeLabel "." (Widget.toAnimId myId)
      return $ ExpressionGui.hbox [recExprEdit, dotLabel, tagEdit]
