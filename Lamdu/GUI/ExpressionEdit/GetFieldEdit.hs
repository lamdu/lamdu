{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
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
import qualified Lamdu.GUI.ExpressionGui.Types as ExprGuiT
import           Lamdu.Sugar.Names.Types (Name(..))
import qualified Lamdu.Sugar.Types as Sugar
import qualified Lamdu.GUI.WidgetIds as WidgetIds

import           Prelude.Compat

make ::
    MonadA m =>
    Sugar.GetField (Name m) (ExprGuiT.SugarExpr m) ->
    Sugar.Payload m ExprGuiT.Payload ->
    ExprGuiM m (ExpressionGui m)
make (Sugar.GetField recExpr tagG) pl =
    ExpressionGui.stdWrapParentExpr pl $ \myId ->
    do
        recExprEdit <-
            ExprGuiM.makeSubexpression (ExpressionGui.precRight .~ 11) recExpr
        dotLabel <- ExpressionGui.makeLabel "." (Widget.toAnimId myId)
        tagEdit <-
            TagEdit.makeRecordTag
            (pl ^. Sugar.plData . ExprGuiT.plNearestHoles) tagG
        return $ ExpressionGui.hbox [recExprEdit, dotLabel, tagEdit]
    & ExprGuiM.assignCursor myId tagId
    where
        tagId = WidgetIds.fromEntityId (tagG ^. Sugar.tagInstance)
