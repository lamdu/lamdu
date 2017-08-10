{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.GetFieldEdit
    ( make
    ) where

import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Responsive as Responsive
import qualified Lamdu.GUI.ExpressionEdit.TagEdit as TagEdit
import           Lamdu.GUI.ExpressionGui (ExpressionGui)
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.ExpressionGui.Types as ExprGuiT
import           Lamdu.Sugar.Names.Types (Name(..))
import qualified Lamdu.Sugar.Types as Sugar
import qualified Lamdu.GUI.WidgetIds as WidgetIds

import           Lamdu.Prelude

make ::
    Monad m =>
    Sugar.GetField (Name m) (ExprGuiT.SugarExpr m) ->
    Sugar.Payload m ExprGuiT.Payload ->
    ExprGuiM m (ExpressionGui m)
make (Sugar.GetField recExpr tagG) pl =
    do
        recExprEdit <-
            ExprGuiM.makeSubexpressionWith 0 (ExpressionGui.after .~ 11) recExpr
        dotLabel <- ExpressionGui.grammarLabel "."
        tagEdit <-
            TagEdit.makeRecordTag
            (pl ^. Sugar.plData . ExprGuiT.plNearestHoles) tagG
        ExpressionGui.combine
            [ recExprEdit
            , Responsive.fromTextView dotLabel
            , Responsive.fromWithTextPos tagEdit
            ]
            & return
    & Widget.assignCursor myId tagId
    & ExpressionGui.stdWrapParentExpr pl
    where
        myId = WidgetIds.fromExprPayload pl
        tagId = WidgetIds.fromEntityId (tagG ^. Sugar.tagInstance)
