{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, TypeFamilies #-}
module Lamdu.GUI.ExpressionEdit
    ( make
    ) where

import qualified Data.List as List
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widget.TreeLayout as TreeLayout
import qualified Lamdu.GUI.ExpressionEdit.ApplyEdit as ApplyEdit
import qualified Lamdu.GUI.ExpressionEdit.CaseEdit as CaseEdit
import qualified Lamdu.GUI.ExpressionEdit.GetFieldEdit as GetFieldEdit
import qualified Lamdu.GUI.ExpressionEdit.GetVarEdit as GetVarEdit
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit as HoleEdit
import qualified Lamdu.GUI.ExpressionEdit.InjectEdit as InjectEdit
import qualified Lamdu.GUI.ExpressionEdit.LambdaEdit as LambdaEdit
import qualified Lamdu.GUI.ExpressionEdit.LiteralEdit as LiteralEdit
import qualified Lamdu.GUI.ExpressionEdit.NomEdit as NomEdit
import qualified Lamdu.GUI.ExpressionEdit.RecordEdit as RecordEdit
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import           Lamdu.GUI.ExpressionGui (ExpressionGui)
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.ExpressionGui.Types as ExprGuiT
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Sugar.Names.Types (Name(..))
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

make :: Monad m => ExprGuiT.SugarExpr m -> ExprGuiM m (ExpressionGui m)
make (Sugar.Expression body pl) =
    makeEditor body pl & assignCursor
    where
        exprHiddenEntityIds =
            List.delete (pl ^. Sugar.plEntityId)
            (pl ^. Sugar.plData ^. ExprGuiT.plStoredEntityIds)
        myId = WidgetIds.fromExprPayload pl
        assignCursor x =
            exprHiddenEntityIds <&> WidgetIds.fromEntityId
            & foldr (`ExprGuiM.assignCursorPrefix` const myId) x

injectedExpr ::
    Monad m => Sugar.Payload m ExprGuiT.Payload -> ExprGuiM m (ExpressionGui m)
injectedExpr pl =
    WidgetIds.fromExprPayload pl & Widget.toAnimId
    & ExpressionGui.makeLabel "★" <&> TreeLayout.fromAlignedWidget

makeEditor ::
    Monad m =>
    Sugar.Body (Name m) m (ExprGuiT.SugarExpr m) ->
    Sugar.Payload m ExprGuiT.Payload ->
    ExprGuiM m (ExpressionGui m)
makeEditor body =
    case body of
    Sugar.BodyHole         x -> x & HoleEdit.make
    Sugar.BodyApply        x -> x & ApplyEdit.make
    Sugar.BodyLam          x -> x & LambdaEdit.make
    Sugar.BodyLiteral      x -> x & LiteralEdit.make
    Sugar.BodyRecord       x -> x & RecordEdit.make
    Sugar.BodyCase         x -> x & CaseEdit.make
    Sugar.BodyGetField     x -> x & GetFieldEdit.make
    Sugar.BodyInject       x -> x & InjectEdit.make
    Sugar.BodyGetVar       x -> x & GetVarEdit.make
    Sugar.BodyToNom        x -> x & NomEdit.makeToNom
    Sugar.BodyFromNom      x -> x & NomEdit.makeFromNom
    Sugar.BodyInjectedExpression -> injectedExpr
