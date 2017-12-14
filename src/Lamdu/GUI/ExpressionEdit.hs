{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, TypeFamilies #-}
module Lamdu.GUI.ExpressionEdit
    ( make
    ) where

import qualified Control.Monad.Reader as Reader
import qualified Data.List as List
import           Data.Store.Transaction (Transaction)
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.State as GuiState
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.TextView as TextView
import qualified Lamdu.GUI.ExpressionEdit.ApplyEdit as ApplyEdit
import qualified Lamdu.GUI.ExpressionEdit.CaseEdit as CaseEdit
import qualified Lamdu.GUI.ExpressionEdit.GetFieldEdit as GetFieldEdit
import qualified Lamdu.GUI.ExpressionEdit.GetVarEdit as GetVarEdit
import qualified Lamdu.GUI.ExpressionEdit.GuardEdit as GuardEdit
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit as HoleEdit
import qualified Lamdu.GUI.ExpressionEdit.InjectEdit as InjectEdit
import qualified Lamdu.GUI.ExpressionEdit.LambdaEdit as LambdaEdit
import qualified Lamdu.GUI.ExpressionEdit.LiteralEdit as LiteralEdit
import qualified Lamdu.GUI.ExpressionEdit.NomEdit as NomEdit
import qualified Lamdu.GUI.ExpressionEdit.RecordEdit as RecordEdit
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import           Lamdu.GUI.ExpressionGui (ExpressionGui)
import qualified Lamdu.GUI.ExpressionGui as ExprGui
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Name (Name(..))
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

type T = Transaction

make :: Monad m => ExprGui.SugarExpr m -> ExprGuiM m (ExpressionGui m)
make (Sugar.Expression body pl) =
    makeEditor body pl & assignCursor
    where
        exprHiddenEntityIds =
            List.delete (pl ^. Sugar.plEntityId)
            (pl ^. Sugar.plData ^. ExprGui.plStoredEntityIds)
        myId = WidgetIds.fromExprPayload pl
        assignCursor x =
            exprHiddenEntityIds <&> WidgetIds.fromEntityId
            & foldr (`GuiState.assignCursorPrefix` const myId) x

injectedExpr :: Monad m => Sugar.Payload (T m) ExprGui.Payload -> ExprGuiM m (ExpressionGui m)
injectedExpr pl =
    (Widget.makeFocusableView ?? WidgetIds.fromExprPayload pl <&> fmap)
    <*> TextView.makeLabel "★"
    <&> Responsive.fromWithTextPos

makeEditor ::
    Monad m =>
    Sugar.Body (Name (T m)) (T m) (ExprGui.SugarExpr m) ->
    Sugar.Payload (T m) ExprGui.Payload ->
    ExprGuiM m (ExpressionGui m)
makeEditor body pl =
    case body of
    Sugar.BodyInjectedExpression -> injectedExpr pl
    Sugar.BodyHole         x -> HoleEdit.make         x pl & r
    Sugar.BodyLabeledApply x -> ApplyEdit.makeLabeled x pl & r
    Sugar.BodySimpleApply  x -> ApplyEdit.makeSimple  x pl & r
    Sugar.BodyLam          x -> LambdaEdit.make       x pl & r
    Sugar.BodyLiteral      x -> LiteralEdit.make      x pl & r
    Sugar.BodyRecord       x -> RecordEdit.make       x pl & r
    Sugar.BodyCase         x -> CaseEdit.make         x pl & r
    Sugar.BodyGuard        x -> GuardEdit.make        x pl & r
    Sugar.BodyGetField     x -> GetFieldEdit.make     x pl & r
    Sugar.BodyInject       x -> InjectEdit.make       x pl & r
    Sugar.BodyGetVar       x -> GetVarEdit.make       x pl & r
    Sugar.BodyToNom        x -> NomEdit.makeToNom     x pl & r
    Sugar.BodyFromNom      x -> NomEdit.makeFromNom   x pl & r
    where
        r = Reader.local (Element.animIdPrefix .~ Widget.toAnimId (WidgetIds.fromExprPayload pl))
