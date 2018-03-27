{-# LANGUAGE TypeFamilies #-}
module Lamdu.GUI.ExpressionEdit
    ( make
    ) where

import qualified Control.Monad.Reader as Reader
import qualified Data.List as List
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.State as GuiState
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.TextView as TextView
import qualified Lamdu.GUI.ExpressionEdit.ApplyEdit as ApplyEdit
import qualified Lamdu.GUI.ExpressionEdit.CaseEdit as CaseEdit
import qualified Lamdu.GUI.ExpressionEdit.Dotter as Dotter
import qualified Lamdu.GUI.ExpressionEdit.FragmentEdit as FragmentEdit
import qualified Lamdu.GUI.ExpressionEdit.GetFieldEdit as GetFieldEdit
import qualified Lamdu.GUI.ExpressionEdit.GetVarEdit as GetVarEdit
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit as HoleEdit
import qualified Lamdu.GUI.ExpressionEdit.IfElseEdit as IfElseEdit
import qualified Lamdu.GUI.ExpressionEdit.InjectEdit as InjectEdit
import qualified Lamdu.GUI.ExpressionEdit.LambdaEdit as LambdaEdit
import qualified Lamdu.GUI.ExpressionEdit.LiteralEdit as LiteralEdit
import qualified Lamdu.GUI.ExpressionEdit.NomEdit as NomEdit
import qualified Lamdu.GUI.ExpressionEdit.RecordEdit as RecordEdit
import           Lamdu.GUI.ExpressionGui (ExpressionGui)
import qualified Lamdu.GUI.ExpressionGui as ExprGui
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Name (Name(..))
import qualified Lamdu.Sugar.Types as Sugar
import           Revision.Deltum.Transaction (Transaction)

import           Lamdu.Prelude

type T = Transaction

make :: Monad m => ExprGui.SugarExpr (T m) -> ExprGuiM m (ExpressionGui (T m))
make (Sugar.Expression body pl) =
    makeEditor body pl & assignCursor
    where
        exprHiddenEntityIds =
            List.delete (pl ^. Sugar.plEntityId)
            (pl ^. Sugar.plData . ExprGui.plStoredEntityIds)
        myId = WidgetIds.fromExprPayload pl
        assignCursor x =
            exprHiddenEntityIds <&> WidgetIds.fromEntityId
            & foldr (`GuiState.assignCursorPrefix` const myId) x

placeHolder :: Monad m => Sugar.Payload name (T m) ExprGui.Payload -> ExprGuiM m (ExpressionGui (T m))
placeHolder pl =
    (Widget.makeFocusableView ?? WidgetIds.fromExprPayload pl <&> fmap)
    <*> TextView.makeLabel "★"
    <&> Responsive.fromWithTextPos

makeEditor ::
    Monad m =>
    Sugar.Body (Name (T m)) (T m) (ExprGui.SugarExpr (T m)) ->
    Sugar.Payload (Name (T m)) (T m) ExprGui.Payload ->
    ExprGuiM m (ExpressionGui (T m))
makeEditor body pl =
    case body of
    Sugar.BodyPlaceHolder    -> placeHolder pl
    Sugar.BodyHole         x -> HoleEdit.make         x pl & r
    Sugar.BodyLabeledApply x -> ApplyEdit.makeLabeled x pl & r <&> d
    Sugar.BodySimpleApply  x -> ApplyEdit.makeSimple  x pl & r <&> d
    Sugar.BodyLam          x -> LambdaEdit.make       x pl & r
    Sugar.BodyLiteral      x -> LiteralEdit.make      x pl & r
    Sugar.BodyRecord       x -> RecordEdit.make       x pl & r
    Sugar.BodyCase         x -> CaseEdit.make         x pl & r <&> d
    Sugar.BodyIfElse       x -> IfElseEdit.make       x pl & r <&> d
    Sugar.BodyGetField     x -> GetFieldEdit.make     x pl & r <&> d
    Sugar.BodyInject       x -> InjectEdit.make       x pl & r
    Sugar.BodyGetVar       x -> GetVarEdit.make       x pl & r <&> d
    Sugar.BodyToNom        x -> NomEdit.makeToNom     x pl & r
    Sugar.BodyFromNom      x -> NomEdit.makeFromNom   x pl & r <&> d
    Sugar.BodyFragment     x -> FragmentEdit.make     x pl & r
    where
        d = Dotter.addEventMap myId
        myId = WidgetIds.fromExprPayload pl
        r = Reader.local (Element.animIdPrefix .~ Widget.toAnimId myId)
