{-# LANGUAGE TypeFamilies #-}
module Lamdu.GUI.ExpressionEdit
    ( make
    ) where

import qualified Control.Monad.Reader as Reader
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

import           Lamdu.Prelude

make ::
    (Monad i, Monad o) =>
    ExprGui.SugarExpr i o -> ExprGuiM i o (ExpressionGui o)
make (Sugar.Expression pl body) =
    makeEditor body pl & assignCursor
    where
        exprHiddenEntityIds = pl ^. Sugar.plData . ExprGui.plHiddenEntityIds
        myId = WidgetIds.fromExprPayload pl
        assignCursor x =
            exprHiddenEntityIds <&> WidgetIds.fromEntityId
            & foldr (`GuiState.assignCursorPrefix` const myId) x

placeHolder ::
    (Monad i, Applicative o) =>
    Sugar.Payload name i o ExprGui.Payload ->
    ExprGuiM i o (ExpressionGui o)
placeHolder pl =
    (Widget.makeFocusableView ?? WidgetIds.fromExprPayload pl <&> fmap)
    <*> TextView.makeLabel "★"
    <&> Responsive.fromWithTextPos

makeEditor ::
    (Monad i, Monad o) =>
    Sugar.Body (Name o) i o (Sugar.Payload (Name o) i o ExprGui.Payload) ->
    Sugar.Payload (Name o) i o ExprGui.Payload ->
    ExprGuiM i o (ExpressionGui o)
makeEditor body pl =
    case body of
    Sugar.BodyPlaceHolder    -> placeHolder pl
    Sugar.BodyHole         x -> HoleEdit.make         x pl
    Sugar.BodyLabeledApply x -> ApplyEdit.makeLabeled x pl <&> d
    Sugar.BodySimpleApply  x -> ApplyEdit.makeSimple  x pl <&> d
    Sugar.BodyLam          x -> LambdaEdit.make       x pl
    Sugar.BodyLiteral      x -> LiteralEdit.make      x pl
    Sugar.BodyRecord       x -> RecordEdit.make       x pl
    Sugar.BodyCase         x -> CaseEdit.make         x pl <&> d
    Sugar.BodyIfElse       x -> IfElseEdit.make       x pl <&> d
    Sugar.BodyGetField     x -> GetFieldEdit.make     x pl <&> d
    Sugar.BodyInject       x -> InjectEdit.make       x pl
    Sugar.BodyGetVar       x -> GetVarEdit.make       x pl <&> d
    Sugar.BodyToNom        x -> NomEdit.makeToNom     x pl
    Sugar.BodyFromNom      x -> NomEdit.makeFromNom   x pl <&> d
    Sugar.BodyFragment     x -> FragmentEdit.make     x pl
    & Reader.local (Element.animIdPrefix .~ Widget.toAnimId myId)
    where
        d = Dotter.addEventMap myId
        myId = WidgetIds.fromExprPayload pl
