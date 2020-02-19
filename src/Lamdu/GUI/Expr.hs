module Lamdu.GUI.Expr
    ( make
    ) where

import qualified Control.Monad.Reader as Reader
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.Responsive (Responsive)
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.State as GuiState
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Grid as Grid
import qualified GUI.Momentu.Widgets.Label as Label
import qualified GUI.Momentu.Widgets.Menu.Search as SearchMenu
import qualified GUI.Momentu.Widgets.TextEdit as TextEdit
import qualified Lamdu.GUI.Expr.ApplyEdit as ApplyEdit
import qualified Lamdu.GUI.Expr.CaseEdit as CaseEdit
import qualified Lamdu.GUI.Expr.FragmentEdit as FragmentEdit
import qualified Lamdu.GUI.Expr.GetFieldEdit as GetFieldEdit
import qualified Lamdu.GUI.Expr.GetVarEdit as GetVarEdit
import qualified Lamdu.GUI.Expr.HoleEdit as HoleEdit
import qualified Lamdu.GUI.Expr.IfElseEdit as IfElseEdit
import qualified Lamdu.GUI.Expr.InjectEdit as InjectEdit
import qualified Lamdu.GUI.Expr.LambdaEdit as LambdaEdit
import qualified Lamdu.GUI.Expr.LiteralEdit as LiteralEdit
import qualified Lamdu.GUI.Expr.NominalEdit as NominalEdit
import qualified Lamdu.GUI.Expr.RecordEdit as RecordEdit
import           Lamdu.GUI.ExpressionGui.Monad (GuiM)
import qualified Lamdu.GUI.ExpressionGui.Payload as ExprGui
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.I18N.Code as Texts
import qualified Lamdu.I18N.CodeUI as Texts
import qualified Lamdu.I18N.Definitions as Texts
import qualified Lamdu.I18N.Name as Texts
import qualified Lamdu.I18N.Navigation as Texts
import           Lamdu.Name (Name(..))
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

make ::
    ( Monad i, Monad o
    , Grid.HasTexts env
    , Has (Texts.Code Text) env
    , Has (Texts.CodeUI Text) env
    , Has (Texts.Definitions Text) env
    , Has (Texts.Name Text) env
    , Has (Texts.Navigation Text) env
    , TextEdit.HasTexts env
    , SearchMenu.HasTexts env
    ) =>
    ExprGui.SugarExpr i o -> GuiM env i o (Responsive o)
make (Ann (Const pl) body) =
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
    GuiM env i o (Responsive o)
placeHolder pl =
    (Widget.makeFocusableView ?? WidgetIds.fromExprPayload pl <&> fmap)
    <*> Label.make "★"
    <&> Responsive.fromWithTextPos

makeEditor ::
    ( Monad i, Monad o
    , Grid.HasTexts env
    , Has (Texts.Code Text) env
    , Has (Texts.CodeUI Text) env
    , Has (Texts.Definitions Text) env
    , Has (Texts.Name Text) env
    , Has (Texts.Navigation Text) env
    , TextEdit.HasTexts env
    , SearchMenu.HasTexts env
    ) =>
    Sugar.Body Name i o # Annotated (Sugar.Payload Name i o ExprGui.Payload) ->
    Sugar.Payload Name i o ExprGui.Payload ->
    GuiM env i o (Responsive o)
makeEditor body pl =
    case body of
    Sugar.BodyPlaceHolder    -> placeHolder pl
    Sugar.BodyHole         x -> HoleEdit.make         x pl
    Sugar.BodyLabeledApply x -> ApplyEdit.makeLabeled x pl
    Sugar.BodySimpleApply  x -> ApplyEdit.makeSimple  x pl
    Sugar.BodyLam          x -> LambdaEdit.make       x pl
    Sugar.BodyLiteral      x -> LiteralEdit.make      x pl
    Sugar.BodyRecord       x -> RecordEdit.make       x pl
    Sugar.BodyCase         x -> CaseEdit.make         x pl
    Sugar.BodyIfElse       x -> IfElseEdit.make       x pl
    Sugar.BodyGetField     x -> GetFieldEdit.make     x pl
    Sugar.BodyInject       x -> InjectEdit.make       x pl
    Sugar.BodyGetVar       x -> GetVarEdit.make       x pl
    Sugar.BodyToNom        x -> NominalEdit.makeToNom x pl
    Sugar.BodyFromNom      x -> NominalEdit.makeFromNom x pl
    Sugar.BodyFragment     x -> FragmentEdit.make     x pl
    & Reader.local
        (Element.animIdPrefix .~ Widget.toAnimId (WidgetIds.fromExprPayload pl))
