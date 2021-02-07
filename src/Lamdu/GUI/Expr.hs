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
import qualified Lamdu.GUI.Expr.FragmentEdit as FragmentEdit
import qualified Lamdu.GUI.Expr.GetVarEdit as GetVarEdit
import qualified Lamdu.GUI.Expr.HoleEdit as HoleEdit
import qualified Lamdu.GUI.Expr.IfElseEdit as IfElseEdit
import qualified Lamdu.GUI.Expr.InjectEdit as InjectEdit
import qualified Lamdu.GUI.Expr.LambdaEdit as LambdaEdit
import qualified Lamdu.GUI.Expr.LiteralEdit as LiteralEdit
import qualified Lamdu.GUI.Expr.NominalEdit as NominalEdit
import qualified Lamdu.GUI.Expr.RecordEdit as RecordEdit
import           Lamdu.GUI.Monad (GuiM)
import qualified Lamdu.GUI.Types as ExprGui
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.I18N.Code as Texts
import qualified Lamdu.I18N.CodeUI as Texts
import qualified Lamdu.I18N.Definitions as Texts
import qualified Lamdu.I18N.Name as Texts
import qualified Lamdu.I18N.Navigation as Texts
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
    ExprGui.Expr Sugar.Term i o -> GuiM env i o (Responsive o)
make e =
    makeEditor e & assignCursor
    where
        exprHiddenEntityIds = e ^. annotation . _2 . ExprGui.plHiddenEntityIds
        myId = e ^. annotation . _1 & WidgetIds.fromExprPayload
        assignCursor x =
            exprHiddenEntityIds <&> WidgetIds.fromEntityId
            & foldr (`GuiState.assignCursorPrefix` const myId) x

placeHolder ::
    (Monad i, Applicative o) =>
    Sugar.Payload v name i o ->
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
    ExprGui.Expr Sugar.Term i o -> GuiM env i o (Responsive o)
makeEditor (Ann (Const pl) body) =
    case body of
    Sugar.BodyPlaceHolder    -> placeHolder (pl ^. _1)
    Sugar.BodyHole         x -> editor pl (Const x) HoleEdit.make
    Sugar.BodyLabeledApply x -> editor pl x ApplyEdit.makeLabeled
    Sugar.BodySimpleApply  x -> editor pl x ApplyEdit.makeSimple
    Sugar.BodyPostfixApply x -> editor pl x ApplyEdit.makePostfix
    Sugar.BodyPostfixFunc  x -> editor pl x ApplyEdit.makePostfixFunc
    Sugar.BodyLam          x -> editor pl x LambdaEdit.make
    Sugar.BodyLiteral      x -> editor pl (Const x) LiteralEdit.make
    Sugar.BodyRecord       x -> editor pl x RecordEdit.make
    Sugar.BodyIfElse       x -> editor pl x IfElseEdit.make
    Sugar.BodyInject       x -> editor pl (Const x) InjectEdit.make
    Sugar.BodyEmptyInject  x -> editor pl (Const x) InjectEdit.make
    Sugar.BodyGetVar       x -> editor pl (Const x) GetVarEdit.make
    Sugar.BodyToNom        x -> editor pl x NominalEdit.makeToNom
    Sugar.BodyFragment     x -> editor pl x FragmentEdit.make
    & Reader.local
        (Element.animIdPrefix .~ Widget.toAnimId (WidgetIds.fromExprPayload (pl ^. _1)))

editor :: a -> h # Annotated a -> (Annotated a # h -> r) -> r
editor pl x f = Ann (Const pl) x & f
