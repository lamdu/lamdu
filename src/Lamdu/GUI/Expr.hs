module Lamdu.GUI.Expr
    ( make
    ) where

import           GUI.Momentu (Responsive)
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.State as GuiState
import qualified GUI.Momentu.Widget as Widget
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
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

make :: _ => ExprGui.Expr Sugar.Term i o -> GuiM env i o (Responsive o)
make e =
    makeEditor e & assignCursor
    where
        exprHiddenEntityIds = e ^. annotation . Sugar.plHiddenEntityIds
        myId = e ^. annotation & WidgetIds.fromExprPayload
        assignCursor x =
            exprHiddenEntityIds <&> WidgetIds.fromEntityId
            & foldr (`GuiState.assignCursorPrefix` const myId) x

makeEditor :: _ => ExprGui.Expr Sugar.Term i o -> GuiM env i o (Responsive o)
makeEditor (Ann (Const pl) body) =
    case body of
    Sugar.BodyLabeledApply x -> editor pl x ApplyEdit.makeLabeled
    Sugar.BodySimpleApply  x -> editor pl x ApplyEdit.makeSimple
    Sugar.BodyPostfixApply x -> editor pl x ApplyEdit.makePostfix
    Sugar.BodyPostfixFunc  x -> editor pl x ApplyEdit.makePostfixFunc
    Sugar.BodyLam          x -> editor pl x LambdaEdit.make
    Sugar.BodyRecord       x -> editor pl x RecordEdit.make
    Sugar.BodyIfElse       x -> editor pl x IfElseEdit.make
    Sugar.BodyToNom        x -> editor pl x NominalEdit.makeToNom
    Sugar.BodyFragment     x -> editor pl x FragmentEdit.make
    Sugar.BodyNullaryInject x -> editor pl x InjectEdit.makeNullary
    Sugar.BodyLeaf         l ->
        case l of
        Sugar.LeafHole        x -> editor pl (Const x) HoleEdit.make
        Sugar.LeafLiteral     x -> editor pl (Const x) LiteralEdit.make
        Sugar.LeafInject      x -> editor pl (Const x) InjectEdit.make
        Sugar.LeafGetVar      x -> editor pl (Const x) (GetVarEdit.make GetVarEdit.Normal)
    & local
        (Element.animIdPrefix .~ Widget.toAnimId (WidgetIds.fromExprPayload pl))

editor :: a -> h # Annotated a -> (Annotated a # h -> r) -> r
editor pl x f = Ann (Const pl) x & f
