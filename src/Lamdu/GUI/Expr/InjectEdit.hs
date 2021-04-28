module Lamdu.GUI.Expr.InjectEdit
    ( make, makeNullary
    ) where

import qualified Control.Lens as Lens
import           GUI.Momentu ((/|/))
import qualified GUI.Momentu as M
import qualified GUI.Momentu.EventMap as E
import qualified GUI.Momentu.I18N as MomentuTexts
import           GUI.Momentu.Responsive (Responsive)
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.Responsive.Expression as ResponsiveExpr
import qualified GUI.Momentu.State as GuiState
import qualified GUI.Momentu.Widget as Widget
import qualified Lamdu.Config as Config
import qualified Lamdu.GUI.Expr.RecordEdit as RecordEdit
import qualified Lamdu.GUI.Expr.TagEdit as TagEdit
import           Lamdu.GUI.Monad (GuiM)
import           Lamdu.GUI.Styled (text, grammar)
import qualified Lamdu.GUI.Types as ExprGui
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.GUI.Wrap as Wrap
import qualified Lamdu.I18N.Code as Texts
import qualified Lamdu.I18N.CodeUI as Texts
import qualified Lamdu.I18N.Navigation as Texts
import           Lamdu.Name (Name)
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

injectTag :: _ => Sugar.TagRef Name i o -> GuiM env i o (M.WithTextPos (M.Widget o))
injectTag tag = grammar (text ["injectIndicator"] Texts.injectSymbol) /|/ TagEdit.makeVariantTag tag

make :: _ => Annotated (ExprGui.Payload i o) # Const (Sugar.TagRef Name i o) -> GuiM env i o (Responsive o)
make (Ann (Const pl) (Const tag)) =
    maybe (pure id) (ResponsiveExpr.addParens ??) (ExprGui.mParensId pl)
    <*> injectTag tag
    <&> Responsive.fromWithTextPos
    & Wrap.stdWrap pl

data NullaryRecord o
    = HiddenNullaryRecord (E.EventMap (o GuiState.Update)) -- enter it
    | FocusedNullaryRecord (Responsive o)

enterSubexpr :: _ => Widget.Id -> f (E.EventMap (o GuiState.Update))
enterSubexpr myId =
    Lens.view id <&>
    \env ->
    E.keysEventMapMovesCursor
    (env ^. has . Config.enterSubexpressionKeys)
    (E.toDoc env [has . MomentuTexts.navigation, has . Texts.enterSubexpression])
    (pure myId)

leaveSubexpr :: _ => Widget.Id -> f (E.EventMap (o GuiState.Update))
leaveSubexpr myId =
    Lens.view id <&>
    \env ->
    E.keysEventMapMovesCursor
    (env ^. has . Config.leaveSubexpressionKeys)
    (E.toDoc env [has . MomentuTexts.navigation, has . Texts.leaveSubexpression])
    (pure myId)

nullaryRecord ::
    _ =>
    Annotated (ExprGui.Payload i o) # Const (Sugar.TagChoice Name i o Sugar.EntityId) ->
    GuiM env i o (NullaryRecord o)
nullaryRecord x =
    do
        isActive <- GuiState.isSubCursor ?? myId
        if isActive
            then RecordEdit.makeEmpty x <&> FocusedNullaryRecord
            else enterSubexpr myId <&> HiddenNullaryRecord
    where
        myId = x ^. annotation . _1 & WidgetIds.fromExprPayload

makeNullary ::
    _ => Annotated (ExprGui.Payload i o) # Sugar.NullaryInject Name i o -> GuiM env i o (Responsive o)
makeNullary (Ann (Const pl) (Sugar.NullaryInject tag r)) =
    do
        nullary <- nullaryRecord r
        rawInjectEdit <- injectTag tag <&> Responsive.fromWithTextPos

        widgets <-
                case nullary of
                HiddenNullaryRecord enterNullary ->
                    pure [M.weakerEvents enterNullary rawInjectEdit]
                FocusedNullaryRecord w ->
                    leaveSubexpr myId
                    <&> \leaveEventMap ->
                    [rawInjectEdit, Widget.weakerEvents leaveEventMap w]

        valEventMap <-
            case r ^. annotation . _1 . Sugar.plActions . Sugar.delete of
            Sugar.SetToHole a ->
                Lens.view id <&>
                \env ->
                E.keysEventMapMovesCursor (env ^. has . Config.injectValueKeys)
                (E.toDoc env [has . MomentuTexts.edit, has . Texts.injectValue])
                (a <&> WidgetIds.fromEntityId)
            _ -> error "cannot set injected empty record to hole??"
        ResponsiveExpr.boxSpacedMDisamb
            ?? ExprGui.mParensId pl
            ?? widgets
            <&> M.weakerEvents valEventMap
    & GuiState.assignCursor
        myId (tag ^. Sugar.tagRefTag . Sugar.tagInstance & WidgetIds.fromEntityId)
    & Wrap.stdWrap pl
    where
        myId = pl ^. _1 & WidgetIds.fromExprPayload
