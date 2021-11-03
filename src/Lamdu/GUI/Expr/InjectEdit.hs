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
import qualified Lamdu.Config.Theme.TextColors as TextColors
import qualified Lamdu.GUI.Expr.EventMap as EventMap
import qualified Lamdu.GUI.Expr.RecordEdit as RecordEdit
import           Lamdu.GUI.Monad (GuiM)
import           Lamdu.GUI.Styled (text, grammar, withColor)
import qualified Lamdu.GUI.TagView as TagView
import qualified Lamdu.GUI.Types as ExprGui
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.GUI.Wrap as Wrap
import qualified Lamdu.I18N.Code as Texts
import qualified Lamdu.I18N.CodeUI as Texts
import qualified Lamdu.I18N.Navigation as Texts
import           Lamdu.Name (Name)
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

injectTag :: _ => Sugar.TagRef Name i o -> GuiM env i o (M.WithTextPos M.View)
injectTag tag =
    grammar (text ["injectIndicator"] Texts.injectSymbol) /|/
    withColor TextColors.caseTagColor (TagView.make (tag ^. Sugar.tagRefTag))

make :: _ => Annotated (ExprGui.Payload i o) # Const (Sugar.TagRef Name i o) -> GuiM env i o (Responsive o)
make (Ann (Const pl) (Const tag)) =
    (Widget.makeFocusableWidget ?? myId <&> (Widget.widget %~)) <*>
    ( maybe (pure id) (ResponsiveExpr.addParens ??) (ExprGui.mParensId pl)
        <*> (injectTag tag <&> Lens.mapped %~ Widget.fromView)
        <&> Responsive.fromWithTextPos
    )
    & Wrap.stdWrap pl
    where
        myId = WidgetIds.fromExprPayload pl

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
    Annotated (ExprGui.Payload i o) # Const (Sugar.TagChoice Name i o) ->
    GuiM env i o (NullaryRecord o)
nullaryRecord x =
    do
        isActive <- GuiState.isSubCursor ?? myId
        if isActive
            then RecordEdit.makeEmpty x <&> FocusedNullaryRecord
            else enterSubexpr myId <&> HiddenNullaryRecord
    where
        myId = x ^. annotation & WidgetIds.fromExprPayload

makeNullary ::
    _ => Annotated (ExprGui.Payload i o) # Sugar.NullaryInject Name i o -> GuiM env i o (Responsive o)
makeNullary (Ann (Const pl) (Sugar.NullaryInject tag r)) =
    do
        makeFocusable <- Widget.makeFocusableView
        nullary <- nullaryRecord r
        parenEvent <- EventMap.parenKeysEvent
        rawInjectEdit <-
            injectTag (tag ^. hVal . Lens._Wrapped) <&> M.tValue %~ makeFocusable myId <&> Responsive.fromWithTextPos
            <&>
            case tag ^. annotation . Sugar.plActions . Sugar.mReplaceParent of
            Nothing -> id
            Just act ->
                M.weakerEvents (parenEvent [has . MomentuTexts.edit, has . Texts.injectSection]
                (act <&> GuiState.updateCursor . WidgetIds.fromEntityId))
        widgets <-
                case nullary of
                HiddenNullaryRecord enterNullary ->
                    pure
                    [ rawInjectEdit & Widget.widget %~ M.weakerEvents enterNullary
                    ]
                FocusedNullaryRecord w ->
                    leaveSubexpr myId
                    <&> \leaveEventMap ->
                    [rawInjectEdit, Widget.weakerEvents leaveEventMap w]

        valEventMap <-
            case r ^. annotation . Sugar.plActions . Sugar.delete of
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
    & Wrap.stdWrap pl
    where
        myId = WidgetIds.fromExprPayload pl
