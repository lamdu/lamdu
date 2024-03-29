module Lamdu.GUI.Expr.InjectEdit
    ( make, makeNullary
    ) where

import qualified Control.Lens as Lens
import           GUI.Momentu (Responsive, (/|/), Update)
import qualified GUI.Momentu as M
import           GUI.Momentu.Element.Id (ElemId)
import qualified GUI.Momentu.EventMap as E
import qualified GUI.Momentu.I18N as MomentuTexts
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.Responsive.Expression as ResponsiveExpr
import qualified GUI.Momentu.State as GuiState
import qualified GUI.Momentu.Widget as Widget
import           Hyper (annValue)
import qualified Lamdu.Config as Config
import qualified Lamdu.Config.Theme.TextColors as TextColors
import qualified Lamdu.GUI.Expr.EventMap as EventMap
import qualified Lamdu.GUI.Expr.RecordEdit as RecordEdit
import qualified Lamdu.GUI.Expr.TagEdit as TagEdit
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
    grammar (text "injectIndicator" Texts.injectSymbol) /|/
    withColor TextColors.caseTagColor (TagView.make (tag ^. Sugar.tagRefTag))

make :: _ => Annotated (ExprGui.Payload i o) # Const (Sugar.TagRef Name i o) -> GuiM env i o (Responsive o)
make (Ann (Const pl) (Const tag)) =
    do
        jumpToTag <- foldMap TagEdit.makeJumpToTagEventMap (tag ^. Sugar.tagRefJumpTo)
        injectTag tag <&> Lens.mapped %~ Widget.fromView
            >>= maybe pure ResponsiveExpr.addParens (ExprGui.mParensId pl)
            >>= M.tValue (Widget.makeFocusableWidget myId)
            <&> Responsive.fromWithTextPos
            <&> M.weakerEvents jumpToTag
    & Wrap.stdWrap pl
    where
        myId = WidgetIds.fromExprPayload pl

data NullaryRecord o
    = HiddenNullaryRecord (E.EventMap (o Update)) -- enter it
    | FocusedNullaryRecord (Responsive o)

enterSubexpr :: _ => ElemId -> f (E.EventMap (o Update))
enterSubexpr myId =
    Lens.view id <&>
    \env ->
    E.keysEventMapMovesCursor
    (env ^. has . Config.enterSubexpressionKeys)
    (E.toDoc env [has . MomentuTexts.navigation, has . Texts.enterSubexpression])
    (pure myId)

leaveSubexpr :: _ => ElemId -> f (E.EventMap (o Update))
leaveSubexpr myId =
    Lens.view id <&>
    \env ->
    E.keysEventMapMovesCursor
    (env ^. has . Config.leaveSubexpressionKeys)
    (E.toDoc env [has . MomentuTexts.navigation, has . Texts.leaveSubexpression])
    (pure myId)

nullaryRecord ::
    _ =>
    Annotated (ExprGui.Payload i o) # Const (i (Sugar.TagChoice Name o)) ->
    GuiM env i o (NullaryRecord o)
nullaryRecord x =
    do
        isActive <- GuiState.isSubCursor myId
        if isActive
            then
                RecordEdit.make
                (x & annValue %~ \(Const add) ->
                    Sugar.Composite (Sugar.TaggedList add Nothing) []
                    -- Hack: Create dummy unnecessary closed-composite tail
                    (Sugar.ClosedCompositeTail
                        (Sugar.ClosedCompositeActions (pure (x ^. annotation . Sugar.plEntityId))))
                ) <&> FocusedNullaryRecord
            else enterSubexpr myId <&> HiddenNullaryRecord
    where
        myId = x ^. annotation & WidgetIds.fromExprPayload

makeNullary ::
    _ => Annotated (ExprGui.Payload i o) # Sugar.NullaryInject Name i o -> GuiM env i o (Responsive o)
makeNullary (Ann (Const pl) (Sugar.NullaryInject tag r)) =
    do
        nullary <- nullaryRecord r
        parenEvent <- EventMap.parenKeysEvent
        rawInjectEdit <-
            injectTag (tag ^. hVal . Lens._Wrapped) >>= M.tValue (Widget.makeFocusableView myId)
            <&> Responsive.fromWithTextPos
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

        jumpToTag <-
            foldMap TagEdit.makeJumpToTagEventMap (tag ^. hVal . Lens._Wrapped . Sugar.tagRefJumpTo)
        valEventMap <-
            case r ^. annotation . Sugar.plActions . Sugar.delete of
            Sugar.SetToHole a ->
                Lens.view id <&>
                \env ->
                E.keysEventMapMovesCursor (env ^. has . Config.injectValueKeys)
                (E.toDoc env [has . MomentuTexts.edit, has . Texts.injectValue])
                (a <&> WidgetIds.fromEntityId)
            _ -> error "cannot set injected empty record to hole??"
        ResponsiveExpr.boxSpacedMDisamb (ExprGui.mParensId pl) widgets
            <&> M.weakerEvents (jumpToTag <> valEventMap)
    & Wrap.stdWrap pl
    where
        myId = WidgetIds.fromExprPayload pl
