module Lamdu.GUI.Expr.InjectEdit
    ( make, makeNullary
    ) where

import qualified Control.Lens as Lens
import qualified GUI.Momentu as M
import qualified GUI.Momentu.EventMap as E
import qualified GUI.Momentu.I18N as MomentuTexts
import           GUI.Momentu.Responsive (Responsive)
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.Responsive.Expression as ResponsiveExpr
import qualified GUI.Momentu.State as GuiState
import qualified Lamdu.Config as Config
import qualified Lamdu.GUI.Expr.RecordEdit as RecordEdit
import qualified Lamdu.GUI.Expr.TagEdit as TagEdit
import           Lamdu.GUI.Monad (GuiM)
import           Lamdu.GUI.Styled (text, grammar)
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.GUI.Wrap (stdWrap)
import qualified Lamdu.GUI.Types as ExprGui
import qualified Lamdu.I18N.Code as Texts
import qualified Lamdu.I18N.Navigation as Texts
import           Lamdu.Name (Name)
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

injectTag :: _ => Sugar.TagRef Name i o -> GuiM env i o (M.WithTextPos (M.Widget o))
injectTag tag = grammar (text ["injectIndicator"] Texts.injectSymbol) M./|/ TagEdit.makeVariantTag tag

make :: _ => Annotated (ExprGui.Payload i o) # Const (Sugar.TagRef Name i o) -> GuiM env i o (Responsive o)
make (Ann (Const pl) (Const tag)) =
    maybe (pure id) (ResponsiveExpr.addParens ??) (ExprGui.mParensId pl)
    <*> injectTag tag
    <&> Responsive.fromWithTextPos
    & stdWrap pl

nullaryRecord ::
    _ =>
    Annotated (ExprGui.Payload i o) # Const (Sugar.TagChoice Name i o Sugar.EntityId) ->
    GuiM env i o (Either _ (Responsive o))
nullaryRecord x =
    do
        isActive <- GuiState.isSubCursor ?? myId
        if isActive
            then RecordEdit.makeEmpty x <&> Right
            else
                Lens.view id <&>
                \env ->
                E.keysEventMapMovesCursor
                (env ^. has . Config.enterSubexpressionKeys)
                (E.toDoc env [has . MomentuTexts.navigation, has . Texts.enterSubexpression])
                (pure myId)
                & Left
    where
        myId = x ^. annotation . _1 & WidgetIds.fromExprPayload

makeNullary ::
    _ => Annotated (ExprGui.Payload i o) # Sugar.NullaryInject Name i o -> GuiM env i o (Responsive o)
makeNullary (Ann (Const pl) (Sugar.NullaryInject tag r)) =
    do
        nullary <- nullaryRecord r
        t <-
            injectTag tag <&> Responsive.fromWithTextPos
            <&> either M.weakerEvents (const id) nullary
        ResponsiveExpr.boxSpacedMDisamb ?? ExprGui.mParensId pl ?? (t : nullary ^.. Lens._Right)
    & stdWrap pl
