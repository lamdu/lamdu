{-# LANGUAGE DisambiguateRecordFields #-}
module Lamdu.GUI.ExpressionGui.Wrap
    ( stdWrap
    , stdWrapParentExpr
    ) where

import           Control.Applicative (liftA2)
import qualified Control.Lens as Lens
import qualified GUI.Momentu.EventMap as E
import qualified GUI.Momentu.I18N as MomentuTexts
import           GUI.Momentu.Responsive (Responsive(..))
import qualified GUI.Momentu.State as GuiState
import           GUI.Momentu.Widget (Widget)
import qualified GUI.Momentu.Widget as Widget
import           GUI.Momentu.Widget.Id (subId)
import qualified GUI.Momentu.Widgets.FocusDelegator as FocusDelegator
import qualified GUI.Momentu.Widgets.Grid as Grid
import           Lamdu.Config (Config)
import qualified Lamdu.Config as Config
import qualified Lamdu.GUI.Expr.Dotter as Dotter
import qualified Lamdu.GUI.Expr.EventMap as ExprEventMap
import           Lamdu.GUI.ExpressionGui.Annotation (maybeAddAnnotationPl)
import           Lamdu.GUI.ExpressionGui.Monad (GuiM)
import qualified Lamdu.GUI.ExpressionGui.Payload as ExprGui
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.I18N.Code as Texts
import qualified Lamdu.I18N.CodeUI as Texts
import qualified Lamdu.I18N.Definitions as Texts
import qualified Lamdu.I18N.Name as Texts
import qualified Lamdu.I18N.Navigation as Texts
import           Lamdu.Name (Name)
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

parentExprFDConfig ::
    ( MonadReader env m, Has Config env
    , Has (MomentuTexts.Texts Text) env
    , Has (Texts.Navigation Text) env
    ) =>
    m FocusDelegator.Config
parentExprFDConfig =
    Lens.view id <&>
    \env ->
    let doc lens =
            E.toDoc env
            [has . MomentuTexts.navigation, has . lens]
    in
    FocusDelegator.Config
    { FocusDelegator.focusChildKeys = env ^. has . Config.enterSubexpressionKeys
    , FocusDelegator.focusChildDoc = doc Texts.enterSubexpression
    , FocusDelegator.focusParentKeys = env ^. has . Config.leaveSubexpressionKeys
    , FocusDelegator.focusParentDoc = doc Texts.leaveSubexpression
    }

stdWrap ::
    ( Monad i, Monad o
    , Has (Texts.Name Text) env
    , Has (Texts.Code Text) env
    , Has (Texts.CodeUI Text) env
    , Has (Texts.Definitions Text) env
    , Grid.HasTexts env
    ) =>
    Sugar.Payload Name i o ExprGui.Payload ->
    GuiM env i o
    (Responsive o -> Responsive o)
stdWrap pl =
    (takeFocusIfNeeded pl <&> (Widget.widget %~))
    <<< (maybeAddAnnotationPl pl <&> (Widget.widget %~))
    <<< Dotter.with pl
    <<< ExprEventMap.add ExprEventMap.defaultOptions pl
    where
        (<<<) = liftA2 (.)

parentDelegator ::
    ( HasCallStack, MonadReader env m, Has Config env
    , Has (MomentuTexts.Texts Text) env
    , Has (Texts.Navigation Text) env
    , GuiState.HasCursor env
    , Applicative o
    ) => Widget.Id ->
    m (Responsive o -> Responsive o)
parentDelegator myId =
    FocusDelegator.make <*> parentExprFDConfig
    ?? FocusDelegator.FocusEntryChild ?? myId

stdWrapParentExpr ::
    ( Monad i, Monad o
    , Grid.HasTexts env
    , Has (Texts.Code Text) env
    , Has (Texts.CodeUI Text) env
    , Has (Texts.Definitions Text) env
    , Has (Texts.Name Text) env
    , Has (Texts.Navigation Text) env
    ) =>
    Sugar.Payload Name i o ExprGui.Payload ->
    GuiM env i o (Responsive o -> Responsive o)
stdWrapParentExpr pl =
    (.)
    <$> stdWrap pl
    <*> parentDelegator (WidgetIds.fromExprPayload pl)

takeFocusIfNeeded ::
    Monad i =>
    Sugar.Payload name i o ExprGui.Payload ->
    GuiM env i o (Widget o -> Widget o)
takeFocusIfNeeded pl =
    Lens.view GuiState.cursor
    <&>
    \cursor widget ->
    if not (Widget.isFocused widget)
        && any (Lens.has Lens._Just . (`subId` cursor)) entityWidgetIds
    then Widget.setFocused widget
    else widget
    where
        entityWidgetIds =
            pl ^. Sugar.plEntityId : pl ^. Sugar.plData . ExprGui.plHiddenEntityIds
            <&> WidgetIds.fromEntityId
