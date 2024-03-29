{-# LANGUAGE DisambiguateRecordFields #-}
module Lamdu.GUI.Wrap
    ( stdWrap
    , stdWrapParentExpr
    , parentDelegator
    , wrapWithoutEvents
    ) where

import qualified Control.Lens as Lens
import           GUI.Momentu (Widget, Responsive, ElemId)
import           GUI.Momentu.Element.Id (subId)
import qualified GUI.Momentu.EventMap as E
import qualified GUI.Momentu.I18N as MomentuTexts
import qualified GUI.Momentu.State as GuiState
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.FocusDelegator as FocusDelegator
import qualified Lamdu.Config as Config
import           Lamdu.GUI.Annotation (maybeAddAnnotationPl)
import qualified Lamdu.GUI.Expr.EventMap as ExprEventMap
import           Lamdu.GUI.Monad (GuiM)
import qualified Lamdu.GUI.Types as ExprGui
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.I18N.Navigation as Texts
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

parentExprFDConfig :: _ => m FocusDelegator.Config
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

wrapWithoutEvents ::
    _ => ExprGui.Payload i o -> GuiM env i o (Responsive o) -> GuiM env i o (Responsive o)
wrapWithoutEvents pl act =
    act
    >>> (takeFocusIfNeeded pl <&> (Widget.widget %~))
    >>> (maybeAddAnnotationPl pl <&> (Widget.widget %~))
    where
        a >>> f = f <*> a

stdWrap :: _ => ExprGui.Payload i o -> GuiM env i o (Responsive o) -> GuiM env i o (Responsive o)
stdWrap pl act =
    wrapWithoutEvents pl act
    >>= ExprEventMap.add ExprEventMap.defaultOptions pl

parentDelegator :: _ => ElemId -> Responsive o -> m (Responsive o)
parentDelegator myId w =
    do
        conf <- parentExprFDConfig
        FocusDelegator.make conf FocusDelegator.FocusEntryChild myId w

stdWrapParentExpr :: _ => ExprGui.Payload i o -> GuiM env i o (Responsive o) -> GuiM env i o (Responsive o)
stdWrapParentExpr pl act =
    act >>= parentDelegator (WidgetIds.fromExprPayload pl) & stdWrap pl

takeFocusIfNeeded :: _ => Sugar.Payload v o -> GuiM env i o (Widget o -> Widget o)
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
            pl ^. Sugar.plEntityId : pl ^. Sugar.plHiddenEntityIds
            <&> WidgetIds.fromEntityId
