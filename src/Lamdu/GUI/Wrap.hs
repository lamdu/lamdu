{-# LANGUAGE DisambiguateRecordFields #-}
module Lamdu.GUI.Wrap
    ( stdWrap
    , stdWrapParentExpr
    , parentDelegator
    ) where

import qualified Control.Lens as Lens
import qualified GUI.Momentu.EventMap as E
import qualified GUI.Momentu.I18N as MomentuTexts
import           GUI.Momentu.Responsive (Responsive(..))
import qualified GUI.Momentu.State as GuiState
import           GUI.Momentu.Widget (Widget)
import qualified GUI.Momentu.Widget as Widget
import           GUI.Momentu.Widget.Id (subId)
import qualified GUI.Momentu.Widgets.FocusDelegator as FocusDelegator
import qualified Lamdu.Config as Config
import qualified Lamdu.GUI.Expr.EventMap as ExprEventMap
import           Lamdu.GUI.Annotation (maybeAddAnnotationPl)
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

stdWrap :: _ => ExprGui.Payload i o -> GuiM env i o (Responsive o) -> GuiM env i o (Responsive o)
stdWrap pl act =
    act
    >>> (takeFocusIfNeeded pl <&> (Widget.widget %~))
    >>> (maybeAddAnnotationPl pl <&> (Widget.widget %~))
    >>> ExprEventMap.add ExprEventMap.defaultOptions pl
    where
        a >>> f = f <*> a

parentDelegator :: _ => Widget.Id -> m (Responsive o -> Responsive o)
parentDelegator myId =
    FocusDelegator.make <*> parentExprFDConfig
    ?? FocusDelegator.FocusEntryChild ?? myId

stdWrapParentExpr :: _ => ExprGui.Payload i o -> GuiM env i o (Responsive o) -> GuiM env i o (Responsive o)
stdWrapParentExpr pl act =
    parentDelegator (WidgetIds.fromExprPayload pl) <*> act & stdWrap pl

takeFocusIfNeeded ::
    Monad i =>
    Sugar.Payload v o ->
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
            pl ^. Sugar.plEntityId : pl ^. Sugar.plHiddenEntityIds
            <&> WidgetIds.fromEntityId
