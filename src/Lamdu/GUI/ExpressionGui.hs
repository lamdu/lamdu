{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, RankNTypes, TypeFamilies, LambdaCase, DeriveTraversable , FlexibleContexts, DisambiguateRecordFields #-}
module Lamdu.GUI.ExpressionGui
    ( ExpressionGui
    , render

    , stdWrap
    , parentDelegator
    , stdWrapParentExpr
    ) where

import qualified Control.Lens as Lens
import           Data.Store.Transaction (Transaction)
import           GUI.Momentu.Align (WithTextPos(..))
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.Responsive (Responsive(..))
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.State as GuiState
import           GUI.Momentu.Widget (Widget)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.FocusDelegator as FocusDelegator
import           Lamdu.Config (Config)
import qualified Lamdu.Config as Config
import qualified Lamdu.GUI.ExpressionEdit.EventMap as ExprEventMap
import           Lamdu.GUI.ExpressionGui.Annotation (maybeAddAnnotationPl)
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import           Lamdu.GUI.ExpressionGui.Types (ExpressionGui)
import qualified Lamdu.GUI.ExpressionGui.Types as ExprGuiT
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

type T = Transaction

parentExprFDConfig :: Config -> FocusDelegator.Config
parentExprFDConfig config = FocusDelegator.Config
    { FocusDelegator.focusChildKeys = Config.enterSubexpressionKeys config
    , FocusDelegator.focusChildDoc = E.Doc ["Navigation", "Enter subexpression"]
    , FocusDelegator.focusParentKeys = Config.leaveSubexpressionKeys config
    , FocusDelegator.focusParentDoc = E.Doc ["Navigation", "Leave subexpression"]
    }

stdWrap ::
    Monad m =>
    Sugar.Payload (T m) ExprGuiT.Payload ->
    ExprGuiM m (ExpressionGui m) ->
    ExprGuiM m (ExpressionGui m)
stdWrap pl act =
    do
        (res, holePicker) <- ExprGuiM.listenResultPicker act
        exprEventMap <- ExprEventMap.make pl holePicker
        maybeAddAnnotationPl pl ?? res
            <&> E.weakerEvents exprEventMap

parentDelegator ::
    ( MonadReader env m, Config.HasConfig env, GuiState.HasCursor env, Applicative f
    ) => Widget.Id ->
    m (Responsive (f GuiState.Update) -> Responsive (f GuiState.Update))
parentDelegator myId =
    FocusDelegator.make <*> (Lens.view Config.config <&> parentExprFDConfig)
    ?? FocusDelegator.FocusEntryChild ?? myId

stdWrapParentExpr ::
    Monad m =>
    Sugar.Payload (T m) ExprGuiT.Payload ->
    ExprGuiM m (ExpressionGui m) ->
    ExprGuiM m (ExpressionGui m)
stdWrapParentExpr pl mkGui =
    parentDelegator (WidgetIds.fromExprPayload pl) <*> mkGui
    & stdWrap pl

render :: Widget.R -> Responsive a -> WithTextPos (Widget a)
render width gui =
    (gui ^. Responsive.render)
    Responsive.LayoutParams
    { _layoutMode = Responsive.LayoutNarrow width
    , _layoutContext = Responsive.LayoutClear
    }
