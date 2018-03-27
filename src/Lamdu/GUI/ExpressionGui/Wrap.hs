{-# LANGUAGE DisambiguateRecordFields #-}
module Lamdu.GUI.ExpressionGui.Wrap
    ( stdWrap
    , parentDelegator
    , stdWrapParentExpr
    ) where

import           Control.Applicative (liftA2)
import qualified Control.Lens as Lens
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.Responsive (Responsive(..))
import qualified GUI.Momentu.State as GuiState
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.FocusDelegator as FocusDelegator
import           Lamdu.Config (Config)
import qualified Lamdu.Config as Config
import qualified Lamdu.GUI.ExpressionEdit.Dotter as Dotter
import qualified Lamdu.GUI.ExpressionEdit.EventMap as ExprEventMap
import           Lamdu.GUI.ExpressionGui (ExpressionGui)
import qualified Lamdu.GUI.ExpressionGui as ExprGui
import           Lamdu.GUI.ExpressionGui.Annotation (maybeAddAnnotationPl)
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Name (Name)
import qualified Lamdu.Sugar.Types as Sugar
import           Revision.Deltum.Transaction (Transaction)

import           Lamdu.Prelude

type T = Transaction

parentExprFDConfig :: Config -> FocusDelegator.Config
parentExprFDConfig config = FocusDelegator.Config
    { FocusDelegator.focusChildKeys = config ^. Config.enterSubexpressionKeys
    , FocusDelegator.focusChildDoc = E.Doc ["Navigation", "Enter subexpression"]
    , FocusDelegator.focusParentKeys = config ^. Config.leaveSubexpressionKeys
    , FocusDelegator.focusParentDoc = E.Doc ["Navigation", "Leave subexpression"]
    }

stdWrap ::
    (Monad m, Applicative f) =>
    Sugar.Payload (Name g) f ExprGui.Payload ->
    ExprGuiM m (Responsive (f GuiState.Update) -> Responsive (f GuiState.Update))
stdWrap pl =
    maybeAddAnnotationPl pl
    <<< Dotter.with pl
    <<< ExprEventMap.add ExprEventMap.defaultOptions pl
    where
        (<<<) = liftA2 (.)

parentDelegator ::
    ( MonadReader env m, Config.HasConfig env, GuiState.HasCursor env, Applicative f
    ) => Widget.Id ->
    m (Responsive (f GuiState.Update) -> Responsive (f GuiState.Update))
parentDelegator myId =
    FocusDelegator.make <*> (Lens.view Config.config <&> parentExprFDConfig)
    ?? FocusDelegator.FocusEntryChild ?? myId

stdWrapParentExpr ::
    Monad m =>
    Sugar.Payload (Name g) (T m) ExprGui.Payload ->
    ExprGuiM m (ExpressionGui (T m) -> ExpressionGui (T m))
stdWrapParentExpr pl =
    (.)
    <$> stdWrap pl
    <*> parentDelegator (WidgetIds.fromExprPayload pl)
