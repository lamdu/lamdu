module Lamdu.GUI.ExpressionEdit.HoleEdit.Closed
  ( make
  ) where

--import qualified Lamdu.Expr.Utils as ExprUtil
import Control.Applicative (Applicative(..), (<$>))
import Control.Lens.Operators
import Control.Monad (guard)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Either.Utils (runMatcherT, justToLeft)
import Control.MonadA (MonadA)
import Data.Maybe.Utils (maybeToMPlus)
import Data.Monoid (Monoid(..))
import Graphics.UI.Bottle.Widget (Widget)
import Lamdu.GUI.ExpressionEdit.HoleEdit.Common (makeBackground, diveIntoHole)
import Lamdu.GUI.ExpressionGui (ExpressionGui(..))
import Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import Lamdu.Sugar.AddNames.Types (Name(..), ExpressionN)
import qualified Control.Lens as Lens
import qualified Data.Store.Transaction as Transaction
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Lamdu.Config as Config
import qualified Lamdu.Expr.Lens as ExprLens
import qualified Lamdu.GUI.BottleWidgets as BWidgets
import qualified Lamdu.GUI.ExpressionEdit.EventMap as ExprEventMap
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.WidgetEnvT as WE
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.Sugar.Types as Sugar

type T = Transaction.Transaction

make ::
  MonadA m =>
  Sugar.Hole (Name m) m (ExprGuiM.SugarExpr m) ->
  Sugar.Payload m ExprGuiM.Payload ->
  Widget.Id ->
  ExprGuiM m (Widget.Id, ExpressionGui m)
make hole pl myId = do
  (destId, rawInactive) <- runMatcherT $ do
    justToLeft $ do
      arg <- maybeToMPlus $ hole ^. Sugar.holeMArg
      lift $ (,) myId <$> makeWrapper arg myId
    justToLeft $ do
      guard . Lens.nullOf ExprLens.valHole $ suggested ^. Sugar.hsValue
      lift $ makeSuggested suggested myId
    lift $ (,) (diveIntoHole myId) <$> makeSimple myId
  exprEventMap <-
    ExprEventMap.make
    (rawInactive ^. ExpressionGui.egWidget . Widget.wIsFocused) [] pl
  inactive <-
    rawInactive
    & addInferredTypes
    <&> ExpressionGui.egWidget %~
        Widget.weakerEvents (mappend openEventMap exprEventMap)
  return (destId, inactive)
  where
    addInferredTypes =
      if null (pl ^. Sugar.plData . ExprGuiM.plStoredEntityIds)
      then return
      else ExpressionGui.addInferredTypes pl
    suggested = hole ^. Sugar.holeSuggested
    openEventMap =
      Widget.keysEventMapMovesCursor [E.ModKey E.noMods E.Key'Enter]
      (E.Doc ["Navigation", "Hole", "Open"]) . pure $
      diveIntoHole myId

makeWrapperEventMap ::
  (MonadA m, MonadA f) =>
  Bool -> Sugar.HoleArg f (ExpressionN f a) -> Widget.Id ->
  ExprGuiM m (Widget.EventHandlers (T f))
makeWrapperEventMap argIsFocused arg myId = do
  config <- ExprGuiM.widgetEnv WE.readConfig
  let
    tryUnwrapEventMap =
      case arg ^? Sugar.haUnwrap . Sugar._UnwrapMAction . Lens._Just of
      Just unwrap ->
        Widget.keysEventMapMovesCursor (Config.acceptKeys config ++ Config.delKeys config)
        (E.Doc ["Edit", "Unwrap"]) $ WidgetIds.fromEntityId <$> unwrap
      Nothing ->
        Widget.keysEventMapMovesCursor (Config.wrapKeys config)
        (E.Doc ["Navigation", "Hole", "Open"]) .
        pure $ diveIntoHole myId
    navigateEventMap
      | argIsFocused =
        Widget.keysEventMapMovesCursor (Config.leaveSubexpressionKeys config)
        (E.Doc ["Navigation", "Go to parent wrapper"]) $
        pure myId
      | otherwise =
        Widget.keysEventMapMovesCursor (Config.enterSubexpressionKeys config)
        (E.Doc ["Navigation", "Go to wrapped expr"]) .
        pure . WidgetIds.fromEntityId $
        arg ^. Sugar.haExpr . Sugar.rPayload . Sugar.plEntityId
  pure $
    mappend tryUnwrapEventMap navigateEventMap

makeWrapper ::
  MonadA m =>
  Sugar.HoleArg m (ExpressionN m ExprGuiM.Payload) ->
  Widget.Id -> ExprGuiM m (ExpressionGui m)
makeWrapper arg myId = do
  config <- ExprGuiM.widgetEnv WE.readConfig
  let
    (bgColor, addTypes) =
      case arg ^. Sugar.haUnwrap of
      Sugar.UnwrapMAction {} ->
        ( Config.deletableHoleBackgroundColor config
        , return
        )
      Sugar.UnwrapTypeMismatch {} ->
        ( Config.typeErrorHoleWrapBackgroundColor config
        , ExpressionGui.addInferredTypes (arg ^. Sugar.haExpr . Sugar.rPayload)
        )
  rawArgGui <-
    arg ^. Sugar.haExpr
    & ExprGuiM.makeSubexpression 0
  eventMap <-
    makeWrapperEventMap
    (rawArgGui ^. ExpressionGui.egWidget . Widget.wIsFocused)
    arg myId
  addTypes rawArgGui
    >>= ExpressionGui.egWidget %%~
      makeFocusable myId . Widget.weakerEvents eventMap
    <&> ExpressionGui.pad (realToFrac <$> Config.wrapperHolePadding config)
    <&> ExpressionGui.egWidget %~
        makeBackground myId
        (Config.layerHoleBG (Config.layers config)) bgColor

makeSuggested ::
  MonadA m =>
  Sugar.HoleSuggested (Name m) m ->
  Widget.Id -> ExprGuiM m (Widget.Id, ExpressionGui m)
makeSuggested suggested myId = do
  config <- ExprGuiM.widgetEnv WE.readConfig
  gui <-
    (suggested ^. Sugar.hsMakeConverted)
    & ExprGuiM.transaction
    <&> Lens.mapped . Lens.mapped .~ emptyPl
    >>= ExprGuiM.makeSubexpression 0
    >>= ExpressionGui.egWidget %%~
        makeFocusable myId .
        Widget.tint (Config.suggestedValueTint config) .
        Widget.scale (realToFrac <$> Config.suggestedValueScaleFactor config) .
        (Widget.wEventMap .~ mempty)
  return $
    if fullySuggested
    then (myId, gui)
    else
      ( diveIntoHole myId
      , gui
        & ExpressionGui.egWidget %~
          makeBackground myId (Config.layerHoleBG (Config.layers config))
          (Config.inactiveHoleBackgroundColor config)
      )
  where
    fullySuggested =
      Lens.nullOf (ExprLens.subExprs . ExprLens.valHole) $
      suggested ^. Sugar.hsValue
    emptyPl =
      ExprGuiM.Payload
      { ExprGuiM._plStoredEntityIds = []
      , ExprGuiM._plInjected = []
      -- filled by AddNextHoles above
      , ExprGuiM._plHoleEntityIds = ExprGuiM.emptyHoleEntityIds
      }

makeSimple :: MonadA m => Widget.Id -> ExprGuiM m (ExpressionGui m)
makeSimple myId = do
  config <- ExprGuiM.widgetEnv WE.readConfig
  ExprGuiM.widgetEnv
    (BWidgets.makeTextViewWidget "  " (Widget.toAnimId myId))
    <&>
      makeBackground myId
      (Config.layerHoleBG (Config.layers config))
      (Config.inactiveHoleBackgroundColor config)
    <&> ExpressionGui.fromValueWidget
    >>= ExpressionGui.egWidget %%~ makeFocusable myId

makeFocusable :: (MonadA m, Applicative f) => Widget.Id -> Widget f -> ExprGuiM m (Widget f)
makeFocusable wId = ExprGuiM.widgetEnv . BWidgets.makeFocusableView wId
