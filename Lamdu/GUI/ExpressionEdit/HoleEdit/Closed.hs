module Lamdu.GUI.ExpressionEdit.HoleEdit.Closed
  ( make
  ) where

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
import System.Random.Utils (genFromHashable)
import qualified Control.Lens as Lens
import qualified Data.Store.Transaction as Transaction
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Lamdu.Config as Config
import qualified Lamdu.Data.Expr.Lens as ExprLens
import qualified Lamdu.Data.Expr.Utils as ExprUtil
import qualified Lamdu.GUI.BottleWidgets as BWidgets
import qualified Lamdu.GUI.ExpressionEdit.EventMap as ExprEventMap
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.WidgetEnvT as WE
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.Sugar.RemoveTypes as SugarRemoveTypes
import qualified Lamdu.Sugar.Types as Sugar

type T = Transaction.Transaction

make ::
  MonadA m =>
  Sugar.Hole Sugar.Name m (ExprGuiM.SugarExpr m) ->
  Sugar.Payload Sugar.Name m ExprGuiM.Payload ->
  Widget.Id ->
  ExprGuiM m (Widget.Id, ExpressionGui m)
make hole pl myId = do
  (destId, rawInactive) <- runMatcherT $ do
    justToLeft $ do
      arg <- maybeToMPlus $ hole ^. Sugar.holeMArg
      lift $ (,) myId <$> makeWrapper arg myId
    justToLeft $ do
      inferred <- maybeToMPlus $ hole ^. Sugar.holeMInferred
      guard . Lens.nullOf ExprLens.exprHole $ inferred ^. Sugar.hiWithVarsValue
      lift $ makeInferred inferred pl myId
    lift $ (,) (diveIntoHole myId) <$> makeSimple myId
  exprEventMap <-
    ExprEventMap.make
    (rawInactive ^. ExpressionGui.egWidget . Widget.wIsFocused) [] pl
  inactive <-
    ExpressionGui.addInferredTypes pl rawInactive
    <&> ExpressionGui.egWidget %~
        Widget.weakerEvents (mappend openEventMap exprEventMap)
  return (destId, inactive)
  where
    openEventMap =
      Widget.keysEventMapMovesCursor [E.ModKey E.noMods E.Key'Enter]
      (E.Doc ["Navigation", "Hole", "Open"]) . pure $
      diveIntoHole myId

makeWrapperEventMap ::
  (MonadA m, MonadA f) =>
  Bool -> Sugar.HoleArg f (Sugar.ExpressionN f a) -> Widget.Id ->
  ExprGuiM m (Widget.EventHandlers (T f))
makeWrapperEventMap argIsFocused arg myId = do
  config <- ExprGuiM.widgetEnv WE.readConfig
  let
    tryUnwrapEventMap =
      case arg ^? Sugar.haUnwrap . Sugar._UnwrapMAction . Lens._Just of
      Just unwrap ->
        Widget.keysEventMapMovesCursor (Config.acceptKeys config ++ Config.delKeys config)
        (E.Doc ["Edit", "Unwrap"]) $ WidgetIds.fromGuid <$> unwrap
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
        maybe mempty
        (Widget.keysEventMapMovesCursor (Config.enterSubexpressionKeys config)
         (E.Doc ["Navigation", "Go to wrapped expr"]) .
         pure . WidgetIds.fromGuid) $
        arg ^? Sugar.haExpr . Sugar.rPayload . Sugar.plActions . Lens._Just . Sugar.storedGuid
  pure $
    mappend tryUnwrapEventMap navigateEventMap

makeWrapper ::
  MonadA m =>
  Sugar.HoleArg m (Sugar.ExpressionN m ExprGuiM.Payload) ->
  Widget.Id -> ExprGuiM m (ExpressionGui m)
makeWrapper arg myId = do
  config <- ExprGuiM.widgetEnv WE.readConfig
  let
    bgColor =
      case arg ^. Sugar.haUnwrap of
      Sugar.UnwrapMAction {} -> Config.deletableHoleBackgroundColor config
      Sugar.UnwrapTypeMismatch {} -> Config.typeErrorHoleWrapBackgroundColor config
  rawArgGui <-
    arg ^. Sugar.haExpr
    & ExprGuiM.makeSubexpression 0
  eventMap <-
    makeWrapperEventMap
    (rawArgGui ^. ExpressionGui.egWidget . Widget.wIsFocused)
    arg myId
  rawArgGui
    & ExpressionGui.egWidget %%~
      makeFocusable myId . Widget.weakerEvents eventMap
    <&> ExpressionGui.pad (realToFrac <$> Config.wrapperHolePadding config)
    <&> ExpressionGui.egWidget %~
        makeBackground myId
        (Config.layerHoleBG (Config.layers config)) bgColor

makeInferred ::
  MonadA m =>
  Sugar.HoleInferred Sugar.Name m -> Sugar.Payload Sugar.Name m a ->
  Widget.Id -> ExprGuiM m (Widget.Id, ExpressionGui m)
makeInferred inferred pl myId = do
  config <- ExprGuiM.widgetEnv WE.readConfig
  gui <-
    (inferred ^. Sugar.hiMakeConverted) gen
    & ExprGuiM.liftMemoT
    <&> Lens.mapped . Lens.mapped .~ emptyPl
    <&> SugarRemoveTypes.holeResultTypes
    >>= ExprGuiM.makeSubexpression 0
    >>= ExpressionGui.egWidget %%~
        makeFocusable myId .
        Widget.tint (Config.inferredValueTint config) .
        Widget.scale (realToFrac <$> Config.inferredValueScaleFactor config) .
        (Widget.wEventMap .~ mempty)
  return $
    if fullyInferred
    then (myId, gui)
    else
      ( diveIntoHole myId
      , gui
        & ExpressionGui.egWidget %~
          makeBackground myId (Config.layerHoleBG (Config.layers config))
          (Config.inactiveHoleBackgroundColor config)
      )
  where
    fullyInferred =
      Lens.nullOf (Lens.folding ExprUtil.subExpressions . ExprLens.exprHole) $
      inferred ^. Sugar.hiWithVarsValue
    -- gen needs to be compatible with the one from Sugar.Convert.Hole
    -- for the hole results, for smooth animation between inferred
    -- pure val and the hole result:
    gen = genFromHashable (pl ^. Sugar.plGuid, "inferred", 0 :: Int)
    emptyPl =
      ExprGuiM.Payload
      { ExprGuiM._plStoredGuids = []
      , ExprGuiM._plInjected = []
      -- filled by AddNextHoles above
      , ExprGuiM._plHoleGuids = ExprGuiM.emptyHoleGuids
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
