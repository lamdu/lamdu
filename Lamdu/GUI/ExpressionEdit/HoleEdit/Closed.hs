module Lamdu.GUI.ExpressionEdit.HoleEdit.Closed
  ( make
  ) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Lens.Operators
import Control.Monad (guard, void)
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
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Lamdu.Config as Config
import qualified Lamdu.Data.Expression.Infer as Infer
import qualified Lamdu.Data.Expression.Lens as ExprLens
import qualified Lamdu.Data.Expression.Utils as ExprUtil
import qualified Lamdu.GUI.BottleWidgets as BWidgets
import qualified Lamdu.GUI.ExpressionEdit.EventMap as ExprEventMap
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.WidgetEnvT as WE
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.Sugar.InputExpr as InputExpr
import qualified Lamdu.Sugar.Types as Sugar

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
      guard . Lens.nullOf ExprLens.exprHole . Infer.iValue $ Sugar.hiInferred inferred
      lift $ makeInferred inferred pl myId
    lift $ (,) (diveIntoHole myId) <$> makeSimple myId
  exprEventMap <- ExprEventMap.make [] pl
  inactive <-
    ExpressionGui.addInferredTypes pl rawInactive
    <&> ExpressionGui.egWidget %~
        Widget.weakerEvents (mappend openEventMap exprEventMap)
  return (destId, inactive)
  where
    openEventMap =
      Widget.keysEventMapMovesCursor [E.ModKey E.noMods E.KeyEnter]
      (E.Doc ["Navigation", "Hole", "Open"]) . pure $
      diveIntoHole myId

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
    eventMap =
      case arg ^? Sugar.haUnwrap . Sugar._UnwrapMAction . Lens._Just of
      Just unwrap ->
        E.keyPresses (Config.acceptKeys config ++ Config.delKeys config)
        (E.Doc ["Edit", "Unwrap"]) $
        Widget.eventResultFromCursor . WidgetIds.fromGuid <$> unwrap
      Nothing ->
        E.keyPresses (Config.wrapKeys config)
        (E.Doc ["Navigation", "Hole", "Open"]) .
        pure . Widget.eventResultFromCursor $
        diveIntoHole myId
  arg ^. Sugar.haExpr
    & ExprGuiM.makeSubexpression 0
    >>= ExpressionGui.egWidget %%~
        makeFocusable myId . (Widget.wEventMap .~ eventMap)
    <&> ExpressionGui.pad (realToFrac <$> Config.wrapperHolePadding config)
    <&> ExpressionGui.egWidget %~
        makeBackground myId
        (Config.layerInactiveHole (Config.layers config)) bgColor

makeInferred ::
  MonadA m =>
  Sugar.HoleInferred m -> Sugar.Payload Sugar.Name m a ->
  Widget.Id -> ExprGuiM m (Widget.Id, ExpressionGui m)
makeInferred inferred pl myId = do
  config <- ExprGuiM.widgetEnv WE.readConfig
  gui <-
    iVal
    & InputExpr.makePure gen
    & Sugar.runConvert (pl ^. Sugar.plConvertInContext)
      (Sugar.hiContext inferred)
    & ExprGuiM.liftMemoT
    <&> Lens.mapped . Lens.mapped .~ emptyPl
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
          makeBackground myId (Config.layerInactiveHole (Config.layers config))
          (Config.inactiveHoleBackgroundColor config)
      )
  where
    fullyInferred = Lens.nullOf (Lens.folding ExprUtil.subExpressions . ExprLens.exprHole) iVal
    iVal = void . Infer.iValue $ Sugar.hiInferred inferred
    -- gen needs to be compatible with the one from Sugar.Convert.Hole
    -- for the hole results, for smooth animation between inferred
    -- pure val and the hole result:
    gen = genFromHashable (pl ^. Sugar.plGuid, ["inferred"], 0 :: Int)
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
      (Config.layerInactiveHole (Config.layers config))
      (Config.inactiveHoleBackgroundColor config)
    <&> ExpressionGui.fromValueWidget
    >>= ExpressionGui.egWidget %%~ makeFocusable myId

makeFocusable :: (MonadA m, Applicative f) => Widget.Id -> Widget f -> ExprGuiM m (Widget f)
makeFocusable wId = ExprGuiM.widgetEnv . BWidgets.makeFocusableView wId
