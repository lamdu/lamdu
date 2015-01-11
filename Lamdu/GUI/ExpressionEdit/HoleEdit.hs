{-# LANGUAGE NoMonomorphismRestriction, RecordWildCards, OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.HoleEdit
  ( make
  ) where

import           Control.Lens.Operators
import           Control.Lens.Tuple
import           Control.Monad (guard)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Maybe (MaybeT(..))
import           Control.MonadA (MonadA)
import           Data.Maybe (fromMaybe)
import           Data.Maybe.Utils (maybeToMPlus)
import qualified Data.Store.Transaction as Transaction
import           Data.Vector.Vector2 (Vector2(..))
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Lamdu.Config as Config
import qualified Lamdu.GUI.BottleWidgets as BWidgets
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.Closed as HoleClosed
import           Lamdu.GUI.ExpressionEdit.HoleEdit.Info (HoleInfo(..), HoleIds(..))
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.Open as HoleOpen
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.State as HoleState
import           Lamdu.GUI.ExpressionEdit.HoleEdit.WidgetIds (openHoleId)
import           Lamdu.GUI.ExpressionGui (ExpressionGui(..))
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.WidgetEnvT as WE
import           Lamdu.Sugar.AddNames.Types (Name(..))
import qualified Lamdu.Sugar.Types as Sugar

-- Resize a gui to be the same size as another gui, but also keep the
-- same alignment point
resizeAs :: ExpressionGui m -> ExpressionGui m -> ExpressionGui m
resizeAs copyFrom gui =
  gui
  & ExpressionGui.egWidget %~
    Widget.translate (Vector2 0 (alignmentHeight copyFrom - alignmentHeight gui))
  -- Copy size and alignment
  & ExpressionGui.egWidget . Widget.wSize .~ destSize
  & ExpressionGui.egAlignment .~ (copyFrom ^. ExpressionGui.egAlignment)
  where
    destSize = copyFrom ^. ExpressionGui.egWidget . Widget.wSize
    alignmentHeight eg =
      eg ^. ExpressionGui.egAlignment *
      eg ^. ExpressionGui.egWidget . Widget.wSize . _2

make ::
  MonadA m =>
  Sugar.Hole (Name m) m (ExprGuiM.SugarExpr m) ->
  Sugar.Payload m ExprGuiM.Payload ->
  Widget.Id -> ExprGuiM m (ExpressionGui m)
make hole pl myId = do
  config <- ExprGuiM.widgetEnv WE.readConfig
  let Config.Hole{..} = Config.hole config
  let hids =
        HoleIds
        { hid = myId
        , hidOpen = openHoleId myId
        , hidClosed = Widget.joinId myId ["OpenHole"]
        }
  (delegateDestId, closedGui) <- HoleClosed.make hole pl hids
  tryOpenHole hole pl hids
    <&> resizeAs closedGui
    <&> ExpressionGui.egWidget %~ BWidgets.liftLayerInterval config
    & runMaybeT
    <&> fromMaybe closedGui
    & ExprGuiM.assignCursor myId delegateDestId

tryOpenHole ::
  MonadA m =>
  Sugar.Hole (Name m) m (ExprGuiM.SugarExpr m) ->
  Sugar.Payload m ExprGuiM.Payload ->
  HoleIds ->
  MaybeT (ExprGuiM m) (ExpressionGui m)
tryOpenHole hole pl hids@HoleIds{..} = do
  isSelected <- lift . ExprGuiM.widgetEnv $ WE.isSubCursor hidOpen
  guard isSelected
  actions <- maybeToMPlus $ hole ^. Sugar.holeMActions
  stateProp <-
    lift . ExprGuiM.transaction $
    HoleState.assocStateRef (actions ^. Sugar.holeGuid) ^.
    Transaction.mkProperty
  lift $ HoleOpen.make pl HoleInfo
    { hiEntityId = pl ^. Sugar.plEntityId
    , hiActions = actions
    , hiSuggested = hole ^. Sugar.holeSuggested
    , hiIds = hids
    , hiState = stateProp
    , hiNearestHoles = pl ^. Sugar.plData . ExprGuiM.plNearestHoles
    , hiMArgument = hole ^. Sugar.holeMArg
    }
