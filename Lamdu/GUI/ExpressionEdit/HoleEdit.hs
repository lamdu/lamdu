{-# LANGUAGE NoMonomorphismRestriction, RecordWildCards, OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.HoleEdit
  ( make
  ) where

import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Lens.Tuple
import           Control.Monad (guard)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Maybe (MaybeT(..))
import           Control.MonadA (MonadA)
import           Data.Maybe (fromMaybe)
import           Data.Maybe.Utils (maybeToMPlus)
import qualified Data.Store.Transaction as Transaction
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Lamdu.Config as Config
import qualified Lamdu.GUI.BottleWidgets as BWidgets
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.Closed as HoleClosed
import           Lamdu.GUI.ExpressionEdit.HoleEdit.Common (diveIntoHole)
import           Lamdu.GUI.ExpressionEdit.HoleEdit.Info (HoleInfo(..))
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.Open as HoleOpen
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.State as HoleState
import           Lamdu.GUI.ExpressionGui (ExpressionGui(..))
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.WidgetEnvT as WE
import           Lamdu.Sugar.AddNames.Types (Name(..))
import qualified Lamdu.Sugar.Types as Sugar

make ::
  MonadA m =>
  Sugar.Hole (Name m) m (ExprGuiM.SugarExpr m) ->
  Sugar.Payload m ExprGuiM.Payload ->
  Widget.Id -> ExprGuiM m (ExpressionGui m)
make hole pl myId = do
  config <- ExprGuiM.widgetEnv WE.readConfig
  let Config.Hole{..} = Config.hole config
  (delegateDestId, closedGui) <- HoleClosed.make hole pl myId
  let
    closedSize = closedGui ^. ExpressionGui.egWidget . Widget.wSize
    resize =
      ( ExpressionGui.egWidget . Widget.wSize %~
        (_1 %~ max (closedSize ^. _1)) .
        (_2 .~ closedSize ^. _2)
      ) .
      (ExpressionGui.egAlignment .~ closedGui ^. ExpressionGui.egAlignment)
  tryOpenHole hole pl myId
    <&> resize
    <&> ExpressionGui.egWidget %~ BWidgets.liftLayerInterval config
    & runMaybeT
    <&> fromMaybe closedGui
    & ExprGuiM.assignCursor myId delegateDestId

tryOpenHole ::
  MonadA m =>
  Sugar.Hole (Name m) m (ExprGuiM.SugarExpr m) ->
  Sugar.Payload m ExprGuiM.Payload ->
  Widget.Id ->
  MaybeT (ExprGuiM m) (ExpressionGui m)
tryOpenHole hole pl myId = do
  isSelected <-
    lift . ExprGuiM.widgetEnv . WE.isSubCursor $ diveIntoHole myId
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
    , hiId = myId
    , hiState = stateProp
    , hiNearestHoles = pl ^. Sugar.plData . ExprGuiM.plNearestHoles
    , hiMArgument = hole ^. Sugar.holeMArg
    }
