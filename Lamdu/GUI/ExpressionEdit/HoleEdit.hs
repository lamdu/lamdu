{-# LANGUAGE NoMonomorphismRestriction, RecordWildCards, OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.HoleEdit
  ( make
  ) where

import           Control.Applicative (Applicative(..))
import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Lens.Tuple
import           Control.Monad (mzero, guard, join)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Maybe (MaybeT(..))
import           Control.MonadA (MonadA)
import           Data.Maybe (fromMaybe)
import           Data.Store.Transaction (Transaction)
import qualified Data.Store.Transaction as Transaction
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets as BWidgets
import qualified Graphics.UI.Bottle.Widgets.Layout as Layout
import qualified Graphics.UI.Bottle.WidgetsEnvT as WE
import qualified Lamdu.Config as Config
import           Lamdu.GUI.ExpressionEdit.HoleEdit.Closed (ClosedHole(..))
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.Closed as HoleClosed
import           Lamdu.GUI.ExpressionEdit.HoleEdit.Info (HoleInfo(..), EditableHoleInfo(..), HoleIds(..))
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.Open as HoleOpen
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.State as HoleState
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.WidgetIds as HoleWidgetIds
import           Lamdu.GUI.ExpressionGui (ExpressionGui)
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Sugar.AddNames.Types (Name(..))
import qualified Lamdu.Sugar.Types as Sugar

type T = Transaction

chDestId :: HoleIds -> HoleClosed.HoleDest -> Widget.Id
chDestId HoleIds{..} HoleClosed.HoleDestClosed = hidClosed
chDestId HoleIds{..} HoleClosed.HoleDestOpened = hidOpen

mkEditableHoleInfo ::
  MonadA m => HoleInfo m -> Sugar.HoleActions (Name m) m -> T m (EditableHoleInfo m)
mkEditableHoleInfo holeInfo actions =
  do
    stateProp <-
      HoleState.assocStateRef (actions ^. Sugar.holeGuid) ^.
      Transaction.mkProperty
    EditableHoleInfo
      { ehiActions = actions
      , ehiState = stateProp
      , ehiInfo = holeInfo
      } & return

make ::
  MonadA m =>
  Sugar.Hole (Name m) m (ExprGuiM.SugarExpr m) ->
  Sugar.Payload m ExprGuiM.Payload ->
  ExprGuiM m (ExpressionGui m)
make hole pl =
  do
    mEditableHoleInfo <-
      hole ^. Sugar.holeMActions
      & Lens._Just %%~ mkEditableHoleInfo holeInfo
      & ExprGuiM.transaction
    let ClosedHole{..} = HoleClosed.make holeInfo mEditableHoleInfo
    do
      (closedHoleGui, unwrappedClosedHoleGui) <-
        _chMkGui
        <&> ExpressionGui.egWidget %~ Widget.takesFocus (const (pure myId))
        <&> join (,)
        & ExpressionGui.stdWrapIn _1 pl
      Config.Hole{..} <- ExprGuiM.readConfig <&> Config.hole
      mEditableHoleInfo
        & maybe mzero (tryOpenHole unwrappedClosedHoleGui pl)
        <&> (`Layout.hoverInPlaceOf` closedHoleGui)
        >>= ExpressionGui.egWidget %%~
            lift . ExprGuiM.widgetEnv . BWidgets.liftLayerInterval
        <&> ExpressionGui.egWidget %~ Widget.takesFocus (const (pure myId))
        & runMaybeT
        <&> fromMaybe closedHoleGui
      & ExprGuiM.assignCursor myId (chDestId hids chDest)
      & ExprGuiM.assignCursor (WidgetIds.notDelegatingId myId) closedHoleId
  where
    holeInfo = HoleInfo
      { hiEntityId = pl ^. Sugar.plEntityId
      , hiInferredType = pl ^. Sugar.plInferredType
      , hiSuggested = hole ^. Sugar.holeSuggested
      , hiIds = hids
      , hiNearestHoles = pl ^. Sugar.plData . ExprGuiM.plNearestHoles
      , hiMArgument = hole ^. Sugar.holeMArg
      }
    myId = WidgetIds.fromExprPayload pl
    closedHoleId = Widget.joinId myId ["ClosedHole"]
    hids = HoleIds
      { hidOpen = HoleWidgetIds.openHoleId myId
      , hidClosed = closedHoleId
      }

tryOpenHole ::
  MonadA m =>
  ExpressionGui m -> Sugar.Payload m ExprGuiM.Payload ->
  EditableHoleInfo m ->
  MaybeT (ExprGuiM m) (ExpressionGui m)
tryOpenHole closedHoleGui pl editableHoleInfo = do
  isSelected <- lift . ExprGuiM.widgetEnv $ WE.isSubCursor hidOpen
  guard isSelected
  lift $ HoleOpen.make closedHoleGui pl editableHoleInfo
  where
    HoleIds{..} = hiIds (ehiInfo editableHoleInfo)
