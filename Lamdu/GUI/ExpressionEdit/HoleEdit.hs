{-# LANGUAGE NoMonomorphismRestriction, RecordWildCards, OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.HoleEdit
  ( make
  ) where

import           Control.Applicative (Applicative(..))
import           Control.Lens.Operators
import           Control.Lens.Tuple
import           Control.Monad (guard, join)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Maybe (MaybeT(..))
import           Control.MonadA (MonadA)
import           Data.Maybe (fromMaybe)
import           Data.Maybe.Utils (maybeToMPlus)
import qualified Data.Store.Transaction as Transaction
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Layout as Layout
import qualified Lamdu.Config as Config
import qualified Lamdu.GUI.BottleWidgets as BWidgets
import           Lamdu.GUI.ExpressionEdit.HoleEdit.Closed (ClosedHole(..))
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.Closed as HoleClosed
import           Lamdu.GUI.ExpressionEdit.HoleEdit.Info (HoleInfo(..), HoleIds(..))
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.Open as HoleOpen
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.State as HoleState
import           Lamdu.GUI.ExpressionEdit.HoleEdit.WidgetIds (openHoleId)
import           Lamdu.GUI.ExpressionGui (ExpressionGui)
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.WidgetEnvT as WE
import           Lamdu.Sugar.AddNames.Types (Name(..))
import qualified Lamdu.Sugar.Types as Sugar

chDestId :: HoleIds -> HoleClosed.HoleDest -> Widget.Id
chDestId HoleIds{..} HoleClosed.HoleDestClosed = hidClosed
chDestId HoleIds{..} HoleClosed.HoleDestOpened = hidOpen

make ::
  MonadA m =>
  Sugar.Hole (Name m) m (ExprGuiM.SugarExpr m) ->
  Sugar.Payload m ExprGuiM.Payload -> Widget.Id ->
  ExprGuiM m (ExpressionGui m)
make hole pl myId =
  do
    (closedHoleGui, unwrappedClosedHoleGui) <-
      _chMkGui
      <&> join (,)
      & ExpressionGui.stdWrapIn _1 pl
    config <- ExprGuiM.widgetEnv WE.readConfig
    let Config.Hole{..} = Config.hole config
    tryOpenHole unwrappedClosedHoleGui hole pl hids
      <&> hAlign .~ 0
      <&> (`Layout.hoverInPlaceOf` (closedHoleGui & hAlign .~ 0))
      <&> ExpressionGui.egWidget %~ BWidgets.liftLayerInterval config
      & runMaybeT
      <&> fromMaybe closedHoleGui
      <&> ExpressionGui.egWidget %~ Widget.takesFocus (const (pure myId))
  & ExprGuiM.assignCursor myId (chDestId hids chDest)
  where
    hAlign = ExpressionGui.egAlignment . _1
    ClosedHole{..} = HoleClosed.make hole pl hids
    hids = HoleIds
      { hidOpen = openHoleId myId
      , hidClosed = Widget.joinId myId ["ClosedHole"]
      }

tryOpenHole ::
  MonadA m =>
  ExpressionGui m ->
  Sugar.Hole (Name m) m (ExprGuiM.SugarExpr m) ->
  Sugar.Payload m ExprGuiM.Payload ->
  HoleIds ->
  MaybeT (ExprGuiM m) (ExpressionGui m)
tryOpenHole closedHoleGui hole pl hids@HoleIds{..} = do
  isSelected <- lift . ExprGuiM.widgetEnv $ WE.isSubCursor hidOpen
  guard isSelected
  actions <- maybeToMPlus $ hole ^. Sugar.holeMActions
  stateProp <-
    lift . ExprGuiM.transaction $
    HoleState.assocStateRef (actions ^. Sugar.holeGuid) ^.
    Transaction.mkProperty
  lift $ HoleOpen.make closedHoleGui pl HoleInfo
    { hiEntityId = pl ^. Sugar.plEntityId
    , hiActions = actions
    , hiSuggested = hole ^. Sugar.holeSuggested
    , hiIds = hids
    , hiState = stateProp
    , hiNearestHoles = pl ^. Sugar.plData . ExprGuiM.plNearestHoles
    , hiMArgument = hole ^. Sugar.holeMArg
    }
