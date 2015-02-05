{-# LANGUAGE NoMonomorphismRestriction, RecordWildCards, OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.HoleEdit
  ( make
  ) where

import           Control.Applicative (Applicative(..))
import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Monad (mzero, guard)
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
import           Lamdu.GUI.ExpressionEdit.HoleEdit.Common (openHoleEventMap)
import           Lamdu.GUI.ExpressionEdit.HoleEdit.Info (HoleInfo(..), EditableHoleInfo(..), HoleIds(..))
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.Open as HoleOpen
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.SearchTerm as SearchTerm
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.State as HoleState
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.WidgetIds as HoleWidgetIds
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.Wrapper as Wrapper
import           Lamdu.GUI.ExpressionGui (ExpressionGui)
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Sugar.AddNames.Types (Name(..))
import qualified Lamdu.Sugar.Types as Sugar

type T = Transaction

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

makeWrapper :: MonadA m => HoleInfo m -> ExprGuiM m (Maybe (ExpressionGui m))
makeWrapper holeInfo =
  hiMArgument holeInfo
  & Lens._Just %%~ Wrapper.make (hiIds holeInfo)
  >>= Lens._Just . ExpressionGui.egWidget %%~
      ExprGuiM.widgetEnv . BWidgets.makeFocusableView hidClosed
  where
    HoleIds{..} = hiIds holeInfo

destCursor :: HoleIds -> Maybe (Sugar.HoleArg m expr) -> Widget.Id
destCursor HoleIds{..} Nothing = hidOpen
destCursor HoleIds{..} (Just _) = hidClosed

maybeHoverSearchTermBelow ::
  MonadA m => HoleIds -> ExpressionGui m -> ExpressionGui m -> ExpressionGui m
maybeHoverSearchTermBelow HoleIds{..} searchTermGui wrapperGui
  | wrapperGui ^. ExpressionGui.egWidget . Widget.isFocused =
    wrapperGui
    & ExpressionGui.addBelow 0.5 [(0.5, searchTermWidget)]
  | otherwise =
    wrapperGui
  where
    searchTermWidget =
      searchTermGui ^. ExpressionGui.egWidget
      & Widget.takesFocus (const (pure hidOpen))

make ::
  MonadA m =>
  Sugar.Hole (Name m) m (ExprGuiM.SugarExpr m) ->
  Sugar.Payload m ExprGuiM.Payload ->
  ExprGuiM m (ExpressionGui m)
make hole pl =
  do
    Config.Hole{..} <- ExprGuiM.readConfig <&> Config.hole
    mEditableHoleInfo <-
      hole ^. Sugar.holeMActions
      & Lens._Just %%~ mkEditableHoleInfo holeInfo
      & ExprGuiM.transaction
    do
      mWrapper <- makeWrapper holeInfo
      searchTermGui <- SearchTerm.make holeInfo mEditableHoleInfo
      closedHoleGui <-
        mWrapper
        & maybe (searchTermGui & takesFocus & respondAtClosedId)
          (return . maybeHoverSearchTermBelow hids searchTermGui)
        <&> ExpressionGui.egWidget %~
            Widget.weakerEvents (openHoleEventMap holeOpenKeys hids)
        & ExpressionGui.stdWrap pl
      mEditableHoleInfo
        & maybe mzero (tryOpenHole mWrapper pl)
        <&> (`Layout.hoverInPlaceOf` closedHoleGui)
        >>= ExpressionGui.egWidget %%~
            lift . ExprGuiM.widgetEnv . BWidgets.liftLayerInterval
        & runMaybeT
        <&> fromMaybe closedHoleGui
      & ExprGuiM.assignCursor myId (destCursor hids (hole ^. Sugar.holeMArg))
      & ExprGuiM.assignCursor (WidgetIds.notDelegatingId myId) closedHoleId
  where
    respondAtClosedId =
      ExpressionGui.egWidget %%~
      ExprGuiM.widgetEnv . BWidgets.respondToCursorPrefix closedHoleId
    takesFocus = ExpressionGui.egWidget %~ Widget.takesFocus (const (pure myId))
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
  Maybe (ExpressionGui m) -> Sugar.Payload m ExprGuiM.Payload ->
  EditableHoleInfo m ->
  MaybeT (ExprGuiM m) (ExpressionGui m)
tryOpenHole mWrapperGui pl editableHoleInfo = do
  isSelected <- lift . ExprGuiM.widgetEnv $ WE.isSubCursor hidOpen
  guard isSelected
  lift $ HoleOpen.make mWrapperGui pl editableHoleInfo
  where
    HoleIds{..} = hiIds (ehiInfo editableHoleInfo)
