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
import           Lamdu.GUI.ExpressionEdit.HoleEdit.Common (addDarkBackground)
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.EventMap as HoleEventMap
import           Lamdu.GUI.ExpressionEdit.HoleEdit.Info (HoleInfo(..), EditableHoleInfo(..))
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.Open as HoleOpen
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.SearchTerm as SearchTerm
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.State as HoleState
import           Lamdu.GUI.ExpressionEdit.HoleEdit.WidgetIds (WidgetIds(..))
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
    WidgetIds{..} = hiIds holeInfo

destCursor :: WidgetIds -> Maybe (Sugar.HoleArg m expr) -> Widget.Id
destCursor WidgetIds{..} Nothing = hidOpen
destCursor WidgetIds{..} (Just _) = hidClosed

maybeHoverSearchTermBelow ::
  MonadA m => WidgetIds -> ExpressionGui m ->
  ExpressionGui m ->
  ExprGuiM m (ExpressionGui m)
maybeHoverSearchTermBelow WidgetIds{..} searchTermGui wrapperGui
  | wrapperGui ^. ExpressionGui.egWidget . Widget.isFocused =
    do
      searchTermWidget <-
        searchTermGui
        & addDarkBackground (Widget.toAnimId hidClosed ++ ["searchTerm"])
        >>= liftLayers
        <&> (^. ExpressionGui.egWidget)
        <&> Widget.takesFocus (const (pure hidOpen))
      wrapperGui
        & ExpressionGui.addBelow 0.5 [(0.5, searchTermWidget)]
        & (`Layout.hoverInPlaceOf` wrapperGui)
        & return
  | otherwise =
    return wrapperGui

liftLayers :: MonadA m => ExpressionGui m -> ExprGuiM m (ExpressionGui m)
liftLayers =
  ExpressionGui.egWidget %%~ ExprGuiM.widgetEnv . BWidgets.liftLayerInterval

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
          (maybeHoverSearchTermBelow hids searchTermGui)
        <&> ExpressionGui.egWidget %~
            Widget.weakerEvents (HoleEventMap.open holeOpenKeys hids)
        & ExpressionGui.stdWrap pl
      mEditableHoleInfo
        & maybe mzero (tryOpenHole mWrapper pl)
        <&> (`Layout.hoverInPlaceOf` closedHoleGui)
        >>= lift . liftLayers
        & runMaybeT
        <&> fromMaybe closedHoleGui
      & ExprGuiM.assignCursor hidHole (destCursor hids (hole ^. Sugar.holeMArg))
      & ExprGuiM.assignCursor (WidgetIds.notDelegatingId hidHole) hidClosed
  where
    respondAtClosedId =
      ExpressionGui.egWidget %%~
      ExprGuiM.widgetEnv . BWidgets.respondToCursorPrefix hidClosed
    takesFocus = ExpressionGui.egWidget %~ Widget.takesFocus (const (pure hidHole))
    holeInfo = HoleInfo
      { hiEntityId = pl ^. Sugar.plEntityId
      , hiInferredType = pl ^. Sugar.plInferredType
      , hiSuggested = hole ^. Sugar.holeSuggested
      , hiIds = hids
      , hiNearestHoles = pl ^. Sugar.plData . ExprGuiM.plNearestHoles
      , hiMArgument = hole ^. Sugar.holeMArg
      }
    hids@WidgetIds{..} = HoleWidgetIds.make (pl ^. Sugar.plEntityId)

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
    WidgetIds{..} = hiIds (ehiInfo editableHoleInfo)
