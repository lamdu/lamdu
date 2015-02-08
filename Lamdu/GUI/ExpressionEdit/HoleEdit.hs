{-# LANGUAGE NoMonomorphismRestriction, RecordWildCards, OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.HoleEdit
  ( make
  ) where

import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.MonadA (MonadA)
import           Data.Store.Transaction (Transaction)
import qualified Data.Store.Transaction as Transaction
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets as BWidgets
import           Graphics.UI.Bottle.Widgets.Layout (Layout)
import qualified Graphics.UI.Bottle.Widgets.Layout as Layout
import qualified Graphics.UI.Bottle.WidgetsEnvT as WE
import qualified Lamdu.Config as Config
import           Lamdu.GUI.ExpressionEdit.HoleEdit.Common (addDarkBackground)
import           Lamdu.GUI.ExpressionEdit.HoleEdit.Info (HoleInfo(..), EditableHoleInfo(..))
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.SearchArea as SearchArea
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

makeWrapper ::
  MonadA m => Sugar.Payload m ExprGuiM.Payload ->
  HoleInfo m -> ExprGuiM m (Maybe (ExpressionGui m))
makeWrapper pl holeInfo =
  hiMArgument holeInfo
  & Lens._Just %%~
    ExpressionGui.wrapExprEventMap pl . Wrapper.make (hiIds holeInfo)

assignHoleCursor ::
  MonadA m =>
  WidgetIds -> Maybe (Sugar.HoleArg m expr) -> ExprGuiM m a -> ExprGuiM m a
assignHoleCursor WidgetIds{..} Nothing =
  ExprGuiM.assignCursor hidHole hidOpen .
  ExprGuiM.assignCursor (WidgetIds.notDelegatingId hidHole) hidClosedSearchArea
assignHoleCursor WidgetIds{..} (Just _) =
  ExprGuiM.assignCursor hidHole hidWrapper .
  ExprGuiM.assignCursor (WidgetIds.notDelegatingId hidHole) hidWrapper

liftLayers :: MonadA m => ExpressionGui n -> ExprGuiM m (ExpressionGui n)
liftLayers =
  ExpressionGui.egWidget %%~ ExprGuiM.widgetEnv . BWidgets.liftLayerInterval

addSearchAreaBelow ::
  MonadA m => WidgetIds -> ExpressionGui m ->
  ExpressionGui m ->
  ExprGuiM m (ExpressionGui m)
addSearchAreaBelow WidgetIds{..} searchAreaGui wrapperGui =
  do
    hoveringSearchArea <- liftLayers searchAreaGui
    wrapperGui
      & Layout.addAfter Layout.Vertical [hoveringSearchArea]
      & return

addWrapperAbove ::
  MonadA m =>
  WidgetIds -> ExpressionGui f -> Layout (T f) -> ExprGuiM m (Layout (T f))
addWrapperAbove WidgetIds{..} wrapperGui searchAreaGui =
  do
    Config.Hole{..} <- ExprGuiM.readConfig <&> Config.hole
    hoveringWrapper <-
      addDarkBackground (Widget.toAnimId hidWrapper ++ ["wrapperDarkBg"])
      wrapperGui
      <&> Layout.scale (holeHoveringWrapperScaleFactor <&> realToFrac)
      >>= liftLayers
    searchAreaGui
      & Layout.addBefore Layout.Vertical [hoveringWrapper]
      & return

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
      mWrapperGui <- makeWrapper pl holeInfo
      case mWrapperGui of
        Just wrapperGui -> do
          unfocusedWrapperGui <-
            wrapperGui & ExpressionGui.maybeAddInferredTypePl pl
          isSelected <- ExprGuiM.widgetEnv $ WE.isSubCursor hidHole
          if isSelected
            then do
              searchAreaGui <- SearchArea.makeStdWrapped pl holeInfo mEditableHoleInfo
              if wrapperGui ^. ExpressionGui.egWidget . Widget.isFocused
                then addSearchAreaBelow WidgetIds{..} searchAreaGui wrapperGui
                else addWrapperAbove WidgetIds{..} wrapperGui searchAreaGui
              <&> (`Layout.hoverInPlaceOf` unfocusedWrapperGui)
            else
              return unfocusedWrapperGui
        Nothing -> SearchArea.makeStdWrapped pl holeInfo mEditableHoleInfo
      & assignHoleCursor WidgetIds{..} (hole ^. Sugar.holeMArg)
  where
    holeInfo = HoleInfo
      { hiEntityId = pl ^. Sugar.plEntityId
      , hiInferredType = pl ^. Sugar.plInferredType
      , hiSuggested = hole ^. Sugar.holeSuggested
      , hiIds = WidgetIds{..}
      , hiNearestHoles = pl ^. Sugar.plData . ExprGuiM.plNearestHoles
      , hiMArgument = hole ^. Sugar.holeMArg
      }
    WidgetIds{..} = HoleWidgetIds.make (pl ^. Sugar.plEntityId)
