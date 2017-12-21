{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.HoleEdit.State
    ( setHoleStateAndJump, readSearchTerm
    ) where

import qualified GUI.Momentu.State as GuiState
import           Lamdu.GUI.ExpressionEdit.HoleEdit.WidgetIds (WidgetIds)
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.WidgetIds as HoleWidgetIds
import           Lamdu.Sugar.Types (EntityId)

import           Lamdu.Prelude

setHoleStateAndJump :: Text -> EntityId -> GuiState.Update
setHoleStateAndJump state entityId =
    GuiState.updateCursor holeId
    <> GuiState.updateWidgetState holeId state
    where
        holeId = HoleWidgetIds.make entityId & HoleWidgetIds.hidOpen

readSearchTerm ::
    (MonadReader env m, GuiState.HasState env) => WidgetIds -> m Text
readSearchTerm widgetIds =
    HoleWidgetIds.hidOpen widgetIds & GuiState.readWidgetState
    <&> fromMaybe ""
