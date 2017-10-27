{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.HoleEdit.State
    ( setHoleStateAndJump, readSearchTerm
    ) where

import qualified GUI.Momentu.State as GuiState
import           Lamdu.GUI.ExpressionEdit.HoleEdit.WidgetIds (WidgetIds)
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.WidgetIds as WidgetIds
import           Lamdu.Sugar.Types (EntityId)

import           Lamdu.Prelude

setHoleStateAndJump :: Text -> EntityId -> GuiState.Update
setHoleStateAndJump state entityId =
    GuiState.updateCursor holeId
    <> GuiState.updateWidgetState holeId state
    where
        holeId = WidgetIds.make entityId & WidgetIds.hidOpen

readSearchTerm ::
    (MonadReader env m, GuiState.HasWidgetState env) => WidgetIds -> m Text
readSearchTerm widgetIds =
    WidgetIds.hidOpen widgetIds & GuiState.readWidgetState
    <&> fromMaybe ""
