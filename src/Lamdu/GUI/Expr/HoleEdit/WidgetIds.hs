-- | The widget ids of exposed hole components
module Lamdu.GUI.Expr.HoleEdit.WidgetIds
    ( WidgetIds(..), make
    , isActive
    ) where

import qualified GUI.Momentu.State as GuiState
import qualified GUI.Momentu.Widget as Widget
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Sugar.EntityId (EntityId)

import           Lamdu.Prelude

data WidgetIds = WidgetIds
    { hidHole   :: Widget.Id
    , hidClosed :: Widget.Id
    , hidOpen   :: Widget.Id
    } deriving Show

make :: EntityId -> WidgetIds
make entityId = WidgetIds
    { hidHole   = holeId
    , hidClosed = Widget.joinId holeId ["Closed"]
    , hidOpen   = Widget.joinId holeId ["Open"]
    }
    where
        holeId = WidgetIds.fromEntityId entityId

isActive :: (MonadReader env m, GuiState.HasCursor env) => WidgetIds -> m Bool
isActive widgetIds = GuiState.isSubCursor ?? hidOpen widgetIds
