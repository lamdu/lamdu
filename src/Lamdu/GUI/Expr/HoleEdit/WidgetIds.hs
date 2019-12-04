-- | The widget ids of exposed hole components
module Lamdu.GUI.Expr.HoleEdit.WidgetIds
    ( WidgetIds(..), make, makeFrom
    , isActive
    ) where

import qualified GUI.Momentu.State as GuiState
import qualified GUI.Momentu.Widget as Widget
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Sugar.EntityId (EntityId)

import           Lamdu.Prelude

data WidgetIds = WidgetIds
    { hidClosed :: Widget.Id
    , hidOpen   :: Widget.Id
    } deriving Show

make :: EntityId -> WidgetIds
make = makeFrom . WidgetIds.fromEntityId

makeFrom :: Widget.Id -> WidgetIds
makeFrom wId = WidgetIds
    { hidClosed = wId
    , hidOpen   = Widget.joinId wId ["Open"]
    }

isActive :: (MonadReader env m, GuiState.HasCursor env) => WidgetIds -> m Bool
isActive widgetIds = GuiState.isSubCursor ?? hidOpen widgetIds
