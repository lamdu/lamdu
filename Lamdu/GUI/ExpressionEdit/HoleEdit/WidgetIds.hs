-- | The widget ids of exposed hole components
{-# LANGUAGE OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.HoleEdit.WidgetIds
  ( WidgetIds(..), make
  ) where

import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Sugar.EntityId (EntityId)

data WidgetIds = WidgetIds
    { hidHole :: Widget.Id
    , hidOpen :: Widget.Id
    , hidClosed :: Widget.Id
    , hidOpenSearchTerm :: Widget.Id
    , hidClosedSearchTerm :: Widget.Id
    , hidResultsPrefix :: Widget.Id
    }

make :: EntityId -> WidgetIds
make entityId = WidgetIds
    { hidHole = holeId
    , hidOpen = openPrefix
    , hidClosed = closedPrefix
    , hidOpenSearchTerm = Widget.joinId openPrefix ["SearchTerm"]
    , hidClosedSearchTerm = Widget.joinId closedPrefix ["SearchTerm"]
    , hidResultsPrefix = Widget.joinId openPrefix ["Results"]
    }
    where
        closedPrefix = Widget.joinId holeId ["Closed"]
        openPrefix = Widget.joinId holeId ["Open"]
        holeId = WidgetIds.fromEntityId entityId
