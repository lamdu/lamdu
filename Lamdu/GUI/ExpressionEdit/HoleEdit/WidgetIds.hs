-- | The widget ids of exposed hole components
{-# LANGUAGE OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.HoleEdit.WidgetIds
  ( openHoleId
  , searchTermWIdOfHoleEntityId
  ) where

import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Sugar.EntityId (EntityId)

openHoleId :: Widget.Id -> Widget.Id
openHoleId = (`Widget.joinId` ["OpenHole"])

searchTermWIdOfHoleEntityId :: EntityId -> Widget.Id
searchTermWIdOfHoleEntityId =
  WidgetIds.searchTermId . openHoleId .
  WidgetIds.fromEntityId
