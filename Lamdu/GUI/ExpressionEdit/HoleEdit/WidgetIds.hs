-- | The widget ids of exposed hole components
{-# LANGUAGE OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.HoleEdit.WidgetIds
  ( openHoleId
  , openSearchTermId
  , resultsPrefixId
  ) where

import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Sugar.EntityId (EntityId)

openHoleId :: Widget.Id -> Widget.Id
openHoleId = (`Widget.joinId` ["OpenHole"])

openSearchTermId :: EntityId -> Widget.Id
openSearchTermId =
  (`Widget.joinId` ["SearchTerm"]) . openHoleId . WidgetIds.fromEntityId

resultsPrefixId :: EntityId -> Widget.Id
resultsPrefixId =
  (`Widget.joinId` ["results"]) . openHoleId . WidgetIds.fromEntityId
