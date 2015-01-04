{-# LANGUAGE OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.HoleEdit.Common
  ( addBackground, diveIntoHole
  , searchTermWIdOfHoleEntityId
  ) where

import Data.Monoid ((<>))
import Graphics.UI.Bottle.Widget (Widget)
import Lamdu.Sugar.EntityId (EntityId)
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Lamdu.Config as Config
import qualified Lamdu.GUI.WidgetIds as WidgetIds

searchTermWIdOfHoleEntityId :: EntityId -> Widget.Id
searchTermWIdOfHoleEntityId =
    WidgetIds.searchTermId . FocusDelegator.delegatingId .
    WidgetIds.fromEntityId

addBackground :: Widget.Id -> Config.Layers -> Draw.Color -> Widget f -> Widget f
addBackground myId layers =
  Widget.backgroundColor (Config.layerHoleBG layers)
  (Widget.toAnimId myId <> ["hole background"])

-- TODO: We no longer use a FocusDelegator, use a different id
-- manipulation function
diveIntoHole :: Widget.Id -> Widget.Id
diveIntoHole = FocusDelegator.delegatingId
