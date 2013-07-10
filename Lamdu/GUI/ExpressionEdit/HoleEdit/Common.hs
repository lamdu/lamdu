{-# LANGUAGE OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.HoleEdit.Common
  ( makeBackground, diveIntoHole
  , searchTermWIdOfHoleGuid
  ) where

import Data.Monoid (Monoid(..))
import Data.Store.Guid (Guid)
import Graphics.UI.Bottle.Widget (Widget)
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Lamdu.GUI.WidgetIds as WidgetIds

searchTermWIdOfHoleGuid :: Guid -> Widget.Id
searchTermWIdOfHoleGuid = WidgetIds.searchTermId . FocusDelegator.delegatingId . WidgetIds.fromGuid

makeBackground :: Widget.Id -> Int -> Draw.Color -> Widget f -> Widget f
makeBackground myId level =
  Widget.backgroundColor level $
  mappend (Widget.toAnimId myId) ["hole background"]

-- TODO: We no longer use a FocusDelegator, use a different id
-- manipulation function
diveIntoHole :: Widget.Id -> Widget.Id
diveIntoHole = FocusDelegator.delegatingId
