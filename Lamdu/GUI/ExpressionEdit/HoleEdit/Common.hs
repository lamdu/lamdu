{-# LANGUAGE OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.HoleEdit.Common
  ( makeBackground
  ) where

import Data.Monoid (Monoid(..))
import Graphics.UI.Bottle.Widget (Widget)
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.Widget as Widget

makeBackground :: Widget.Id -> Int -> Draw.Color -> Widget f -> Widget f
makeBackground myId level =
  Widget.backgroundColor level $
  mappend (Widget.toAnimId myId) ["hole background"]
