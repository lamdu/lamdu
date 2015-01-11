{-# LANGUAGE OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.HoleEdit.Common
  ( addBackground
  ) where

import Data.Monoid ((<>))
import Graphics.UI.Bottle.Widget (Widget)
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Lamdu.Config as Config

addBackground :: Widget.Id -> Config.Layers -> Draw.Color -> Widget f -> Widget f
addBackground myId layers =
  Widget.backgroundColor (Config.layerHoleBG layers)
  (Widget.toAnimId myId <> ["hole background"])
