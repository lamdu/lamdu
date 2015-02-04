{-# LANGUAGE OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.HoleEdit.Common
  ( addBackground
  ) where

import           Data.Monoid ((<>))
import qualified Graphics.DrawingCombinators as Draw
import           Graphics.UI.Bottle.Animation (AnimId)
import           Graphics.UI.Bottle.Widget (Widget)
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Lamdu.Config as Config

addBackground :: AnimId -> Config.Layers -> Draw.Color -> Widget f -> Widget f
addBackground myId layers =
  Widget.backgroundColor (Config.layerHoleBG layers)
  (myId <> ["hole background"])
