{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.HoleEdit.Common
  ( addBackground, openHoleEventMap
  ) where

import           Control.Applicative (Applicative(..))
import           Data.Monoid ((<>))
import qualified Graphics.DrawingCombinators as Draw
import           Graphics.UI.Bottle.Animation (AnimId)
import qualified Graphics.UI.Bottle.EventMap as E
import           Graphics.UI.Bottle.ModKey (ModKey(..))
import           Graphics.UI.Bottle.Widget (Widget)
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Lamdu.Config as Config
import           Lamdu.GUI.ExpressionEdit.HoleEdit.Info (HoleIds(..))

openHoleEventMap ::
  Applicative f => [ModKey] -> HoleIds -> Widget.EventHandlers f
openHoleEventMap keys HoleIds{..} =
  Widget.keysEventMapMovesCursor keys doc $ pure hidOpen
  where
    doc = E.Doc ["Navigation", "Hole", "Open"]

addBackground :: AnimId -> Config.Layers -> Draw.Color -> Widget f -> Widget f
addBackground myId layers =
  Widget.backgroundColor (Config.layerHoleBG layers)
  (myId <> ["hole background"])
