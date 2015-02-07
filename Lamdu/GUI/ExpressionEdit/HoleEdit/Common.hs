{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.HoleEdit.Common
    ( addBackground, addDarkBackground
    , openHoleEventMap
    ) where

import           Control.Applicative (Applicative(..))
import           Control.Lens.Operators
import           Control.MonadA (MonadA)
import           Data.Monoid ((<>))
import qualified Graphics.DrawingCombinators as Draw
import           Graphics.UI.Bottle.Animation (AnimId)
import qualified Graphics.UI.Bottle.EventMap as E
import           Graphics.UI.Bottle.ModKey (ModKey(..))
import           Graphics.UI.Bottle.Widget (Widget)
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Lamdu.Config as Config
import           Lamdu.GUI.ExpressionEdit.HoleEdit.WidgetIds (WidgetIds(..))
import           Lamdu.GUI.ExpressionGui (ExpressionGui)
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM

openHoleEventMap ::
    Applicative f => [ModKey] -> WidgetIds -> Widget.EventHandlers f
openHoleEventMap keys WidgetIds{..} =
    Widget.keysEventMapMovesCursor keys doc $ pure hidOpen
    where
        doc = E.Doc ["Navigation", "Hole", "Open"]

addBackground :: AnimId -> Config.Layers -> Draw.Color -> Widget f -> Widget f
addBackground myId layers =
    Widget.backgroundColor (Config.layerHoleBG layers)
    (myId <> ["hole background"])

addDarkBackground :: MonadA m => AnimId -> ExpressionGui f -> ExprGuiM m (ExpressionGui f)
addDarkBackground animId widget =
    do
        config <- ExprGuiM.readConfig
        let Config.Hole{..} = Config.hole config
        widget
            & ExpressionGui.pad (holeDarkPadding <&> realToFrac)
            & ExpressionGui.egWidget %~
              Widget.backgroundColor
              (Config.layerDarkHoleBG (Config.layers config))
              (animId <> ["hole dark background"])
              holeDarkBGColor
            & return
