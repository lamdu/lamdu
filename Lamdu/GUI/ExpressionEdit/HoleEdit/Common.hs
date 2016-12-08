{-# LANGUAGE NoImplicitPrelude, RecordWildCards, OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.HoleEdit.Common
    ( addBackground, addDarkBackground
    ) where

import qualified Graphics.DrawingCombinators as Draw
import           Graphics.UI.Bottle.Animation (AnimId)
import           Graphics.UI.Bottle.Widget (Widget)
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widget.TreeLayout as TreeLayout
import qualified Lamdu.Config as Config
import           Lamdu.GUI.ExpressionGui (ExpressionGui)
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM

import           Lamdu.Prelude

addBackground :: AnimId -> Draw.Color -> Widget f -> Widget f
addBackground myId = Widget.backgroundColor (myId <> ["hole background"])

addDarkBackground :: Monad m => AnimId -> ExprGuiM m (ExpressionGui f -> ExpressionGui f)
addDarkBackground animId =
    do
        config <- ExprGuiM.readConfig
        let Config.Hole{..} = Config.hole config
        return $ \gui ->
            gui
            & TreeLayout.pad (holeDarkPadding <&> realToFrac)
            & TreeLayout.widget %~
              Widget.backgroundColor
              (animId <> ["hole dark background"])
              holeDarkBGColor
