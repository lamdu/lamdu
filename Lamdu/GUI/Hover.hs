-- | The dark background is meant to be added around the set of
-- rectangular shaped hovering UI elements (e.g: annotations)
{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Lamdu.GUI.Hover
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
addBackground myId = Widget.backgroundColor (myId <> ["hover background"])

addDarkBackground :: Monad m => AnimId -> ExprGuiM m (ExpressionGui f -> ExpressionGui f)
addDarkBackground animId =
    do
        config <- ExprGuiM.readConfig
        return $ \gui ->
            gui
            & TreeLayout.pad (Config.hoverDarkPadding config <&> realToFrac)
            & TreeLayout.widget %~
              Widget.backgroundColor
              (animId <> ["hover dark background"])
              (Config.hoverDarkBGColor config)
