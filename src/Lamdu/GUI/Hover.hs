-- | The dark background is meant to be added around the set of
-- rectangular shaped hovering UI elements (e.g: annotations)
{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Lamdu.GUI.Hover
    ( addBackground, addDarkBackground
    ) where

import qualified Graphics.DrawingCombinators as Draw
import           Graphics.UI.Bottle.Animation (AnimId)
import qualified Graphics.UI.Bottle.View as View
import           Graphics.UI.Bottle.Widget (Widget)
import qualified Graphics.UI.Bottle.Widget.Layout as Layout
import           Graphics.UI.Bottle.Widget.Layout (Layout)
import qualified Lamdu.Config.Theme as Theme
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM

import           Lamdu.Prelude

addBackground :: AnimId -> Draw.Color -> Widget f -> Widget f
addBackground myId = View.backgroundColor (myId <> ["hover background"])

addDarkBackground :: (Monad m, Layout w) => AnimId -> ExprGuiM m (w a -> w a)
addDarkBackground animId =
    do
        theme <- ExprGuiM.readTheme
        return $ \gui ->
            gui
            & Layout.pad (Theme.hoverDarkPadding theme <&> realToFrac)
            & Layout.widget %~
                View.backgroundColor
                (animId <> ["hover dark background"])
                (Theme.hoverDarkBGColor theme)
