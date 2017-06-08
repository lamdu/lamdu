-- | The dark background is meant to be added around the set of
-- rectangular shaped hovering UI elements (e.g: annotations)
{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Lamdu.GUI.Hover
    ( addBackground, addDarkBackground
    ) where

import qualified Graphics.DrawingCombinators as Draw
import           Graphics.UI.Bottle.Animation (AnimId)
import qualified Graphics.UI.Bottle.View as View
import qualified Lamdu.Config.Theme as Theme
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM

import           Lamdu.Prelude

addBackground :: View.MkView a => AnimId -> Draw.Color -> a -> a
addBackground myId = View.backgroundColor (myId <> ["hover background"])

addDarkBackground ::
    (Monad m, View.MkView (w a)) =>
    AnimId -> ExprGuiM m (w a -> w a)
addDarkBackground animId =
    ExprGuiM.readTheme
    <&>
    \theme gui ->
    gui
    & View.pad (Theme.hoverDarkPadding theme <&> realToFrac)
    & View.backgroundColor
        (animId <> ["hover dark background"])
        (Theme.hoverDarkBGColor theme)
