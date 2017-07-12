{-# LANGUAGE FlexibleContexts #-}
-- | The dark background is meant to be added around the set of
-- rectangular shaped hovering UI elements (e.g: annotations)
{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Lamdu.GUI.Hover
    ( addBackground, addDarkBackground
    ) where

import qualified Control.Lens as Lens
import qualified Graphics.DrawingCombinators as Draw
import           Graphics.UI.Bottle.Animation (AnimId)
import qualified Graphics.UI.Bottle.View as View
import qualified Lamdu.Config.Theme as Theme

import           Lamdu.Prelude

addBackground :: View.SetLayers a => AnimId -> Draw.Color -> a -> a
addBackground myId = View.backgroundColor (myId <> ["hover background"])

addDarkBackground ::
    (Theme.HasTheme env, View.Resizable a, MonadReader env m) => AnimId -> m (a -> a)
addDarkBackground animId =
    Lens.view Theme.theme
    <&>
    \theme gui ->
    gui
    & View.pad (Theme.hoverDarkPadding theme <&> realToFrac)
    & View.backgroundColor
        (animId <> ["hover dark background"])
        (Theme.hoverDarkBGColor theme)
