-- | The dark background is meant to be added around the set of
-- rectangular shaped hovering UI elements (e.g: annotations)
{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Lamdu.GUI.Hover
    ( addBackground, addDarkBackground
    ) where

import qualified Control.Lens as Lens
import           GUI.Momentu.Animation (AnimId)
import qualified GUI.Momentu.Draw as Draw
import           GUI.Momentu.Element (Element)
import qualified GUI.Momentu.Element as Element
import qualified Lamdu.Config.Theme as Theme

import           Lamdu.Prelude

addBackground :: Element a => AnimId -> Draw.Color -> a -> a
addBackground myId = Draw.backgroundColor (myId <> ["hover background"])

addDarkBackground ::
    (Theme.HasTheme env, Element a, MonadReader env m) => AnimId -> m (a -> a)
addDarkBackground animId =
    Lens.view Theme.theme
    <&>
    \theme gui ->
    gui
    & Element.pad (Theme.hoverDarkPadding theme <&> realToFrac)
    & Draw.backgroundColor
        (animId <> ["hover dark background"])
        (Theme.hoverDarkBGColor theme)
