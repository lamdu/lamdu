{-# LANGUAGE NoImplicitPrelude #-}
module Lamdu.GUI.Scroll
    ( focusAreaIntoWindow
    ) where

import qualified Control.Lens as Lens
import           Data.Vector.Vector2 (Vector2(..))
import qualified Graphics.UI.Bottle.Rect as Rect
import qualified Graphics.UI.Bottle.View as View
import           Graphics.UI.Bottle.Widget (Widget)
import qualified Graphics.UI.Bottle.Widget as Widget

import           Lamdu.Prelude

focusAreaIntoWindow :: Widget.Size -> Widget f -> Widget f
focusAreaIntoWindow winSize widget =
    widget
    & intoWindow _1
    & intoWindow _2
    where
        widgetSize = widget ^. View.size
        center = winSize / 2
        allowedScroll = winSize - widgetSize
        intoWindow rawLens
            | widgetSize ^. l > winSize ^. l && movement < 0 =
              Widget.translate (0 & l .~ max (allowedScroll ^. l) movement)
            | otherwise = id
            where
                movement = center ^. l - focalPoint ^. l
                l :: Lens' (Vector2 Widget.R) Widget.R
                l = Lens.cloneLens rawLens
        focalPoint =
            widget ^? Widget.mFocus . Lens._Just . Widget.focalArea
            <&> (^. Rect.center)
            & fromMaybe 0
