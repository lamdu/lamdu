{-# LANGUAGE NoImplicitPrelude #-}
module Lamdu.GUI.Scroll
    ( focusAreaIntoWindow
    ) where

import qualified Control.Lens as Lens
import           Data.Vector.Vector2 (Vector2(..))
import qualified Graphics.UI.Bottle.Rect as Rect
import qualified Graphics.UI.Bottle.View as View
import           Graphics.UI.Bottle.Widget (Widget(..))
import qualified Graphics.UI.Bottle.Widget as Widget

import           Lamdu.Prelude

focusAreaIntoWindow ::
    Functor f =>
    Widget.Size -> Widget (f Widget.EventResult) -> Widget (f Widget.EventResult)
focusAreaIntoWindow winSize widget =
    widget
    & intoWindow _1
    & intoWindow _2
    where
        widgetSize = widget ^. View.size
        center = winSize / 2
        allowedScroll = winSize - widgetSize
        intoWindow rawLens w
            | widgetSize ^. l > winSize ^. l && movement < 0 =
              w
              & Widget.wState .~ Widget.translate (0 & l .~ max (allowedScroll ^. l) movement) w
              & View.size .~ winSize
            | otherwise = w
            where
                movement = center ^. l - focalPoint ^. l
                l :: Lens' (Vector2 Widget.R) Widget.R
                l = Lens.cloneLens rawLens
        surrounding =
            Widget.Surrounding
            { Widget._sLeft = 0
            , Widget._sTop = 0
            , Widget._sRight = extraSize ^. _1
            , Widget._sBottom = extraSize ^. _2
            }
        extraSize = max 0 (winSize - widgetSize)
        focalPoint =
            widget ^? Widget.wState . Widget._StateFocused
            <&> (surrounding &)
            <&> (^. Widget.fFocalArea . Rect.center)
            & fromMaybe 0
