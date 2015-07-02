module Lamdu.GUI.Scroll
    ( focusAreaIntoWindow
    ) where

import           Control.Lens (Lens')
import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Lens.Tuple
import           Data.Vector.Vector2 (Vector2(..))
import qualified Graphics.UI.Bottle.Rect as Rect
import           Graphics.UI.Bottle.Widget (Widget)
import qualified Graphics.UI.Bottle.Widget as Widget

focusAreaIntoWindow :: Widget.Size -> Widget f -> Widget f
focusAreaIntoWindow winSize widget =
    widget
    & intoWindow _1
    & intoWindow _2
    where
        widgetSize = widget ^. Widget.size
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
        focalPoint = widget ^. Widget.focalArea . Rect.center
