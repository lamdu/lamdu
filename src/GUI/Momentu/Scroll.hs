module GUI.Momentu.Scroll
    ( focusAreaInto
    ) where

import qualified Control.Lens as Lens
import qualified GUI.Momentu.Direction as Dir
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.Rect as Rect
import           GUI.Momentu.Widget (Widget(..))
import qualified GUI.Momentu.Widget as Widget

import           GUI.Momentu.Prelude

-- Focus area into the given region
focusAreaInto ::
    Functor f =>
    Widget.Size -> Widget f -> Widget f
focusAreaInto regionSize widget =
    Element.pad Dir.LeftToRight translation2d
    (regionSize - widget ^. Element.size - translation2d)
    widget
    where
        translation2d =
            translation1d <$> widget ^. Element.size <*> regionSize <*> focalCenter
        translation1d widgetSz regionSz focalPt
            | widgetSz > regionSz = -max 0 translation
            | otherwise = 0
            where
                translation = min (focalPt - regionCenter) (widgetSz - regionSz)
                regionCenter = regionSz / 2
        focalCenter =
            widget ^? Widget.wState . Widget._StateFocused
            <&> (surrounding &)
            >>= (^? Widget.fFocalAreas . Lens.element 0 . Rect.center)
            & fromMaybe 0
        extraSize = max 0 (regionSize - widget ^. Element.size)
        surrounding =
            Widget.Surrounding
            { Widget._sLeft = 0
            , Widget._sTop = 0
            , Widget._sRight = extraSize ^. _1
            , Widget._sBottom = extraSize ^. _2
            }
