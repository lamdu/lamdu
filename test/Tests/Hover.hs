module Tests.Hover where

import           Data.Vector.Vector2 (Vector2(..))
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.Hover as H
import qualified GUI.Momentu.Rect as R
import           GUI.Momentu.State (Update)
import qualified GUI.Momentu.Widget as W
import           Test.Momentu.Env (env)

import           Test.Lamdu.Prelude

square :: W.Widget (Identity Update)
square = Element.padAround 0.5 Element.empty

focusedSquare :: W.Widget (Identity Update)
focusedSquare = W.setFocused square

testHover :: Vector2 Double -> Vector2 Double -> W.Widget (Identity Update) -> IO ()
testHover topLeft bottomRight widget =
    (padded ^?! W.wState . W._StateFocused) (W.Surrounding 0 0 0 0)
    ^. W.fFocalAreas
    & traverse_ checkFocal
    where
        checkFocal a
            | rectWithin a paddedRect = pure ()
            | otherwise =
                assertString ("hover out of screen: " <> show (a, paddedRect))
        padded = Element.pad env topLeft bottomRight widget
        paddedRect = R.Rect 0 (padded ^. W.wSize)
        rectWithin inner outer =
            R.isWithin (inner ^. R.topLeft) outer &&
            R.isWithin (inner ^. R.bottomRight) outer

test :: Test
test =
    do
        testHover 0 (Vector2 2 2) widget
        testHover (Vector2 2 2) 0 widget
    & testCase "hover"
    where
        widget =
            H.hoverInPlaceOf
            (H.hoverBesideOptions env (H.hover env focusedSquare) anchor)
            anchor
        anchor = H.anchor env square
