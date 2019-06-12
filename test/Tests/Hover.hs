module Tests.Hover where

import qualified Control.Lens as Lens
import           Data.Vector.Vector2 (Vector2(..))
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.Glue ((/-/))
import qualified GUI.Momentu.Main.Events as Events
import qualified GUI.Momentu.Hover as H
import qualified GUI.Momentu.ModKey as ModKey
import qualified GUI.Momentu.Rect as R
import           GUI.Momentu.State (VirtualCursor(..))
import qualified GUI.Momentu.Widget as W
import           Test.Momentu.Env (env)

import           Test.Lamdu.Prelude

square :: W.Widget Identity
square = Element.padAround 0.5 Element.empty

focusedSquare :: W.Widget Identity
focusedSquare = W.setFocused square

testHover :: Vector2 Double -> Vector2 Double -> W.Widget Identity -> IO ()
testHover topLeft bottomRight widget =
    do
        traverse_ checkFocal focalAreas
        when (hoverIsAbove && not (hasKeyEvent ModKey.Key'Down))
            (assertString "Navigation down from hover not working!")
    where
        rendered = (padded ^?! W.wState . W._StateFocused) (W.Surrounding 0 0 0 0)
        focalAreas = rendered ^. W.fFocalAreas
        checkFocal a
            | rectWithin a paddedRect = pure ()
            | otherwise =
                assertString ("hover out of screen: " <> show (a, paddedRect))
        padded = Element.pad env topLeft bottomRight widget
        paddedRect = R.Rect 0 (padded ^. W.wSize)
        rectWithin inner outer =
            R.isWithin (inner ^. R.topLeft) outer &&
            R.isWithin (inner ^. R.bottomRight) outer
        hoverIsAbove =
            focalAreas ^? Lens.ix 0 . R.top >= focalAreas ^? Lens.ix 1 . R.bottom
        eventMap =
            (rendered ^. W.fEventMap)
            (W.EventContext (VirtualCursor (R.Rect 0 1)) "")
        lookupEvent e = E.lookup (Identity Nothing) e eventMap ^. Lens._Wrapped
        hasKeyEvent = Lens.has Lens._Just . lookupEvent . E.EventKey . mkKeyEvent
        mkKeyEvent k =
            Events.KeyEvent
            { Events.keKey = k
            , Events.keScanCode = 0 -- dummy
            , Events.keState = ModKey.KeyState'Pressed
            , Events.keModKeys = mempty
            }

positioningTest :: Test
positioningTest =
    do
        testHover 0 (Vector2 2 2) widget
        testHover (Vector2 2 2) 0 widget
    & testCase "positioning"
    where
        widget =
            H.hoverInPlaceOf
            (H.hoverBesideOptions env (H.hover env focusedSquare) anchor)
            anchor
        anchor =
            square
            & W.takesFocus (const (pure "blah"))
            & H.anchor env

anchorPointTest :: Test
anchorPointTest =
    assertEqual "unexpected anchor point" (widget ^. H.unHover . H.anchorPoint) (Vector2 0 1)
    & testCase "anchor-point"
    where
        widget =
            (H.hover ?? square) /-/ (H.anchor ?? square)
            $ env

test :: Test
test =
    testGroup "hover"
    [ positioningTest
    , anchorPointTest
    ]
