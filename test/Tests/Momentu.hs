module Tests.Momentu where

import qualified Control.Lens as Lens
import           Data.Binary.Extended (encodeS, decodeS)
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Vector.Vector2 (Vector2(..))
import           GUI.Momentu.Align (Aligned(..))
import qualified GUI.Momentu.Align as Align
import           GUI.Momentu.Animation (R)
import qualified GUI.Momentu.Direction as Dir
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.Main.Events (KeyEvent(..))
import           GUI.Momentu.MetaKey (MetaKey(..))
import qualified GUI.Momentu.MetaKey as MetaKey
import           GUI.Momentu.Rect (Rect(Rect))
import qualified GUI.Momentu.Rect as Rect
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.Responsive.Options as Options
import           GUI.Momentu.State (Gui, GUIState(..), VirtualCursor(..))
import qualified GUI.Momentu.State as State
import           GUI.Momentu.View (View(..))
import qualified GUI.Momentu.View as View
import           GUI.Momentu.Widget (Widget)
import qualified GUI.Momentu.Widget as W
import qualified GUI.Momentu.Widgets.Grid as Grid
import qualified GUI.Momentu.Widgets.GridView as GridView
import qualified Graphics.UI.GLFW as GLFW
import           Test.Momentu.Env (env)

import           Test.Lamdu.Prelude

test :: Test
test =
    testGroup "Momentu"
    [ testCase "vertical-disambguation" verticalDisambigTest
    , testProperty "grid-sensible-size" propGridSensibleSize
        & plusTestOptions mempty
        { topt_maximum_generated_tests = Just 1000
        }
    , testCase "grid-stroll" gridStrollTest
    ]

propGridSensibleSize :: NonEmpty (NonEmpty (Aligned (Vector2 R))) -> Bool
propGridSensibleSize viewConfs =
    size == grid ^. Element.size &&
    isFinite (size ^. _1) && isFinite (size ^. _2) &&
    and ((>=) <$> size <*> minGridSize views) &&
    Lens.allOf (traverse . traverse) goodPlacement placements &&
    Lens.allOf (traverse . traverse . Align.alignmentRatio . traverse) goodAlignment alignments
    where
        isFinite x = not (isNaN x || isInfinite x)
        views = viewsFromConf viewConfs
        (alignments, grid) = GridView.make Dir.LeftToRight views
        (size, placements) = GridView.makePlacements views
        goodPlacement (Aligned alignment (place, view)) =
            vSize == place ^. Rect.size &&
            pos vSize && pos (place ^. Rect.topLeft) &&
            and ((<=) <$> place ^. Rect.bottomRight <*> size * 1.001) &&
            Lens.allOf traverse goodAlignment alignment
            where
                pos x = and ((<=) <$> 0 <*> x)
                vSize = view ^. Element.size
        goodAlignment x = isFinite x && 0 <= x && x <= 1

minGridSize ::
    (Traversable vert, Element.SizedElement view) =>
    vert (NonEmpty (Aligned view)) -> Vector2 R
minGridSize views =
    Vector2 (sum colWidths) (sum rowHeights) * 0.999 -- Due to float inaccuracies
    where
        colWidths =
            views <&> Lens.mapped %~ (^. Element.width)
            & foldl1 (NonEmpty.zipWith max)
        rowHeights = views <&> Lens.mapped %~ (^. Element.height) <&> maximum

viewsFromConf :: NonEmpty (NonEmpty (Aligned (Vector2 R))) -> NonEmpty (NonEmpty (Aligned View))
viewsFromConf viewConfs =
    viewConfs
    <&> onTail (take minRowTailLen)
    <&> traverse . Align.value %~ (`View` mempty)
    where
        minRowTailLen = minimum (viewConfs ^.. traverse <&> NonEmpty.tail <&> length)
        onTail f (x :| xs) = x :| f xs

simpleKeyEvent :: MetaKey -> E.Event
simpleKeyEvent (MetaKey mods key) =
    E.EventKey KeyEvent
    { keKey = key
    , keScanCode = 0 -- dummy
    , keModKeys = MetaKey.toGLFWModifiers mods
    , keState = GLFW.KeyState'Pressed
    }

gridStrollTest :: IO ()
gridStrollTest =
    makeGrid 0
    & testStroll W.strollAheadKeys 1
    >>= testStroll W.strollAheadKeys 2
    >>= testStroll W.strollAheadKeys 3
    >>= testStroll W.strollBackKeys 2
    >>= testStroll W.strollBackKeys 1
    >>= testStroll W.strollBackKeys 0
    & void
    where
        makeGrid :: Int -> Gui Widget Identity
        makeGrid pos =
            Grid.make Dir.LeftToRight
            [ [ Aligned 0 (mkWidget pos 0)
              , Aligned 0 (mkWidget pos 1)
              ]
            , [ Aligned 0 (mkWidget pos 2)
              , Aligned 0 (mkWidget pos 3)
              ]
            ] & snd
        eventCtx = W.EventContext (VirtualCursor (Rect 0 0)) ""
        getEventMap :: Gui Widget Identity -> Gui E.EventMap Identity
        getEventMap w =
            w ^. W.wState . W._StateFocused .
            Lens.to ($ W.Surrounding 0 0 0 0) .
            W.fEventMap . Lens.to ($ eventCtx)
        fromCursor ~(W.Id [bs]) = decodeS bs
        toCursor i = W.Id [encodeS i]
        keyEventTarget msg keys w =
            getEventMap w
            & E.lookup (Identity Nothing) (simpleKeyEvent (head keys))
            & runIdentity
            & fromMaybe (error (msg ++ ": has no key mapping"))
            & (^. E.dhHandler)
            & runIdentity
            & (^?! State.uCursor . Lens._Wrapped . Lens._Just)
            & fromCursor
        testStroll keys idx w =
            do
                assertEqual "Stroll dest" idx
                    (keyEventTarget
                     ("Strolling to " ++ show idx ++ " via " ++ show keys) keys w)
                pure (makeGrid idx)
        mkWidget :: Int -> Int -> Gui Widget Identity
        mkWidget pos i =
            W.makeFocusableView
            GUIState
            { _sCursor = toCursor pos
            , _sWidgetStates = mempty
            } myId (View.make 1 mempty)
            & W.takesStroll myId
            where
                myId = toCursor i

verticalDisambigTest :: IO ()
verticalDisambigTest =
    do
        check False (Vector2 1 2)
        check True (Vector2 1.5 2)
    where
        check needDisamb expect
            | size == expect = pure ()
            | otherwise =
                assertString ("unexpected size " <> show size <> ", expected " <> show expect)
            where
                size = rendered ^. Align.tValue . W.wSize
                rendered =
                    (box ^. Responsive.rNarrow)
                    Responsive.NarrowLayoutParams
                    { Responsive._layoutWidth = 1.9
                    , Responsive._layoutNeedDisambiguation = needDisamb
                    }
                box =
                    Options.box env disambig [unitItem, unitItem]
                    <&> (<>[]) -- to avoid ambiguous type var
        unitItem = Element.pad Dir.LeftToRight 0 1 Element.empty
        disambig =
            Options.disambiguationNone
            & Options.disambVert .~ Element.pad Dir.LeftToRight (Vector2 0.5 0) 0
