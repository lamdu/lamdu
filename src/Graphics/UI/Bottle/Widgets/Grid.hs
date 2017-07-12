{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, RecordWildCards, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Graphics.UI.Bottle.Widgets.Grid
    ( make, makeWithKeys
    , Alignment(..)
    , Cursor
    , Keys(..), stdKeys
    ) where

import           Control.Applicative (liftA2)
import qualified Control.Lens as Lens
import           Control.Monad (msum)
import           Data.Foldable (toList)
import           Data.List (foldl', transpose, find, sortOn)
import           Data.List.Utils (groupOn, minimumOn)
import           Data.MRUMemo (memo)
import           Data.Vector.Vector2 (Vector2(..))
import qualified Data.Vector.Vector2 as Vector2
import           Graphics.UI.Bottle.Direction (Direction)
import qualified Graphics.UI.Bottle.Direction as Direction
import qualified Graphics.UI.Bottle.EventMap as EventMap
import           Graphics.UI.Bottle.MetaKey (MetaKey(..), noMods)
import qualified Graphics.UI.Bottle.MetaKey as MetaKey
import           Graphics.UI.Bottle.ModKey (ModKey)
import           Graphics.UI.Bottle.Rect (Rect(..))
import qualified Graphics.UI.Bottle.Rect as Rect
import qualified Graphics.UI.Bottle.View as View
import           Graphics.UI.Bottle.Widget (R, Widget(Widget))
import qualified Graphics.UI.Bottle.Widget as Widget
import           Graphics.UI.Bottle.Widgets.GridView (Alignment(..))
import qualified Graphics.UI.Bottle.Widgets.GridView as GridView
import           Graphics.UI.Bottle.Widgets.StdKeys (DirKeys(..), stdDirKeys)
import qualified Graphics.UI.GLFW as GLFW

import           Lamdu.Prelude

type Cursor = Vector2 Int

length2d :: (Foldable vert, Foldable horiz) => vert (horiz a) -> Vector2 Int
length2d xs = Vector2 (foldl' max 0 (toList xs <&> length)) (length xs)

capCursor :: Vector2 Int -> Vector2 Int -> Vector2 Int
capCursor size = fmap (max 0) . liftA2 min (subtract 1 <$> size)

data NavDests a = NavDests
    { leftOfCursor
    , aboveCursor
    , rightOfCursor
    , belowCursor
    , topCursor
    , leftMostCursor
    , bottomCursor
    , rightMostCursor :: Maybe (Widget.EnterResult a)
    }

mkNavDests ::
    Functor f =>
    Widget.Size -> Cursor -> Widget.VirtualCursor ->
    [[Maybe (Direction -> Widget.EnterResult (f Widget.EventResult))]] ->
    NavDests (f Widget.EventResult)
mkNavDests widgetSize cursor@(Vector2 cursorX cursorY) virtCursor mEnterss =
    NavDests
    { leftOfCursor    = take cursorX curRow    & reverse & enterFromPrevArea <&> setHVirt
    , aboveCursor     = take cursorY curColumn & reverse & enterFromPrevArea <&> setVVirt
    , rightOfCursor   = drop (cursorX+1) curRow          & enterFromPrevArea <&> setHVirt
    , belowCursor     = drop (cursorY+1) curColumn       & enterFromPrevArea <&> setVVirt

    , topCursor       = take (min 1 cursorY) curColumn                & enterFromEdge (Vector2 Nothing (Just 0)) <&> setVVirt
    , leftMostCursor  = take (min 1 cursorX) curRow                   & enterFromEdge (Vector2 (Just 0) Nothing) <&> setHVirt
    , bottomCursor    = drop (cursorY+1) curColumn & reverse & take 1 & enterFromEdge (Vector2 Nothing (Just 1)) <&> setVVirt
    , rightMostCursor = drop (cursorX+1) curRow    & reverse & take 1 & enterFromEdge (Vector2 (Just 1) Nothing) <&> setHVirt
    }
    where
        setHVirt = setVirt Rect.verticalRange
        setVVirt = setVirt Rect.horizontalRange
        setVirt axis enterResult =
            enterResult
            & Widget.enterResultEvent . Lens.mapped . Widget.eVirtualCursor . Lens._Wrapped ?~
            Widget.NewVirtualCursor
            ( enterResult ^. Widget.enterResultRect
                & Lens.cloneLens axis .~ prevArea ^. Lens.cloneLens axis
                & Widget.VirtualCursor
            )
        curRow = fromMaybe [] $ mEnterss ^? Lens.ix cappedY
        curColumn = fromMaybe [] $ transpose mEnterss ^? Lens.ix cappedX
        Vector2 cappedX cappedY = capCursor size cursor
        size = length2d mEnterss

        prevArea = virtCursor ^. Widget.virtualCursor
        enterFrom rect mEnters = mEnters & msum ?? Direction.PrevFocalArea rect
        enterFromPrevArea = enterFrom prevArea
        enterFromEdge edge = enterFrom Rect
            { Rect._topLeft =
                    liftA2 fromMaybe (Rect._topLeft prevArea) $
                    liftA2 (fmap . (*)) widgetSize edge
            , Rect._size =
                    liftA2 fromMaybe (Rect._size prevArea) $
                    (fmap . fmap) (const 0) edge
            }

data Keys key = Keys
    { keysDir :: DirKeys key
    , keysMoreLeft :: [key]
    , keysMoreRight :: [key]
    , keysLeftMost :: [key]
    , keysRightMost :: [key]
    , keysTop :: [key]
    , keysBottom :: [key]
    } deriving (Functor, Foldable, Traversable)

stdKeys :: Keys MetaKey
stdKeys = Keys
    { keysDir = k <$> stdDirKeys
    , keysMoreLeft = [k GLFW.Key'Home]
    , keysMoreRight = [k GLFW.Key'End]
    , keysLeftMost = [MetaKey.cmd GLFW.Key'Home]
    , keysRightMost = [MetaKey.cmd GLFW.Key'End]
    , keysTop = [k GLFW.Key'PageUp]
    , keysBottom = [k GLFW.Key'PageDown]
    }
    where
        k = MetaKey noMods

addNavEventmap ::
    Keys ModKey -> NavDests a -> Widget.EventMap a -> Widget.EventMap a
addNavEventmap Keys{..} navDests eMap =
    strongMap <> eMap <> weakMap
    where
        weakMap =
            [ movement "left"       (keysLeft  keysDir) leftOfCursor
            , movement "right"      (keysRight keysDir) rightOfCursor
            , movement "up"         (keysUp    keysDir) aboveCursor
            , movement "down"       (keysDown  keysDir) belowCursor
            , movement "more left"  keysMoreLeft        leftMostCursor
            , movement "more right" keysMoreRight       rightMostCursor
            ] ^. Lens.traverse . Lens._Just
        strongMap =
            [ movement "top"       keysTop       topCursor
            , movement "bottom"    keysBottom    bottomCursor
            , movement "leftmost"  keysLeftMost  leftMostCursor
            , movement "rightmost" keysRightMost rightMostCursor
            ] ^. Lens.traverse . Lens._Just
        movement dirName events f =
            (EventMap.keyPresses
              events
              (EventMap.Doc ["Navigation", "Move", dirName]) .
              (^. Widget.enterResultEvent)) <$>
            f navDests

enumerate2d ::
    (Foldable vert, Foldable horiz) =>
    vert (horiz a) -> [(Vector2 Int, a)]
enumerate2d xss =
    xss ^@.. Lens.folded <.> Lens.folded
    <&> _1 %~ uncurry (flip Vector2)

index2d :: (Foldable vert, Foldable horiz) => vert (horiz a) -> Vector2 Int -> a
index2d xss (Vector2 x y) = toList (toList xss !! y) !! x

getCursor :: [[Widget k]] -> Maybe Cursor
getCursor widgets =
    widgets
    & enumerate2d
    & find (Widget.isFocused . snd)
    <&> fst

make ::
    (Traversable vert, Traversable horiz, Functor f) =>
    vert (horiz (Alignment, Widget (f Widget.EventResult))) ->
    (vert (horiz Alignment), Widget (f Widget.EventResult))
make = makeWithKeys (stdKeys <&> MetaKey.toModKey)

makeWithKeys ::
    (Traversable vert, Traversable horiz, Functor f) =>
    Keys ModKey ->
    vert (horiz (Alignment, Widget (f Widget.EventResult))) ->
    (vert (horiz Alignment), Widget (f Widget.EventResult))
makeWithKeys keys children =
    ( content & each2d %~ (^. _1)
    , toList content <&> toList
      & each2d %~ (\(_align, rect, widget) -> (rect, widget))
      & toWidgetWithKeys keys mCursor size
    )
    where
        mCursor = toList children <&> toList & each2d %~ (^. _2) & getCursor
        (size, content) =
            children
            & each2d %~ toTriplet
            & GridView.makePlacements
        toTriplet (alignment, widget) =
            (alignment, widget ^. View.size, widget)

each2d :: (Traversable vert, Traversable horiz) => Lens.Traversal (vert (horiz a)) (vert (horiz b)) a b
each2d = traverse . traverse

-- TODO: We assume that the given Cursor selects a focused
-- widget. Prove it by passing the Focused data of that widget
toWidgetWithKeys ::
    Functor f =>
    Keys ModKey -> Maybe Cursor -> Widget.Size ->
    [[(Rect, Widget (f Widget.EventResult))]] ->
    Widget (f Widget.EventResult)
toWidgetWithKeys keys mCursor size sChildren =
    Widget
    { _wSize = size
    , _wState =
        case mCursor of
        Nothing ->
            Widget.StateUnfocused Widget.Unfocused
            { _uLayers = layers
            , _uMEnter = mEnter
            }
        Just cursor ->
            Widget.StateFocused Widget.Focused
            { Widget._fLayers = layers
            , Widget._fMEnter = mEnter
            , Widget._fEventMap =
                focusedChild ^. Widget.fEventMap & Lens.imapped %@~ f
            , Widget._fFocalArea = focusedChild ^. Widget.fFocalArea
            }
            where
                focusedChild =
                    index2d widgets cursor ^? Widget.wState . Widget._StateFocused
                    & fromMaybe (error "selected unfocused widget?")
                f virtCursor =
                    mkNavDests size cursor virtCursor mEnterss
                    & addNavEventmap keys
    }
    where
        mEnter = combineMEnters size mEnterss
        layers = widgets ^. Lens.folded . Lens.folded . Widget.wState . Widget.stateMakeLayers
        translateChildWidget (rect, widget) =
            Widget.translate (rect ^. Rect.topLeft) widget
        widgets = sChildren & each2d %~ translateChildWidget
        mEnterss = widgets & each2d %~ (^. Widget.mEnter)

groupSortOn :: Ord b => (a -> b) -> [a] -> [[a]]
groupSortOn f = groupOn f . sortOn f

combineMEnters ::
    Widget.Size ->
    [[Maybe (Direction -> Widget.EnterResult a)]] ->
    Maybe (Direction -> Widget.EnterResult a)
combineMEnters size children =
    chooseClosest childEnters
    where
        childEnters =
                (enumerate2d children <&> Lens.sequenceAOf _2)
                ^.. Lens.traverse . Lens._Just

        chooseClosest [] = Nothing
        chooseClosest _ = Just byDirection

        byDirection dir =
            filteredByEdge edge <&> (dir &) & minimumOn score
            where
                score enter
                    | dist > 0 = dist
                    | otherwise = enter ^. Widget.enterResultLayer & negate & fromIntegral
                    where
                        dist =
                            enter ^. Widget.enterResultRect
                            & Rect.distance dirRect
                            & modifyDistance
                            & abs
                            & Vector2.uncurry (+)
                removeUninterestingAxis :: Vector2 R -> Vector2 R
                removeUninterestingAxis = ((1 - abs (fromIntegral <$> edge)) *)
                (modifyDistance, dirRect) =
                    case dir of
                    Direction.Outside -> (id, Rect 0 0)
                    Direction.PrevFocalArea x -> (removeUninterestingAxis, x)
                    Direction.Point x -> (id, Rect x 0)
                edge =
                    case dir of
                    Direction.Point{} ->
                        -- Check all widgets for mouse movements (for hovers)
                        Vector2 0 0
                    _ -> asEdge size dirRect

        filteredByEdge =
            memo $ \(Vector2 hEdge vEdge) ->
            map snd .
            safeHead . groupSortOn ((* (-hEdge)) . (^._1._1)) .
            safeHead . groupSortOn ((* (-vEdge)) . (^._1._2)) $
            childEnters

safeHead :: Monoid a => [a] -> a
safeHead = mconcat . take 1

asEdge :: Vector2 R -> Rect -> Vector2 Int
asEdge size rect =
    Vector2 hEdge vEdge
    where
        hEdge = boolToInt rightEdge - boolToInt leftEdge
        vEdge = boolToInt bottomEdge - boolToInt topEdge
        boolToInt False = 0
        boolToInt True = 1
        Vector2 leftEdge topEdge =
            (<= 0) <$> (rect ^. Rect.bottomRight)
        Vector2 rightEdge bottomEdge =
            liftA2 (>=) (rect ^. Rect.topLeft) size
