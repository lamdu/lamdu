{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Graphics.UI.Bottle.Widgets.Grid
  ( Grid, KGrid(..)
  , make, makeKeyed, makeAlign, makeCentered
  , unkey
  , Alignment
  , gridMCursor, gridSize, gridContent
  , Element(..)
  , elementAlign, elementRect, elementW
  , Cursor, toWidget, toWidgetBiased
  ) where

import Control.Applicative (liftA2, (<$>))
import Control.Lens ((^.), (%~))
import Control.Monad (join, msum)
import Data.Function (on)
import Data.List (foldl', transpose, find, minimumBy, sortBy, groupBy)
import Data.List.Utils (index, enumerate2d)
import Data.MRUMemo (memo)
import Data.Maybe (isJust, fromMaybe, catMaybes, mapMaybe)
import Data.Monoid (Monoid(..))
import Data.Ord (comparing)
import Data.Vector.Vector2 (Vector2(..))
import Graphics.UI.Bottle.Rect (Rect(..))
import Graphics.UI.Bottle.Widget (Widget(..), R)
import Graphics.UI.Bottle.Widgets.GridView (Alignment)
import Graphics.UI.Bottle.Widgets.StdKeys (DirKeys(..), stdDirKeys)
import qualified Control.Lens as Lens
import qualified Data.Vector.Vector2 as Vector2
import qualified Graphics.UI.Bottle.Direction as Direction
import qualified Graphics.UI.Bottle.EventMap as EventMap
import qualified Graphics.UI.Bottle.Rect as Rect
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.GridView as GridView
import qualified Graphics.UI.GLFW as GLFW

type Cursor = Vector2 Int

length2d :: [[a]] -> Vector2 Int
length2d xs = Vector2 (foldl' max 0 . map length $ xs) (length xs)

capCursor :: Vector2 Int -> Vector2 Int -> Vector2 Int
capCursor size = fmap (max 0) . liftA2 min (subtract 1 <$> size)

data NavDests f = NavDests
  { leftOfCursor
  , aboveCursor
  , rightOfCursor
  , belowCursor
  , topCursor
  , leftMostCursor
  , bottomCursor
  , rightMostCursor :: Maybe (Widget.EnterResult f)
  }

mkNavDests :: Widget.Size -> Rect -> [[Widget.MEnter f]] -> Cursor -> NavDests f
mkNavDests widgetSize prevFocalArea mEnterss cursor@(Vector2 cursorX cursorY) = NavDests
  { leftOfCursor    = givePrevFocalArea . reverse $ take cursorX curRow
  , aboveCursor     = givePrevFocalArea . reverse $ take cursorY curColumn
  , rightOfCursor   = givePrevFocalArea $ drop (cursorX+1) curRow
  , belowCursor     = givePrevFocalArea $ drop (cursorY+1) curColumn

  , topCursor       = giveEdge (Vector2 Nothing (Just 0)) $ take (min 1 cursorY) curColumn
  , leftMostCursor  = giveEdge (Vector2 (Just 0) Nothing) $ take (min 1 cursorX) curRow
  , bottomCursor    = giveEdge (Vector2 Nothing (Just 1)) . take 1 . reverse $ drop (cursorY+1) curColumn
  , rightMostCursor = giveEdge (Vector2 (Just 1) Nothing) . take 1 . reverse $ drop (cursorX+1) curRow
  }
  where
    curRow = fromMaybe [] $ index cappedY mEnterss
    curColumn = fromMaybe [] $ index cappedX (transpose mEnterss)
    Vector2 cappedX cappedY = capCursor size cursor
    size = length2d mEnterss

    give rect = fmap ($ Direction.PrevFocalArea rect) . msum
    givePrevFocalArea = give prevFocalArea
    giveEdge edge = give Rect
      { Rect._topLeft =
          liftA2 fromMaybe (Rect._topLeft prevFocalArea) $
          liftA2 (fmap . (*)) widgetSize edge
      , Rect._size =
          liftA2 fromMaybe (Rect._size prevFocalArea) $
          (fmap . fmap) (const 0) edge
      }


mkNavEventmap
  :: NavDests f -> (Widget.EventHandlers f, Widget.EventHandlers f)
mkNavEventmap navDests = (weakMap, strongMap)
  where
    weakMap = mconcat . catMaybes $ [
      movementk "left"       (keysLeft  stdDirKeys) leftOfCursor,
      movementk "right"      (keysRight stdDirKeys) rightOfCursor,
      movementk "up"         (keysUp    stdDirKeys) aboveCursor,
      movementk "down"       (keysDown  stdDirKeys) belowCursor,
      movementk "more left"  [GLFW.KeyHome]         leftMostCursor,
      movementk "more right" [GLFW.KeyEnd]          rightMostCursor
      ]
    strongMap = mconcat . catMaybes $ [
      movementk "top"       [GLFW.KeyPageup]    topCursor,
      movementk "bottom"    [GLFW.KeyPagedown]  bottomCursor,
      movement "leftmost"  [ctrlK GLFW.KeyHome] leftMostCursor,
      movement "rightmost" [ctrlK GLFW.KeyEnd]  rightMostCursor
      ]
    k = EventMap.ModKey EventMap.noMods
    ctrlK = EventMap.ModKey EventMap.ctrl
    movementk dirName = movement dirName . map k
    movement dirName events f =
      (EventMap.keyPresses
       events
       (EventMap.Doc ["Navigation", "Move", dirName]) .
       (^. Widget.enterResultEvent)) <$>
      f navDests

getCursor :: [[Widget k]] -> Maybe Cursor
getCursor =
  fmap cursorOf . find (_wIsFocused . snd) . concat . enumerate2d
  where
    cursorOf ((row, column), _) = Vector2 column row

data Element f = Element
  { _elementAlign :: Alignment
  , _elementRect :: Rect
  , _elementW :: Widget f
  }

data KGrid key f = KGrid
  { _gridMCursor :: Maybe Cursor
  , _gridSize :: Widget.Size
  , _gridContent :: [[(key, Element f)]]
  }

Lens.makeLenses ''Element
Lens.makeLenses ''KGrid

type Grid = KGrid ()

makeKeyed :: [[(key, (Alignment, Widget f))]] -> KGrid key f
makeKeyed children = KGrid
  { _gridMCursor = getCursor $ (map . map) (snd . snd) children
  , _gridSize = size
  , _gridContent = content
  }
  where
    (size, content) =
      GridView.makeGeneric translate $
      (map . map) mkSizedKeyedContent children
    mkSizedKeyedContent (key, (alignment, widget)) =
      (alignment, (widget ^. Widget.wSize, (key, widget)))
    translate align rect =
      Lens._2 %~
      Element align rect . Widget.translate (rect ^. Rect.topLeft)

unkey :: [[(Alignment, Widget f)]] -> [[((), (Alignment, Widget f))]]
unkey = (map . map) ((,) ())

make :: [[(Alignment, Widget f)]] -> Grid f
make = makeKeyed . unkey

makeAlign :: Alignment -> [[Widget f]] -> Grid f
makeAlign alignment = make . (map . map) ((,) alignment)

makeCentered :: [[Widget f]] -> Grid f
makeCentered = makeAlign 0.5

helper ::
  (Widget.Size -> [[Widget.MEnter f]] -> Widget.MEnter f) ->
  KGrid key f -> Widget f
helper combineEnters (KGrid mCursor size sChildren) =
  combineWs $ (map . map) (^. Lens._2 . elementW) sChildren
  where
    combineWs wss =
      maybe unselectedW makeW mCursor
      where
        framess = (map . map) _wFrame wss
        mEnterss = (map . map) _wMaybeEnter wss
        frame = mconcat $ concat framess
        mEnter = combineEnters size mEnterss

        unselectedW = Widget
          { _wIsFocused = isJust mCursor
          , _wSize = size
          , _wFrame = frame
          , _wMaybeEnter = mEnter
          , _wEventMap = mempty
          , _wFocalArea = Rect 0 size
          }

        makeW cursor@(Vector2 x y) = Widget
          { _wIsFocused = isJust mCursor
          , _wSize = size
          , _wFrame = frame
          , _wMaybeEnter = mEnter
          , _wEventMap = makeEventMap w navDests
          , _wFocalArea = _wFocalArea w
          }
          where
            navDests = mkNavDests size (_wFocalArea w) mEnterss cursor
            w = wss !! y !! x

        makeEventMap w navDests =
          mconcat [strongMap, _wEventMap w, weakMap]
          where
            (weakMap, strongMap) = mkNavEventmap navDests

tupleToVector2 :: (a, a) -> Vector2 a
tupleToVector2 (y, x) = Vector2 x y

minimumOn :: Ord b => (a -> b) -> [a] -> a
minimumOn = minimumBy . comparing

sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn = sortBy . comparing

groupOn :: Eq b => (a -> b) -> [a] -> [[a]]
groupOn = groupBy . ((==) `on`)

groupSortOn :: Ord b => (a -> b) -> [a] -> [[a]]
groupSortOn f = groupOn f . sortOn f

-- ^ If unfocused, will enters the given child when entered
toWidgetBiased :: Cursor -> KGrid key f -> Widget f
toWidgetBiased (Vector2 x y) =
  helper $ \size children ->
  maybeOverride children <$> combineMEnters size children
  where
    maybeOverride children enter dir =
      case dir of
      Direction.Outside -> biased
      Direction.PrevFocalArea _ -> biased
      Direction.Point _ -> unbiased
      where
        unbiased = enter dir
        biased = maybe unbiased ($ dir) . join $ index y children >>= index x


toWidget :: KGrid key f -> Widget f
toWidget = helper combineMEnters

combineMEnters :: Widget.Size -> [[Widget.MEnter f]] -> Widget.MEnter f
combineMEnters size children = chooseClosest childEnters
  where
    childEnters =
      mapMaybe indexIntoMaybe .
      concat . (Lens.mapped . Lens.mapped . Lens._1 %~ tupleToVector2) $
      enumerate2d children

    chooseClosest [] = Nothing
    chooseClosest _ = Just byDirection

    byDirection dir =
      minimumOn
      (Vector2.uncurry (+) . abs . modifyDistance .
       distance dirRect . (^. Widget.enterResultRect)) .
      map ($ dir) $ filteredByEdge edge
      where
        removeUninterestingAxis = ((1 - abs (fromIntegral <$> edge)) *)
        (modifyDistance, dirRect) = case dir of
          Direction.Outside -> (id, Rect 0 0)
          Direction.PrevFocalArea x -> (removeUninterestingAxis, x)
          Direction.Point x -> (id, Rect x 0)
        edge = asEdge size dirRect

    distance = (-) `on` (^. Rect.center)

    filteredByEdge = memo $ \(Vector2 hEdge vEdge) ->
      map snd .
      safeHead . groupSortOn ((* (-hEdge)) . (^. Lens._1 . Lens._1)) .
      safeHead . groupSortOn ((* (-vEdge)) . (^. Lens._1 . Lens._2)) $
      childEnters
    indexIntoMaybe (i, m) = (,) i <$> m

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
