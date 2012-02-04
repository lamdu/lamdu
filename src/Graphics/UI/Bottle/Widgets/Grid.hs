{-# OPTIONS -Wall #-}
module Graphics.UI.Bottle.Widgets.Grid(Cursor, make, makeBiased) where

import Control.Applicative (liftA2)
import Control.Arrow (first)
import Control.Monad (msum, (>=>))
import Data.List (foldl', transpose, find, maximumBy)
import Data.List.Utils (index, enumerate2d)
import Data.Maybe (isJust, fromMaybe, mapMaybe, catMaybes)
import Data.Monoid (mempty, mconcat)
import Data.Ord (comparing)
import Data.Vector.Vector2 (Vector2(..))
import Graphics.UI.Bottle.Widget (Widget(..))
import qualified Graphics.UI.Bottle.EventMap as EventMap
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.GridView as GridView
import qualified Graphics.UI.GLFW as GLFW

type Cursor = Vector2 Int

length2d :: [[a]] -> Vector2 Int
length2d xs = Vector2 (foldl' max 0 . map length $ xs) (length xs)

capCursor :: Vector2 Int -> Vector2 Int -> Vector2 Int
capCursor size = fmap (max 0) . liftA2 min (fmap (subtract 1) size)

fromRight :: Widget.Direction
fromRight = Widget.Dir (Vector2 1 0)
fromLeft :: Widget.Direction
fromLeft = Widget.Dir (Vector2 (-1) 0)
fromAbove :: Widget.Direction
fromAbove = Widget.Dir (Vector2 0 (-1))
fromBelow :: Widget.Direction
fromBelow = Widget.Dir (Vector2 0 1)

mkNavEventmap :: [[Widget.MEnter f]] -> Cursor -> (Widget.EventHandlers f, Widget.EventHandlers f)
mkNavEventmap mEnterChildren cursor@(Vector2 cursorX cursorY) = (weakMap, strongMap)
  where
    weakMap = mconcat . catMaybes $ [
      movement "left"       (k GLFW.KeyLeft)  fromRight  leftOfCursor,
      movement "right"      (k GLFW.KeyRight) fromLeft   rightOfCursor,
      movement "up"         (k GLFW.KeyUp)    fromBelow  aboveCursor,
      movement "down"       (k GLFW.KeyDown)  fromAbove  belowCursor,
      movement "more left"  (k GLFW.KeyHome)  fromLeft   leftMostCursor,
      movement "more right" (k GLFW.KeyEnd)   fromRight  rightMostCursor
      ]
    strongMap = mconcat . catMaybes $ [
      movement "top"       (k GLFW.KeyPageup)   fromAbove  topCursor,
      movement "bottom"    (k GLFW.KeyPagedown) fromBelow  bottomCursor,
      movement "leftmost"  (ctrlK GLFW.KeyHome) fromLeft   leftMostCursor,
      movement "rightmost" (ctrlK GLFW.KeyEnd)  fromRight  rightMostCursor
      ]
    k = EventMap.KeyEventType EventMap.noMods
    ctrlK = EventMap.KeyEventType EventMap.ctrl
    size = length2d mEnterChildren
    Vector2 cappedX cappedY = capCursor size cursor
    movement dirName event direction =
      fmap
        (EventMap.fromEventType
         event
         ("Move " ++ dirName) .
         ($ direction)) .
      msum
    leftOfCursor    = reverse $ take cursorX curRow
    aboveCursor     = reverse $ take cursorY curColumn
    rightOfCursor   = drop (cursorX+1) curRow
    belowCursor     = drop (cursorY+1) curColumn
    topCursor       = take (min 1 cursorY) curColumn
    leftMostCursor  = take (min 1 cursorX) curRow
    bottomCursor    = take 1 . reverse $ drop (cursorY+1) curColumn
    rightMostCursor = take 1 . reverse $ drop (cursorX+1) curRow
    curRow          = fromMaybe [] $ index cappedY mEnterChildren
    curColumn       = fromMaybe [] $ index cappedX (transpose mEnterChildren)

-- makeHelper :: Bool -> Cursor -> Cursor -> [[Widget k]] -> Widget k
-- makeHelper isFocused =

getCursor :: [[Widget k]] -> Maybe Cursor
getCursor =
  fmap cursorOf . find (isFocused . snd) . concat . enumerate2d
  where
    cursorOf ((row, column), _) = Vector2 column row

makeHelper ::
  ([[Widget.MEnter k]] -> Widget.MEnter k) ->
  [[Widget k]] -> Widget k
makeHelper combineEnters children =
  Widget {
    isFocused = isJust mCursor,
    content =
      fmap combineUserIOs .
      GridView.makeGeneric Widget.translateUserIO .
      (map . map) content $ children
    }
  where
    mCursor = getCursor children
    combineUserIOs userIOss =
      Widget.UserIO {
        Widget.uioFrame = mconcat . map Widget.uioFrame $ concat userIOss,
        -- Take the enter of the chosen child here. The Grid enter
        -- logic is in GridView.makeFromWidgets
        Widget.uioMaybeEnter = combineEnters mEnterss,
        Widget.uioEventMap = maybe mempty makeEventMap mCursor
        }
      where
        mEnterss = (map . map) Widget.uioMaybeEnter userIOss
        makeEventMap cursor =
          mconcat [strongMap, chosenEventmap cursor, weakMap]
          where
            (weakMap, strongMap) = mkNavEventmap mEnterss cursor
        chosenEventmap = maybe mempty Widget.uioEventMap . mChosenUserIO
        mChosenUserIO (Vector2 x y) = index y userIOss >>= index x

makeEnter :: [[Widget.MEnter k]] -> Widget.MEnter k
makeEnter =
  search . mapMaybe inverse . concat . enumerate2d
  where
    search [] = Nothing
    search xs = Just $ byDirection xs
    byDirection xs dir =
        ($ dir) . snd . maximumOn fst $ (map . first) (Widget.direction 0 id dir *) xs
    inverse ((y, x), m) = fmap ((,) $ Vector2 x y) m
    maximumOn = maximumBy . comparing

-- ^ If unfocused, will enters the given child when entered
makeBiased :: Cursor -> [[Widget k]] -> Widget k
makeBiased (Vector2 x y) = makeHelper (index y >=> index x >=> id)

make :: [[Widget k]] -> Widget k
make = makeHelper makeEnter
