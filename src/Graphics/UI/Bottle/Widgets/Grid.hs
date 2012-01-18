{-# OPTIONS -Wall #-}
{-# LANGUAGE TypeOperators #-}
module Graphics.UI.Bottle.Widgets.Grid(Cursor, make, makeBiased) where

import Control.Applicative (liftA2)
import Control.Arrow (first)
import Control.Monad (msum, (>=>))
import Data.List (foldl', transpose, find, maximumBy)
import Data.List.Utils (index, enumerate2d)
import Data.Maybe (isJust, fromMaybe, mapMaybe, catMaybes)
import Data.Monoid (Monoid(..))
import Data.Ord (comparing)
import Data.Vector.Vector2 (Vector2(..))
import Graphics.UI.Bottle.EventMap (EventMap)
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

fromRight :: Vector2 Int
fromRight = Vector2 1 0
fromLeft :: Vector2 Int
fromLeft = Vector2 (-1) 0
fromAbove :: Vector2 Int
fromAbove = Vector2 0 (-1)
fromBelow :: Vector2 Int
fromBelow = Vector2 0 1

mkNavEventmap :: [[Widget.MEnter k]] -> Cursor -> EventMap k
mkNavEventmap mEnterChildren cursor@(Vector2 cursorX cursorY) =
  mconcat . catMaybes $ [
    movement "left"      GLFW.KeyLeft     fromRight  leftOfCursor,
    movement "right"     GLFW.KeyRight    fromLeft   rightOfCursor,
    movement "up"        GLFW.KeyUp       fromBelow  aboveCursor,
    movement "down"      GLFW.KeyDown     fromAbove  belowCursor,
    movement "top"       GLFW.KeyPageup   fromBelow  topCursor,
    movement "bottom"    GLFW.KeyPagedown fromAbove  bottomCursor,
    movement "leftmost"  GLFW.KeyHome     fromRight  leftMostCursor,
    movement "rightmost" GLFW.KeyEnd      fromLeft   rightMostCursor
    ]
  where
    size = length2d mEnterChildren
    Vector2 cappedX cappedY = capCursor size cursor
    movement _dirName key direction =
      fmap
        (EventMap.fromEventType {-("Move " ++ dirName)-}
         (EventMap.KeyEventType EventMap.noMods key)) .
      fmap ($ Just direction) . msum
    leftOfCursor    = reverse . take cursorX $ curRow
    aboveCursor     = reverse . take cursorY $ curColumn
    rightOfCursor   = drop (cursorX+1) $ curRow
    belowCursor     = drop (cursorY+1) $ curColumn
    topCursor       = take (min 1 cursorY) $ curColumn
    leftMostCursor  = take (min 1 cursorX) $ curRow
    bottomCursor    = take 1 . reverse . drop (cursorY+1) $ curColumn
    rightMostCursor = take 1 . reverse . drop (cursorX+1) $ curRow
    curRow          = fromMaybe [] $ index cappedY mEnterChildren
    curColumn       = fromMaybe [] $ index cappedX (transpose mEnterChildren)

-- makeHelper :: Bool -> Cursor -> Cursor -> [[Widget k]] -> Widget k
-- makeHelper isFocused =

getCursor :: [[Widget k]] -> Maybe Cursor
getCursor =
  fmap cursorOf . find (wIsFocused . snd) . concat . enumerate2d
  where
    cursorOf ((row, column), _) = Vector2 column row

makeHelper ::
  ([[Widget.MEnter k]] -> Widget.MEnter k) ->
  [[Widget k]] -> Widget k
makeHelper combineEnters children =
  Widget {
    wIsFocused = isJust mCursor,
    wContent =
      fmap combineUserIOs .
      GridView.makeGeneric Widget.uioFrame .
      (map . map) wContent $ children
    }
  where
    mCursor = getCursor children
    combineUserIOs (frame, userIOss) =
      Widget.UserIO {
        Widget.uioFrame = frame,
        -- Take the enter of the chosen child here. The Grid enter
        -- logic is in GridView.makeFromWidgets
        Widget.uioMaybeEnter = combineEnters mEnterss,
        Widget.uioEventMap = maybe mempty makeEventMap mCursor
        }
      where
        mEnterss = (map . map) Widget.uioMaybeEnter userIOss
        makeEventMap cursor =
          chosenEventmap cursor `mappend`
          mkNavEventmap mEnterss cursor
        chosenEventmap = maybe mempty Widget.uioEventMap . mChosenUserIO
        mChosenUserIO (Vector2 x y) = index y userIOss >>= index x

makeEnter :: [[Widget.MEnter k]] -> Widget.MEnter k
makeEnter =
  search . mapMaybe inverse . concat . enumerate2d
  where
    search [] = Nothing
    search xs = Just $ byDirection xs
    byDirection xs dir = ($ dir) . snd . maximumOn fst $ (map . first) (fromMaybe 0 dir *) xs
    inverse ((y, x), m) = fmap ((,) $ Vector2 x y) m
    maximumOn = maximumBy . comparing

-- ^ If unfocused, will enters the given child when entered
makeBiased :: Cursor -> [[Widget k]] -> Widget k
makeBiased (Vector2 x y) = makeHelper (index y >=> index x >=> id)

make :: [[Widget k]] -> Widget k
make = makeHelper makeEnter
