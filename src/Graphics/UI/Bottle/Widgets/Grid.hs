{-# OPTIONS -Wall #-}
{-# LANGUAGE TypeOperators #-}
module Graphics.UI.Bottle.Widgets.Grid(Cursor, make, makeWithLabel) where

import Control.Applicative (liftA2)
import Control.Arrow (first, second)
import Control.Newtype (unpack)
import Control.Monad (join)
import Data.List (foldl', find, transpose)
import Data.List.Utils (enumerate, enumerate2d, index)
import Data.Maybe (fromMaybe, isJust)
import Data.Monoid (mconcat)
import Data.Record.Label ((:->), getL, setL)
import Data.Vector.Vector2 (Vector2(..))
import Graphics.UI.Bottle.EventMap (EventMap)
import Graphics.UI.Bottle.Widget (Widget(..))
import qualified Graphics.UI.Bottle.EventMap as EventMap
import qualified Graphics.UI.Bottle.Widgets.GridView as GridView
import qualified Graphics.UI.GLFW as GLFW

type Cursor = Vector2 Int

length2d :: [[a]] -> Vector2 Int
length2d xs = Vector2 (foldl' max 0 . map length $ xs) (length xs)

capCursor :: Vector2 Int -> Vector2 Int -> Vector2 Int
capCursor size = fmap (max 0) . liftA2 min (fmap (subtract 1) size)

mkNavMKeymap :: [[Bool]] -> Cursor -> Maybe (EventMap Cursor)
mkNavMKeymap wantFocusRows cursor@(Vector2 cursorX cursorY) =
  mconcat [
    movement "left"      GLFW.KeyLeft   leftOfCursor,
    movement "right"     GLFW.KeyRight  rightOfCursor,
    movement "up"        GLFW.KeyUp     aboveCursor,
    movement "down"      GLFW.KeyDown   belowCursor,
    movement "top"       GLFW.KeyPageup   topCursor,
    movement "bottom"    GLFW.KeyPagedown bottomCursor,
    movement "leftmost"  GLFW.KeyHome   leftMostCursor,
    movement "rightmost" GLFW.KeyEnd    rightMostCursor
    ]
  where
    size = length2d wantFocusRows
    Vector2 cappedX cappedY = capCursor size cursor
    movement _dirName key =
      fmap $
      EventMap.singleton {-("Move " ++ dirName)-}
      (EventMap.KeyEventType EventMap.noMods key) . const
    x = fmap (cappedX `Vector2`) . findMove
    y = fmap (`Vector2` cappedY) . findMove
    leftOfCursor    = y . reverse . take cursorX $ curRow
    aboveCursor     = x . reverse . take cursorY $ curColumn
    rightOfCursor   = y . drop (cursorX+1) $ curRow
    belowCursor     = x . drop (cursorY+1) $ curColumn
    topCursor       = x . take (min 1 cursorY) $ curColumn
    leftMostCursor  = y . take (min 1 cursorX) $ curRow
    bottomCursor    = x . take 1 . reverse . drop (cursorY+1) $ curColumn
    rightMostCursor = y . take 1 . reverse . drop (cursorX+1) $ curRow
    findMove      = fmap fst . find snd
    curRow        = enumerate . fromMaybe [] $ index wantFocusRows cappedY
    curColumn     = enumerate . fromMaybe [] $ index (transpose wantFocusRows) cappedX

make ::
  (Cursor -> k) -> Cursor ->
  [[Bool -> Widget k]] -> Bool -> Widget k
make liftCursor cursor@(Vector2 x y) children hasFocus =
  Widget $
    (fmap . second) (buildKeymap . (map . map) snd) .
    GridView.makeGeneric fst .
    (map . map) (applyHasFocus . first compareCursor) .
    enumerate2d $ children
  where
    compareCursor (r, c) = Vector2 c r == cursor
    applyHasFocus (isSelected, child) = unpack . child $ hasFocus && isSelected
    buildKeymap xss =
      mconcat [
        join $ index xss y >>= (`index` x),
        (fmap . fmap) liftCursor $ mkNavMKeymap wantFocusRows cursor
      ]
      where
        wantFocusRows = (map . map) isJust xss

makeWithLabel ::
  (model :-> Cursor) -> model ->
  [[Bool -> Widget model]] -> Bool -> Widget model
makeWithLabel label model = make (flip (setL label) model) (getL label model)
