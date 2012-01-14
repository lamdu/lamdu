{-# OPTIONS -Wall #-}
{-# LANGUAGE TypeOperators #-}
module Graphics.UI.Bottle.Widgets.Grid(Cursor, make) where

import Control.Applicative (liftA2)
import Control.Newtype (op)
import Control.Monad (join, msum)
import Data.List (foldl', transpose)
import Data.List.Utils (index)
import Data.Maybe (fromMaybe, catMaybes)
import Data.Monoid (Monoid(..))
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

fromRight :: Widget.Direction
fromRight = Vector2 1 0
fromLeft :: Widget.Direction
fromLeft = Vector2 (-1) 0
fromAbove :: Widget.Direction
fromAbove = Vector2 0 (-1)
fromBelow :: Widget.Direction
fromBelow = Vector2 0 1

mkNavEventmap :: [[Maybe (Widget.Direction -> k)]] -> Cursor -> EventMap k
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
      fmap ($ direction) . msum
    leftOfCursor    = reverse . take cursorX $ curRow
    aboveCursor     = reverse . take cursorY $ curColumn
    rightOfCursor   = drop (cursorX+1) $ curRow
    belowCursor     = drop (cursorY+1) $ curColumn
    topCursor       = take (min 1 cursorY) $ curColumn
    leftMostCursor  = take (min 1 cursorX) $ curRow
    bottomCursor    = take 1 . reverse . drop (cursorY+1) $ curColumn
    rightMostCursor = take 1 . reverse . drop (cursorX+1) $ curRow
    curRow          = fromMaybe [] $ index mEnterChildren cappedY
    curColumn       = fromMaybe [] $ index (transpose mEnterChildren) cappedX

makeFocused :: Cursor -> [[Widget k]] -> Widget k
makeFocused cursor@(Vector2 x y) children =
  Widget $
    fmap combineUserIOs .
    GridView.makeGeneric Widget.uioFrame .
    (map . map) (op Widget) $ children
  where
    combineUserIOs (frame, userIOss) =
      Widget.UserIO {
        Widget.uioFrame = frame,
        -- Take the enter of the chosen child here. The Grid enter
        -- logic is in GridView.makeFromWidgets
        Widget.uioMaybeEnter = join $ fmap Widget.uioMaybeEnter mChosenUserIO,
        Widget.uioEventMap = chosenEventmap `mappend` navEventmap
        }
      where
        mEnterss = (map . map) Widget.uioMaybeEnter userIOss
        navEventmap = mkNavEventmap mEnterss cursor
        chosenEventmap = fromMaybe mempty $ fmap Widget.uioEventMap mChosenUserIO
        mChosenUserIO = index userIOss y >>= (`index` x)

make :: Maybe Cursor -> [[Widget k]] -> Widget k
make (Just cursor) = makeFocused cursor
make Nothing = GridView.makeFromWidgets
