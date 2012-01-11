{-# OPTIONS -Wall #-}
{-# LANGUAGE TypeOperators #-}
module Graphics.UI.Bottle.Widgets.Grid(Cursor, make) where

import Control.Applicative (liftA2)
import Control.Arrow (second)
import Control.Newtype (op)
import Control.Monad (join, msum)
import Data.List (foldl', transpose)
import Data.List.Utils (index)
import Data.Maybe (fromMaybe, catMaybes)
import Data.Monoid (mappend, mconcat)
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

mkNavKeymap :: [[Maybe k]] -> Cursor -> EventMap k
mkNavKeymap mEnterChildren cursor@(Vector2 cursorX cursorY) =
  mconcat . catMaybes $ [
    movement "left"      GLFW.KeyLeft     leftOfCursor,
    movement "right"     GLFW.KeyRight    rightOfCursor,
    movement "up"        GLFW.KeyUp       aboveCursor,
    movement "down"      GLFW.KeyDown     belowCursor,
    movement "top"       GLFW.KeyPageup   topCursor,
    movement "bottom"    GLFW.KeyPagedown bottomCursor,
    movement "leftmost"  GLFW.KeyHome     leftMostCursor,
    movement "rightmost" GLFW.KeyEnd      rightMostCursor
    ]
  where
    size = length2d mEnterChildren
    Vector2 cappedX cappedY = capCursor size cursor
    movement _dirName key =
      fmap
        (EventMap.fromEventType {-("Move " ++ dirName)-}
         (EventMap.KeyEventType EventMap.noMods key)) .
      msum
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
    fmap (uncurry Widget.UserIO .
          second (buildMEventHandlers . (map . map) Widget.uioMEventHandlers)) .
    GridView.makeGeneric Widget.uioFrame .
    (map . map) (op Widget) $ children
  where
    buildMEventHandlers mEventHandlerss =
      (fmap . Widget.atEhEventMap) (`mappend` navKeymap) mActiveChild
      where
        mEnterChildren = (map . map . fmap) Widget.ehEnter mEventHandlerss
        navKeymap = mkNavKeymap mEnterChildren cursor
        mActiveChild = join $ index mEventHandlerss y >>= (`index` x)

make :: Maybe Cursor -> [[Widget k]] -> Widget k
make (Just fd) = makeFocused fd
make Nothing = GridView.makeFromWidgets
