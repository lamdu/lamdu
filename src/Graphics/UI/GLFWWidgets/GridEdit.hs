{-# OPTIONS -Wall #-}
{-# LANGUAGE TemplateHaskell #-}
module Graphics.UI.GLFWWidgets.GridEdit(Cursor, make) where

import Control.Applicative (liftA2)
import Control.Arrow (second)
import Control.Newtype (unpack)
import Data.List (foldl', find, transpose)
import Data.List.Utils (enumerate, enumerate2d)
import Data.Maybe (isJust)
import Data.Monoid (mconcat)
import Data.Vector.Vector2 (Vector2(..))
import Graphics.UI.GLFWWidgets.EventMap (EventMap(..))
import Graphics.UI.GLFWWidgets.Widget (Widget(..))
import qualified Graphics.UI.GLFWWidgets.EventMap as EventMap
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.UI.GLFWWidgets.GridView as GridView

type Cursor = Vector2 Int

length2d :: [[a]] -> Vector2 Int
length2d xs = Vector2 (foldl' max 0 . map length $ xs) (length xs)

capCursor :: Vector2 Int -> Vector2 Int -> Vector2 Int
capCursor size = fmap (max 0) . liftA2 min (fmap (subtract 1) size)

mkNavMKeymap :: [[Bool]] -> Cursor -> Maybe (EventMap Cursor)
mkNavMKeymap wantFocusRows cursor@(Vector2 cursorX cursorY) =
  mconcat $ [
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
    curRow        = enumerate $ wantFocusRows !! cappedY
    curColumn     = enumerate $ transpose wantFocusRows !! cappedX

make :: (Cursor -> k) -> Cursor -> [[Widget k]] -> Widget k
make liftCursor cursor@(Vector2 x y) children = Widget handleHasFocus
  where
    handleHasFocus hasFocus =
      (fmap . second) (f . (map . map) snd) .
      GridView.makeGeneric fst .
      (map . map) (applyHasFocus hasFocus . second unpack) .
      enumerate2d $ children
    applyHasFocus False (_, inWidget) = inWidget False
    applyHasFocus True ((r, c), inWidget) = inWidget $ Vector2 c r == cursor
    f xss =
      mconcat [
        xss !! y !! x,
        (fmap . fmap) liftCursor $ mkNavMKeymap wantFocusRows cursor
      ]
      where
        wantFocusRows = (map . map) isJust xss
