{-# OPTIONS -Wall #-}
{-# LANGUAGE Rank2Types #-}
module Graphics.UI.Bottle.Widgets.Box(
  Box(..), make,
  Cursor, toWidget, toWidgetBiased,
  Orientation, horizontal, vertical) where

import Data.Vector.Vector2(Vector2(..))
import Graphics.UI.Bottle.Rect (Rect(..))
import Graphics.UI.Bottle.Sized (Sized(..))
import Graphics.UI.Bottle.Widget(Widget, UserIO)
import Graphics.UI.Bottle.Widgets.Grid(Grid(..))
import qualified Data.Vector.Vector2 as Vector2
import qualified Graphics.UI.Bottle.Widgets.Grid as Grid

type Cursor = Int

eHead :: [a] -> a
eHead (x:_) = x
eHead [] = error "Grid returned invalid list without any elements, instead of list Box handed it"

data Orientation = Orientation {
  oToGridCursor :: Cursor -> Grid.Cursor,
  oToGridChildren :: forall a. [a] -> [[a]],
  oFromGridCursor :: Grid.Cursor -> Cursor,
  oFromGridChildren :: forall a. [[a]] -> [a]
  }

horizontal :: Orientation
horizontal = Orientation {
  oToGridCursor = (`Vector2` 0),
  oToGridChildren = (: []),
  oFromGridCursor = Vector2.fst,
  oFromGridChildren = eHead
  }

vertical :: Orientation
vertical = Orientation {
  oToGridCursor = (0 `Vector2`),
  oToGridChildren = map (: []),
  oFromGridCursor = Vector2.snd,
  oFromGridChildren = map eHead
  }

data Box f = Box {
  boxOrientation :: Orientation,
  boxMCursor :: Maybe Cursor,
  boxContent :: Sized [(Rect, UserIO f)]
  }

make :: Orientation -> [Widget f] -> Box f
make orientation children = Box {
  boxOrientation = orientation,
  boxMCursor =
    fmap (oFromGridCursor orientation) $ Grid.gridMCursor grid,
  boxContent =
    fmap (oFromGridChildren orientation) $ Grid.gridContent grid
  }
  where
    grid = Grid.make (oToGridChildren orientation children)

toGrid :: Box f -> Grid f
toGrid (Box orientation mCursor content) = Grid {
  gridMCursor = fmap (oToGridCursor orientation) mCursor,
  gridContent = fmap (oToGridChildren orientation) content
  }

toWidget :: Box f -> Widget f
toWidget = Grid.toWidget . toGrid

toWidgetBiased :: Cursor -> Box f -> Widget f
toWidgetBiased cursor box =
  Grid.toWidgetBiased gridCursor $ toGrid box
  where
    gridCursor = oToGridCursor (boxOrientation box) cursor
