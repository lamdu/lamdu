{-# LANGUAGE Rank2Types, TemplateHaskell #-}
module Graphics.UI.Bottle.Widgets.Box(
  Box, KBox(..), make, makeKeyed, unkey, getElement,
  atBoxMCursor, atBoxContent, atBoxOrientation,
  BoxElement, mkBoxElement, boxElementRect, boxElementUio,
  atBoxElementRect, atBoxElementUio,
  Cursor, toWidget, toWidgetBiased,
  Orientation, horizontal, vertical) where

import Data.Maybe (fromMaybe)
import Data.Vector.Vector2 (Vector2(..))
import Graphics.UI.Bottle.Rect (Rect(..))
import Graphics.UI.Bottle.Sized (Sized(..))
import Graphics.UI.Bottle.Widget (Widget, UserIO)
import Graphics.UI.Bottle.Widgets.Grid (KGrid(..))
import qualified Data.AtFieldTH as AtFieldTH
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

type BoxElement f = Grid.GridElement f

mkBoxElement :: Rect -> UserIO f -> BoxElement f
mkBoxElement = Grid.GridElement

boxElementRect :: Grid.GridElement f -> Rect
boxElementRect = Grid.gridElementRect

boxElementUio :: Grid.GridElement f -> UserIO f
boxElementUio = Grid.gridElementUio

atBoxElementRect :: (Rect -> Rect) -> Grid.GridElement f0 -> Grid.GridElement f0
atBoxElementRect = Grid.atGridElementRect

atBoxElementUio
  :: (UserIO a -> UserIO b)
  -> Grid.GridElement a -> Grid.GridElement b
atBoxElementUio = Grid.atGridElementUio

data KBox key f = KBox {
  boxOrientation :: Orientation,
  boxMCursor :: Maybe Cursor,
  boxContent :: Sized [(key, BoxElement f)]
  }

AtFieldTH.make ''KBox

type Box = KBox ()

makeKeyed :: Orientation -> [(key, Widget f)] -> KBox key f
makeKeyed orientation children = KBox {
  boxOrientation = orientation,
  boxMCursor =
    fmap (oFromGridCursor orientation) $ Grid.gridMCursor grid,
  boxContent =
    fmap (oFromGridChildren orientation) $ Grid.gridContent grid
  }
  where
    grid = Grid.makeKeyed (oToGridChildren orientation children)

unkey :: [Widget f] -> [((), Widget f)]
unkey = map ((,) ())

getElement :: (Show key, Eq key) => key -> [(key, BoxElement f)] -> BoxElement f
getElement key =
  fromMaybe (error errorMsg) . lookup key
  where
    errorMsg = "getElement: " ++ show key ++ " not found in Box!"

make :: Orientation -> [Widget f] -> Box f
make orientation = makeKeyed orientation . unkey

toGrid :: KBox key f -> KGrid key f
toGrid (KBox orientation mCursor content) = KGrid {
  gridMCursor = fmap (oToGridCursor orientation) mCursor,
  gridContent = fmap (oToGridChildren orientation) content
  }

toWidget :: KBox key f -> Widget f
toWidget = Grid.toWidget . toGrid

toWidgetBiased :: Cursor -> KBox key f -> Widget f
toWidgetBiased cursor box =
  Grid.toWidgetBiased gridCursor $ toGrid box
  where
    gridCursor = oToGridCursor (boxOrientation box) cursor
