{-# LANGUAGE Rank2Types, TemplateHaskell #-}
module Graphics.UI.Bottle.Widgets.Box
  ( Box, KBox(..), Alignment
  , make, makeKeyed, makeAlign, makeCentered
  , unkey
  , atBoxMCursor, atBoxContent, atBoxOrientation
  , BoxElement, boxElementRect, boxElementW
  , atBoxElementRect, atBoxElementW
  , Cursor, toWidget, toWidgetBiased
  , Orientation, horizontal, vertical
  ) where

import Control.Arrow (first, second)
import Data.Vector.Vector2 (Vector2(..))
import Graphics.UI.Bottle.Rect (Rect(..))
import Graphics.UI.Bottle.Widget (Widget, Size, R)
import Graphics.UI.Bottle.Widgets.Grid (KGrid(..))
import qualified Data.AtFieldTH as AtFieldTH
import qualified Data.Vector.Vector2 as Vector2
import qualified Graphics.UI.Bottle.Widgets.Grid as Grid

type Cursor = Int

eHead :: [a] -> a
eHead (x:_) = x
eHead [] = error "Grid returned invalid list without any elements, instead of list Box handed it"

type Alignment = R -- 0..1

data Orientation = Orientation {
  oToGridCursor :: Cursor -> Grid.Cursor,
  oToGridChildren :: forall a. [a] -> [[a]],
  oFromGridCursor :: Grid.Cursor -> Cursor,
  oFromGridChildren :: forall a. [[a]] -> [a],
  oToGridAlignment :: Alignment -> Grid.Alignment
  }

horizontal :: Orientation
horizontal = Orientation {
  oToGridCursor = (`Vector2` 0),
  oToGridChildren = (: []),
  oFromGridCursor = Vector2.fst,
  oFromGridChildren = eHead,
  oToGridAlignment = Vector2 0
  }

vertical :: Orientation
vertical = Orientation {
  oToGridCursor = (0 `Vector2`),
  oToGridChildren = map (: []),
  oFromGridCursor = Vector2.snd,
  oFromGridChildren = map eHead,
  oToGridAlignment = flip Vector2 0
  }

type BoxElement f = Grid.GridElement f

boxElementRect :: Grid.GridElement f -> Rect
boxElementRect = Grid.gridElementRect

boxElementW :: Grid.GridElement f -> Widget f
boxElementW = Grid.gridElementW

atBoxElementRect :: (Rect -> Rect) -> Grid.GridElement f0 -> Grid.GridElement f0
atBoxElementRect = Grid.atGridElementRect

atBoxElementW
  :: (Widget a -> Widget b)
  -> Grid.GridElement a -> Grid.GridElement b
atBoxElementW = Grid.atGridElementW

data KBox key f = KBox
  { boxOrientation :: Orientation
  , boxMCursor :: Maybe Cursor
  , boxSize :: Size
  , boxContent :: [(key, BoxElement f)]
  }

AtFieldTH.make ''KBox

type Box = KBox ()

makeKeyed :: Orientation -> [(key, (Alignment, Widget f))] -> KBox key f
makeKeyed orientation children = KBox
  { boxOrientation = orientation
  , boxMCursor = fmap (oFromGridCursor orientation) $ Grid.gridMCursor grid
  , boxSize = Grid.gridSize grid
  , boxContent = oFromGridChildren orientation $ Grid.gridContent grid
  }
  where
    grid =
      Grid.makeKeyed .
      oToGridChildren orientation .
      (map . second . first) (oToGridAlignment orientation) $
      children

unkey :: [(Alignment, Widget f)] -> [((), (Alignment, Widget f))]
unkey = map ((,) ())

make :: Orientation -> [(Alignment, Widget f)] -> Box f
make orientation = makeKeyed orientation . unkey

makeAlign :: Alignment -> Orientation -> [Widget f] -> Box f
makeAlign alignment orientation = make orientation . map ((,) alignment)

makeCentered :: Orientation -> [Widget f] -> Box f
makeCentered = makeAlign 0.5

toGrid :: KBox key f -> KGrid key f
toGrid (KBox orientation mCursor size content) = KGrid
  { gridMCursor = fmap (oToGridCursor orientation) mCursor
  , gridSize = size
  , gridContent = oToGridChildren orientation content
  }

toWidget :: KBox key f -> Widget f
toWidget = Grid.toWidget . toGrid

toWidgetBiased :: Cursor -> KBox key f -> Widget f
toWidgetBiased cursor box =
  Grid.toWidgetBiased gridCursor $ toGrid box
  where
    gridCursor = oToGridCursor (boxOrientation box) cursor
