{-# LANGUAGE Rank2Types, TemplateHaskell #-}
module Graphics.UI.Bottle.Widgets.Box
  ( Box, KBox(..), Alignment
  , make, makeKeyed, makeAlign, makeCentered
  , unkey
  , boxMCursor, boxSize, boxContent, boxOrientation
  , Element, elementRect, elementW
  , Cursor, toWidget, toWidgetBiased
  , Orientation, horizontal, vertical
  , hboxAlign, vboxAlign
  , hboxCentered, vboxCentered
  , hbox, vbox
  ) where

import Control.Lens (Lens', (^.))
import Data.Vector.Vector2 (Vector2(..))
import Graphics.UI.Bottle.Rect (Rect(..))
import Graphics.UI.Bottle.Widget (Widget, Size)
import Graphics.UI.Bottle.Widgets.Grid (KGrid(..))
import qualified Control.Lens as Lens
import qualified Graphics.UI.Bottle.Widgets.Grid as Grid

type Cursor = Int

eHead :: [a] -> a
eHead (x:_) = x
eHead [] = error "Grid returned invalid list without any elements, instead of list Box handed it"

type Alignment = Grid.Alignment

data Orientation = Orientation
  { oToGridCursor :: Cursor -> Grid.Cursor
  , oToGridChildren :: forall a. [a] -> [[a]]
  , oFromGridCursor :: Grid.Cursor -> Cursor
  , oFromGridChildren :: forall a. [[a]] -> [a]
  }

horizontal :: Orientation
horizontal = Orientation
  { oToGridCursor = (`Vector2` 0)
  , oToGridChildren = (: [])
  , oFromGridCursor = (^. Lens._1)
  , oFromGridChildren = eHead
  }

vertical :: Orientation
vertical = Orientation
  { oToGridCursor = (0 `Vector2`)
  , oToGridChildren = map (: [])
  , oFromGridCursor = (^. Lens._2)
  , oFromGridChildren = map eHead
  }

type Element = Grid.Element

elementRect :: Lens' (Element f) Rect
elementRect = Grid.elementRect

elementW :: Lens' (Element f) (Widget f)
elementW = Grid.elementW

data KBox key f = KBox
  { _boxOrientation :: Orientation
  , _boxMCursor :: Maybe Cursor
  , _boxSize :: Size
  , _boxContent :: [(key, Element f)]
  }
Lens.makeLenses ''KBox

type Box = KBox ()

makeKeyed :: Orientation -> [(key, (Alignment, Widget f))] -> KBox key f
makeKeyed orientation children = KBox
  { _boxOrientation = orientation
  , _boxMCursor = fmap (oFromGridCursor orientation) $ grid ^. Grid.gridMCursor
  , _boxSize = grid ^. Grid.gridSize
  , _boxContent = oFromGridChildren orientation $ grid ^. Grid.gridContent
  }
  where
    grid = Grid.makeKeyed $ oToGridChildren orientation children

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
  { _gridMCursor = fmap (oToGridCursor orientation) mCursor
  , _gridSize = size
  , _gridContent = oToGridChildren orientation content
  }

toWidget :: KBox key f -> Widget f
toWidget = Grid.toWidget . toGrid

toWidgetBiased :: Cursor -> KBox key f -> Widget f
toWidgetBiased cursor box =
  Grid.toWidgetBiased gridCursor $ toGrid box
  where
    gridCursor = oToGridCursor (box ^. boxOrientation) cursor

boxAlign :: Orientation -> Alignment -> [Widget f] -> Widget f
boxAlign orientation align =
  toWidget .
  makeAlign align orientation

hboxAlign :: Alignment -> [Widget f] -> Widget f
hboxAlign = boxAlign horizontal

vboxAlign :: Alignment -> [Widget f] -> Widget f
vboxAlign = boxAlign vertical

vboxCentered :: [Widget f] -> Widget f
vboxCentered = vboxAlign 0.5

hboxCentered :: [Widget f] -> Widget f
hboxCentered = hboxAlign 0.5

hbox :: [(Alignment, Widget f)] -> Widget f
hbox = toWidget . make horizontal

vbox :: [(Alignment, Widget f)] -> Widget f
vbox = toWidget . make vertical
