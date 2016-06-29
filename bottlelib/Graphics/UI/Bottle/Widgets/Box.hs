{-# LANGUAGE Rank2Types, TemplateHaskell #-}
module Graphics.UI.Bottle.Widgets.Box
    ( Box, KBox(..), Alignment(..), Grid.alignmentRatio
    , make, makeKeyed, makeAlign
    , unkey
    , boxMCursor, boxSize, boxContent, boxOrientation
    , Element, elementRect, elementAlign, elementOriginalWidget
    , Cursor
    , Orientation, horizontal, vertical
    , hboxAlign, vboxAlign
    , hboxCentered, vboxCentered
    , hbox, vbox
    ) where

import           Control.Lens ((^.))
import qualified Control.Lens as Lens
import           Control.Lens.Tuple
import           Data.Vector.Vector2 (Vector2(..))
import           Graphics.UI.Bottle.Rect (Rect(..))
import           Graphics.UI.Bottle.Widget (Widget, Size)
import           Graphics.UI.Bottle.Widgets.Grid (Alignment(..))
import qualified Graphics.UI.Bottle.Widgets.Grid as Grid

type Cursor = Int

eHead :: [a] -> a
eHead (x:_) = x
eHead [] = error "Grid returned invalid list without any elements, instead of list Box handed it"

-- Want a 2d vector like in Grid, because we may want to preserve the
-- alignment in the resulting KBox.
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
    , oFromGridCursor = (^. _1)
    , oFromGridChildren = eHead
    }

vertical :: Orientation
vertical = Orientation
    { oToGridCursor = (0 `Vector2`)
    , oToGridChildren = map (: [])
    , oFromGridCursor = (^. _2)
    , oFromGridChildren = map eHead
    }

type Element = Grid.Element

{-# INLINE elementRect #-}
elementRect :: Lens.Getter (Element a) Rect
elementRect = Grid.elementRect

{-# INLINE elementAlign #-}
elementAlign :: Lens.Getter (Element a) Alignment
elementAlign = Grid.elementAlign

{-# INLINE elementOriginalWidget #-}
elementOriginalWidget :: Lens.Getter (Element a) (Widget a)
elementOriginalWidget = Grid.elementOriginalWidget

data KBox key a = KBox
    { __boxOrientation :: Orientation
    , __boxMCursor :: Maybe Cursor
    , __boxSize :: Size
    , __boxContent :: [(key, Element a)]
    }
Lens.makeLenses ''KBox

{-# INLINE boxOrientation #-}
boxOrientation :: Lens.Getter (KBox key a) Orientation
boxOrientation = _boxOrientation

{-# INLINE boxMCursor #-}
boxMCursor :: Lens.Getter (KBox key a) (Maybe Cursor)
boxMCursor = _boxMCursor

{-# INLINE boxSize #-}
boxSize :: Lens.Getter (KBox key a) Size
boxSize = _boxSize

{-# INLINE boxContent #-}
boxContent :: Lens.Getter (KBox key a) [(key, Element a)]
boxContent = _boxContent

type Box = KBox ()

makeKeyed :: Orientation -> [(key, (Alignment, Widget a))] -> (KBox key a, Widget a)
makeKeyed orientation children =
    ( KBox
      { __boxOrientation = orientation
      , __boxMCursor = fmap (oFromGridCursor orientation) $ grid ^. Grid.gridMCursor
      , __boxSize = grid ^. Grid.gridSize
      , __boxContent = oFromGridChildren orientation $ grid ^. Grid.gridContent
      }
    , Grid.toWidget grid
    )
    where
        grid = Grid.makeKeyed $ oToGridChildren orientation children

unkey :: [(Alignment, Widget a)] -> [((), (Alignment, Widget a))]
unkey = map ((,) ())

make :: Orientation -> [(Alignment, Widget a)] -> (Box a, Widget a)
make orientation = makeKeyed orientation . unkey

makeAlign :: Alignment -> Orientation -> [Widget a] -> (Box a, Widget a)
makeAlign alignment orientation = make orientation . map ((,) alignment)

boxAlign :: Orientation -> Alignment -> [Widget a] -> Widget a
boxAlign orientation align = snd . makeAlign align orientation

hboxAlign :: Alignment -> [Widget a] -> Widget a
hboxAlign = boxAlign horizontal

vboxAlign :: Alignment -> [Widget a] -> Widget a
vboxAlign = boxAlign vertical

vboxCentered :: [Widget a] -> Widget a
vboxCentered = vboxAlign 0.5

hboxCentered :: [Widget a] -> Widget a
hboxCentered = hboxAlign 0.5

hbox :: [(Alignment, Widget a)] -> Widget a
hbox = snd . make horizontal

vbox :: [(Alignment, Widget a)] -> Widget a
vbox = snd . make vertical
