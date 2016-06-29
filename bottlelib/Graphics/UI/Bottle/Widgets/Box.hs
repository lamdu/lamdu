{-# LANGUAGE Rank2Types, TemplateHaskell #-}
module Graphics.UI.Bottle.Widgets.Box
    ( Box, KBox(..), Alignment(..), Grid.alignmentRatio
    , make, makeKeyed, makeAlign
    , unkey
    , boxMCursor, boxContent
    , Element, elementRect, elementAlign, elementOriginalWidget
    , Cursor
    , Orientation(..)
    , hboxAlign, vboxAlign
    , hboxCentered, vboxCentered
    , hbox, vbox
    ) where

import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Lens.Tuple
import           Graphics.UI.Bottle.Rect (Rect(..))
import           Graphics.UI.Bottle.Widget (Widget)
import           Graphics.UI.Bottle.Widgets.Grid (Alignment(..))
import qualified Graphics.UI.Bottle.Widgets.Grid as Grid

type Cursor = Int

eHead :: [a] -> a
eHead (x:_) = x
eHead [] = error "Grid returned invalid list without any elements, instead of list Box handed it"

-- Want a 2d vector like in Grid, because we may want to preserve the
-- alignment in the resulting KBox.
data Orientation = Horizontal | Vertical
    deriving (Eq)

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

data KBox t key a = KBox
    { __boxMCursor :: Maybe Cursor
    , __boxContent :: t (key, Element a)
    }
Lens.makeLenses ''KBox

{-# INLINE boxMCursor #-}
boxMCursor :: Lens.Getter (KBox t key a) (Maybe Cursor)
boxMCursor = _boxMCursor

{-# INLINE boxContent #-}
boxContent :: Lens.Getter (KBox t key a) (t (key, Element a))
boxContent = _boxContent

type Box = KBox [] ()

makeKeyed ::
    Traversable t =>
    Orientation -> t (key, (Alignment, Widget a)) -> (KBox t key a, Widget a)
makeKeyed Horizontal children =
    ( KBox
      { __boxMCursor = grid ^. Grid.gridMCursor <&> (^. _1)
      , __boxContent = grid ^. Grid.gridContent & eHead
      }
    , Grid.toWidget grid
    )
    where
        grid = Grid.makeKeyed [children]
makeKeyed Vertical children =
    ( KBox
      { __boxMCursor = grid ^. Grid.gridMCursor <&> (^. _2)
      , __boxContent = grid ^. Grid.gridContent <&> eHead
      }
    , Grid.toWidget grid
    )
    where
        grid = children <&> (:[]) & Grid.makeKeyed

unkey :: Functor f => f (Alignment, Widget a) -> f ((), (Alignment, Widget a))
unkey = fmap ((,) ())

make :: Traversable t => Orientation -> t (Alignment, Widget a) -> (KBox t () a, Widget a)
make orientation = makeKeyed orientation . unkey

makeAlign ::
    Traversable t =>
    Alignment -> Orientation -> t (Widget a) -> (KBox t () a, Widget a)
makeAlign alignment orientation = make orientation . fmap ((,) alignment)

boxAlign ::
    Traversable t => Orientation -> Alignment -> t (Widget a) -> Widget a
boxAlign orientation align = snd . makeAlign align orientation

hboxAlign :: Traversable t => Alignment -> t (Widget a) -> Widget a
hboxAlign = boxAlign Horizontal

vboxAlign :: Traversable t => Alignment -> t (Widget a) -> Widget a
vboxAlign = boxAlign Vertical

vboxCentered :: Traversable t => t (Widget a) -> Widget a
vboxCentered = vboxAlign 0.5

hboxCentered :: Traversable t => t (Widget a) -> Widget a
hboxCentered = hboxAlign 0.5

hbox :: Traversable t => t (Alignment, Widget a) -> Widget a
hbox = snd . make Horizontal

vbox :: Traversable t => t (Alignment, Widget a) -> Widget a
vbox = snd . make Vertical
