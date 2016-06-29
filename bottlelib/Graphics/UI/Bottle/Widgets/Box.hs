{-# LANGUAGE Rank2Types, TemplateHaskell #-}
module Graphics.UI.Bottle.Widgets.Box
    ( Box(..), Alignment(..), Grid.alignmentRatio
    , make, makeAlign
    , boxMCursor, boxContent
    , Cursor
    , Orientation(..)
    , hboxAlign, vboxAlign
    , hboxCentered, vboxCentered
    , hbox, vbox
    ) where

import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Lens.Tuple
import           Graphics.UI.Bottle.Widget (Widget)
import           Graphics.UI.Bottle.Widgets.Grid (Alignment(..))
import qualified Graphics.UI.Bottle.Widgets.Grid as Grid

type Cursor = Int

eHead :: [a] -> a
eHead (x:_) = x
eHead [] = error "Grid returned invalid list without any elements, instead of list Box handed it"

-- Want a 2d vector like in Grid, because we may want to preserve the
-- alignment in the resulting Box.
data Orientation = Horizontal | Vertical
    deriving (Eq)

data Box t = Box
    { _boxMCursor :: Maybe Cursor
    , _boxContent :: t Alignment
    }
Lens.makeLenses ''Box

make ::
    Traversable t =>
    Orientation -> t (Alignment, Widget a) -> (Box t, Widget a)
make Horizontal children =
    Grid.make [children] & _1 %~ boxify
    where
        boxify grid =
            Box
            { _boxMCursor = grid ^. Grid.gridMCursor <&> (^. _1)
            , _boxContent = grid ^. Grid.gridContent & eHead
            }
make Vertical children =
    children <&> (:[]) & Grid.make & _1 %~ boxify
    where
        boxify grid =
            Box
            { _boxMCursor = grid ^. Grid.gridMCursor <&> (^. _2)
            , _boxContent = grid ^. Grid.gridContent <&> eHead
            }

makeAlign ::
    Traversable t =>
    Alignment -> Orientation -> t (Widget a) -> (Box t, Widget a)
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
