{-# LANGUAGE Rank2Types #-}
module Graphics.UI.Bottle.Widgets.Box
    ( Alignment(..)
    , make, makeAlign
    , Cursor
    , Orientation(..)
    , hboxAlign, vboxAlign
    , hboxCentered, vboxCentered
    , hbox, vbox
    ) where

import qualified Control.Lens as Lens
import           Data.Functor.Identity (Identity(..))
import           Graphics.UI.Bottle.Widget (Widget)
import           Graphics.UI.Bottle.Widgets.Grid (Alignment(..))
import qualified Graphics.UI.Bottle.Widgets.Grid as Grid

import           Lamdu.Prelude

type Cursor = Int

-- Want a 2d vector like in Grid, because we may want to preserve the
-- alignment in the resulting Box.
data Orientation = Horizontal | Vertical
    deriving (Eq)

make ::
    Traversable t =>
    Orientation -> t (Alignment, Widget a) -> (t Alignment, Widget a)
make Horizontal x = Grid.make (Identity x) & _1 %~ runIdentity
make Vertical x = x <&> Identity & Grid.make & _1 . Lens.mapped %~ runIdentity

makeAlign ::
    Traversable t =>
    Alignment -> Orientation -> t (Widget a) -> (t Alignment, Widget a)
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
