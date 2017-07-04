{-# LANGUAGE Rank2Types #-}
module Graphics.UI.Bottle.Widgets.Box
    ( Alignment(..)
    , make, makeAlign
    , makePlacements
    , Cursor
    , Orientation(..)
    , hboxAlign, vboxAlign
    ) where

import qualified Control.Lens as Lens
import           Data.Functor.Identity (Identity(..))
import           Graphics.UI.Bottle.Rect (Rect)
import           Graphics.UI.Bottle.View (Size)
import           Graphics.UI.Bottle.Widget (Widget)
import           Graphics.UI.Bottle.Widgets.Grid (Alignment(..))
import qualified Graphics.UI.Bottle.Widgets.Grid as Grid
import qualified Graphics.UI.Bottle.Widgets.GridView as GridView

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

makePlacements ::
    Traversable t =>
    Orientation -> t (Alignment, Size, a) -> (Size, t (Alignment, Rect, a))
makePlacements Horizontal x = Identity x & GridView.makePlacements & _2 %~ runIdentity
makePlacements Vertical x = x <&> Identity & GridView.makePlacements & _2 . Lens.mapped %~ runIdentity

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
