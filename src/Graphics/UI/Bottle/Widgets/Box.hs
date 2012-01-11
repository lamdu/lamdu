{-# OPTIONS -Wall #-}
{-# LANGUAGE Rank2Types, TypeOperators #-}
module Graphics.UI.Bottle.Widgets.Box(Cursor, make, Orientation, horizontal, vertical) where

import Data.Vector.Vector2(Vector2(..))
import Graphics.UI.Bottle.Widget(Widget)
import qualified Data.Vector.Vector2 as Vector2
import qualified Graphics.UI.Bottle.Widgets.Grid as Grid

type Cursor = Int

data Orientation = Orientation {
  toGridCursor :: Cursor -> Grid.Cursor,
  fromGridCursor :: Grid.Cursor -> Cursor,
  toGridChildren :: forall a. [a] -> [[a]]
  }

horizontal :: Orientation
horizontal = Orientation {
  toGridCursor = (`Vector2` 0),
  fromGridCursor = Vector2.fst,
  toGridChildren = (: [])
  }

vertical :: Orientation
vertical = Orientation {
  toGridCursor = (0 `Vector2`),
  fromGridCursor = Vector2.snd,
  toGridChildren = map (: [])
  }

make ::
  Orientation -> (Cursor -> k) -> Cursor ->
  [Bool -> Widget k] -> Bool -> Widget k
make orientation liftCursor cursor children =
  Grid.make
    (liftCursor . fromGridCursor orientation)
    (toGridCursor orientation cursor)
    (toGridChildren orientation children)
