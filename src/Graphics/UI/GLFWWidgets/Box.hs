{-# OPTIONS -Wall #-}
{-# LANGUAGE Rank2Types #-}
module Graphics.UI.GLFWWidgets.Box(Cursor, make, Orientation, horizontal, vertical) where

import Graphics.UI.GLFWWidgets.Widget(Widget)
import qualified Graphics.UI.GLFWWidgets.Grid as Grid
import qualified Data.Vector.Vector2 as Vector2
import Data.Vector.Vector2(Vector2(..))

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

make :: Orientation -> (Cursor -> k) -> Cursor -> [Widget k] -> Widget k
make orientation liftCursor cursor children =
  Grid.make
    (liftCursor . fromGridCursor orientation)
    (toGridCursor orientation cursor)
    (toGridChildren orientation children)
