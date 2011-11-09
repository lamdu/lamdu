module GridEdit(Model, make) where

import Control.Arrow (second)
import Control.Newtype (unpack)
import Data.Vector.Vector2 (Vector2(..))
import EventMap (EventMap(..))
import qualified Graphics.DrawingCombinators as Draw
import qualified GridView
import Sized (Sized(..))
import Widget (Widget(..))

type Cursor = Vector2 Int
type Model = Cursor

make :: (Model -> k) -> [[Widget k]] -> Model -> Widget k
make liftModel children (Vector2 x y) =
  Widget .
  (fmap . second) (snd . (!! x) . (!! y)) .
  GridView.makeGeneric fst .
  (map . map) unpack $
  children
