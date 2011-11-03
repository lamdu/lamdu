module GridEdit(Model, make) where

import Data.Vector.Vector2 (Vector2(..))
import EventMap (EventMap(..))
import qualified Graphics.DrawingCombinators as Draw
import qualified GridView
import Sized (Sized(..))

type Cursor = Vector2 Int
type Model = Cursor

make :: (Model -> k) -> [[(Sized (Draw.Image ()), Maybe (EventMap k))]] -> Model -> (Sized (Draw.Image ()), Maybe (EventMap k))
make liftModel children cursor =
  (GridView.make ((map . map) fst children), Nothing)
