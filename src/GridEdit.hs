module GridEdit(Model, make) where

import Data.Vector.Vector2 (Vector2(..))
import EventMap (EventMap(..))
import qualified GridView
import SizedImage (SizedImage(..))

type Cursor = Vector2 Int
type Model = Cursor

make :: (Model -> k) -> [[(SizedImage, Maybe (EventMap k))]] -> Model -> (SizedImage, Maybe (EventMap k))
make liftModel children cursor =
  (GridView.make ((map . map) fst children), Nothing)
