module Graphics.UI.Bottle.Geometry(
  rectScore)
where

import Control.Applicative (liftA2)
import Data.Vector.Vector2 (Vector2(..))
import Graphics.DrawingCombinators(R)
import Graphics.UI.Bottle.Rect (Rect(..))
import qualified Graphics.UI.Bottle.Rect as Rect

rectScore:: Vector2 R -> Rect -> Vector2 Int -> Rect -> ([Int], R)
rectScore size entryRect (Vector2 row col) enterResultRect =
  (borderScore, Rect.distance entryRect enterResultRect)
    where
      borderScore =
        concat [[col | fromLeft], [-col | fromRight],
                [row | fromTop], [-row | fromBottom]]
      Vector2 fromLeft fromTop = fmap (<= 0) (Rect.bottomRight entryRect)
      Vector2 fromRight fromBottom = liftA2 (>=) (Rect.rectTopLeft entryRect) size
