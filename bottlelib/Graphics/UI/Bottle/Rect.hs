{-# LANGUAGE TemplateHaskell #-}
module Graphics.UI.Bottle.Rect(
  R, Rect(..), atRectTopLeft, atRectSize, bottomRight, center,
  distance)
where

import Control.Applicative (liftA2)
import Data.Vector.Vector2 (Vector2(..), sqrNorm)
import Graphics.DrawingCombinators(R)
import qualified Data.AtFieldTH as AtFieldTH

data Rect = Rect {
  rectTopLeft :: Vector2 R,
  rectSize :: Vector2 R
  } deriving Show
AtFieldTH.make ''Rect

center :: Rect -> Vector2 R
center (Rect tl size) = tl + size / 2

bottomRight :: Rect -> Vector2 R
bottomRight (Rect tl size) = tl + size

distance :: Rect -> Rect -> R
distance r1 r2 = sqrNorm dist2
  where
    max2 = liftA2 max
    dist2 = max2 (max2 0 (tl2 - br1)) (max2 0 (tl1 - br2))
    tl1 = rectTopLeft r1
    tl2 = rectTopLeft r2
    br1 = bottomRight r1
    br2 = bottomRight r2
