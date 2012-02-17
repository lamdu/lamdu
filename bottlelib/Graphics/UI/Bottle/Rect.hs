{-# LANGUAGE TemplateHaskell #-}
module Graphics.UI.Bottle.Rect(
  R, Rect(..),
  atRectTopLeft, atLeft, atTop,
  atRectSize, atWidth, atHeight, width, height,
  bottomRight, center, distance)
where

import Control.Applicative (liftA2)
import Data.Vector.Vector2 (Vector2(..))
import Graphics.DrawingCombinators(R)
import qualified Data.AtFieldTH as AtFieldTH
import qualified Data.Vector.Vector2 as Vector2

data Rect = Rect {
  rectTopLeft :: Vector2 R,
  rectSize :: Vector2 R
  } deriving Show
AtFieldTH.make ''Rect

width :: Rect -> R
width = Vector2.fst . rectSize

height :: Rect -> R
height = Vector2.snd . rectSize

atLeft :: (R -> R) -> Rect -> Rect
atLeft = atRectTopLeft . Vector2.first

atTop :: (R -> R) -> Rect -> Rect
atTop = atRectTopLeft . Vector2.second

atWidth :: (R -> R) -> Rect -> Rect
atWidth = atRectSize . Vector2.first

atHeight :: (R -> R) -> Rect -> Rect
atHeight = atRectSize . Vector2.second

center :: Rect -> Vector2 R
center (Rect tl size) = tl + size / 2

bottomRight :: Rect -> Vector2 R
bottomRight (Rect tl size) = tl + size

distance :: Rect -> Rect -> R
distance r1 r2 = Vector2.sqrNorm dist2
  where
    max2 = liftA2 max
    dist2 = max2 (max2 0 (tl2 - br1)) (max2 0 (tl1 - br2))
    tl1 = rectTopLeft r1
    tl2 = rectTopLeft r2
    br1 = bottomRight r1
    br2 = bottomRight r2
