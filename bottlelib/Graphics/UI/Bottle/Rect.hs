{-# LANGUAGE TemplateHaskell #-}
module Graphics.UI.Bottle.Rect
  ( R, Rect(..), topLeft, size
  , topLeftAndSize
  , left, top, right, bottom
  , width, height
  , bottomRight, center, distance
  ) where

import Control.Applicative (liftA2)
import Control.Lens (Traversal', Lens', (^.), _1, _2)
import Data.Vector.Vector2 (Vector2(..))
import Graphics.DrawingCombinators(R)
import qualified Control.Lens as Lens
import qualified Data.Vector.Vector2 as Vector2

data Rect = Rect {
  _topLeft :: Vector2 R,
  _size :: Vector2 R
  } deriving Show
Lens.makeLenses ''Rect

topLeftAndSize :: Traversal' Rect (Vector2 R)
topLeftAndSize f (Rect tl s) = liftA2 Rect (f tl) (f s)

bottomRight :: Lens' Rect (Vector2 R)
bottomRight f (Rect tl s) =
  fmap withNew $ f (tl + s)
  where
    withNew newBottomRight =
      Rect tl (newBottomRight - tl)

center :: Lens' Rect (Vector2 R)
center f (Rect tl s) =
  fmap withNew $ f centerVal
  where
    centerVal = tl + s / 2
    withNew newCenter =
      Rect (tl + newCenter - centerVal) s

left :: Lens' Rect R
left = topLeft . _1

top :: Lens' Rect R
top = topLeft . _2

right :: Lens' Rect R
right = bottomRight . _1

bottom :: Lens' Rect R
bottom = bottomRight . _2

width :: Lens' Rect R
width = size . _1

height :: Lens' Rect R
height = size . _2

distance :: Rect -> Rect -> R
distance r1 r2 = Vector2.sqrNorm dist2
  where
    max2 = liftA2 max
    dist2 = max2 (max2 0 (tl2 - br1)) (max2 0 (tl1 - br2))
    tl1 = r1 ^. topLeft
    tl2 = r2 ^. topLeft
    br1 = r1 ^. bottomRight
    br2 = r2 ^. bottomRight
