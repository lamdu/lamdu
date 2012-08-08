{-# LANGUAGE TemplateHaskell #-}
module Graphics.UI.Bottle.Rect
  ( R, Rect(..), topLeft, size
  , topLeftAndSize
  , left, top, right, bottom
  , width, height
  , bottomRight, center, distance
  ) where

import Control.Applicative (liftA2)
import Control.Lens (Simple, Traversal, Lens, (^.))
import Data.Vector.Vector2 (Vector2(..))
import Graphics.DrawingCombinators(R)
import qualified Control.Lens.TH as LensTH
import qualified Data.Vector.Vector2 as Vector2

data Rect = Rect {
  _topLeft :: Vector2 R,
  _size :: Vector2 R
  } deriving Show
LensTH.makeLenses ''Rect

topLeftAndSize :: Simple Traversal Rect (Vector2 R)
topLeftAndSize f (Rect tl s) = liftA2 Rect (f tl) (f s)

bottomRight :: Simple Lens Rect (Vector2 R)
bottomRight f (Rect tl s) =
  fmap withNew $ f (tl + s)
  where
    withNew newBottomRight =
      Rect tl (newBottomRight - tl)

center :: Simple Lens Rect (Vector2 R)
center f (Rect tl s) =
  fmap withNew $ f centerVal
  where
    centerVal = tl + s / 2
    withNew newCenter =
      Rect (tl + newCenter - centerVal) s

left :: Simple Lens Rect R
left = topLeft . Vector2.first

top :: Simple Lens Rect R
top = topLeft . Vector2.second

right :: Simple Lens Rect R
right = bottomRight . Vector2.first

bottom :: Simple Lens Rect R
bottom = bottomRight . Vector2.second

width :: Simple Lens Rect R
width = size . Vector2.first

height :: Simple Lens Rect R
height = size . Vector2.second

distance :: Rect -> Rect -> R
distance r1 r2 = Vector2.sqrNorm dist2
  where
    max2 = liftA2 max
    dist2 = max2 (max2 0 (tl2 - br1)) (max2 0 (tl1 - br2))
    tl1 = r1 ^. topLeft
    tl2 = r2 ^. topLeft
    br1 = r1 ^. bottomRight
    br2 = r2 ^. bottomRight
