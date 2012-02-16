{-# LANGUAGE TemplateHaskell #-}
module Graphics.UI.Bottle.Rect(
  R, Rect(..), atRectTopLeft, atRectSize, bottomRight, center)
where

import Data.Vector.Vector2 (Vector2(..))
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
