{-# LANGUAGE NoImplicitPrelude #-}
module Graphics.UI.Bottle.Direction
    ( Direction(..), translate, scale
    ) where

import           Data.Vector.Vector2 (Vector2(..))
import           Graphics.UI.Bottle.Rect (R, Rect(..))
import qualified Graphics.UI.Bottle.Rect as Rect

import           Lamdu.Prelude

-- RelativePos pos is relative to the top-left of the widget
data Direction = Outside | PrevFocalArea Rect | Point (Vector2 R)

translate :: Vector2 R -> Direction -> Direction
translate _ Outside = Outside
translate pos (PrevFocalArea x) = x & Rect.topLeft +~ pos & PrevFocalArea
translate pos (Point x) = x + pos & Point

scale :: Vector2 R -> Direction -> Direction
scale _ Outside = Outside
scale r (PrevFocalArea x) = x & Rect.topLeftAndSize *~ r & PrevFocalArea
scale r (Point x) = x * r & Point
