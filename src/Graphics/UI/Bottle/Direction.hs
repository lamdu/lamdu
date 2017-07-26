{-# LANGUAGE NoImplicitPrelude, TemplateHaskell #-}
module Graphics.UI.Bottle.Direction
    ( Direction(..), translate, scale
    ) where

import           Data.Vector.Vector2 (Vector2(..))
import           Graphics.UI.Bottle.Rect (R, Range, rangeStart)

import           Lamdu.Prelude

-- RelativePos pos is relative to the top-left of the widget
data Direction
    = Outside
    | Point (Vector2 R)
    | FromAbove (Range R) -- ^ horizontal virtual cursor
    | FromBelow (Range R) -- ^ horizontal virtual cursor
    | FromLeft  (Range R) -- ^ vertical virtual cursor
    | FromRight (Range R) -- ^ vertical virtual cursor

translate :: Vector2 R -> Direction -> Direction
translate _ Outside = Outside
translate pos (Point x) = x + pos & Point
translate pos (FromAbove r) = r & rangeStart +~ pos ^. _1 & FromAbove
translate pos (FromBelow r) = r & rangeStart +~ pos ^. _1 & FromBelow
translate pos (FromLeft  r) = r & rangeStart +~ pos ^. _2 & FromLeft
translate pos (FromRight r) = r & rangeStart +~ pos ^. _2 & FromRight

scale :: Vector2 R -> Direction -> Direction
scale _ Outside = Outside
scale ratio (Point x) = x * ratio & Point
scale ratio (FromAbove r) = r <&> (* ratio ^. _1) & FromAbove
scale ratio (FromBelow r) = r <&> (* ratio ^. _1) & FromBelow
scale ratio (FromLeft  r) = r <&> (* ratio ^. _2) & FromLeft
scale ratio (FromRight r) = r <&> (* ratio ^. _2) & FromRight
