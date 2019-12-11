module GUI.Momentu.FocusDirection
    ( FocusDirection(..), translate, scale
    , GeometricOrigin(..)
    ) where

import           Data.Vector.Vector2 (Vector2(..))
import qualified GUI.Momentu.Direction as Dir
import           GUI.Momentu.Rect (R, Range, rangeStart)

import           GUI.Momentu.Prelude

data GeometricOrigin = GeometricOrigin
    { oOrientation :: Dir.Orientation
    , oGeometricOrder :: Dir.Order
    , oPerpendicularVirtCursorRange :: Range R
      -- ^ The range is the virtual cursor's perpendicular axis to the
      -- orientation. e.g: when Orientation is Vertical, the range is
      -- the horizontal range of the virtual cursor.
    }

-- RelativePos pos is relative to the top-left of the widget
data FocusDirection
    = Point (Vector2 R)
    | FromOutside
    | FromAbove (Range R) -- ^ horizontal virtual cursor
    | FromBelow (Range R) -- ^ horizontal virtual cursor
    | FromLeft  (Range R) -- ^ vertical virtual cursor
    | FromRight (Range R) -- ^ vertical virtual cursor

translate :: Vector2 R -> FocusDirection -> FocusDirection
translate pos (Point x) = x + pos & Point
translate _ FromOutside = FromOutside
translate pos (FromAbove r) = r & rangeStart +~ pos ^. _1 & FromAbove
translate pos (FromBelow r) = r & rangeStart +~ pos ^. _1 & FromBelow
translate pos (FromLeft  r) = r & rangeStart +~ pos ^. _2 & FromLeft
translate pos (FromRight r) = r & rangeStart +~ pos ^. _2 & FromRight

scale :: Vector2 R -> FocusDirection -> FocusDirection
scale _ FromOutside = FromOutside
scale ratio (Point x) = x * ratio & Point
scale ratio (FromAbove r) = r <&> (* ratio ^. _1) & FromAbove
scale ratio (FromBelow r) = r <&> (* ratio ^. _1) & FromBelow
scale ratio (FromLeft  r) = r <&> (* ratio ^. _2) & FromLeft
scale ratio (FromRight r) = r <&> (* ratio ^. _2) & FromRight
