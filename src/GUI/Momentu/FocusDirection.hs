module GUI.Momentu.FocusDirection
    ( FocusDirection(..), translate, scale
    , GeometricOrigin(..)
    ) where

import           Data.Vector.Vector2 (Vector2(..))
import qualified GUI.Momentu.Direction as Dir
import           GUI.Momentu.Direction (perpendicular, axis)
import           GUI.Momentu.Rect (R, Range, rangeStart)

import           Lamdu.Prelude

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
    | FromGeometric GeometricOrigin

translate :: Vector2 R -> FocusDirection -> FocusDirection
translate pos (Point x) = x + pos & Point
translate _ FromOutside = FromOutside
translate pos (FromGeometric (GeometricOrigin o d r)) =
    r
    & rangeStart +~ pos ^. axis (perpendicular o)
    & GeometricOrigin o d
    & FromGeometric

scale :: Vector2 R -> FocusDirection -> FocusDirection
scale _ FromOutside = FromOutside
scale ratio (Point x) = x * ratio & Point
scale ratio (FromGeometric (GeometricOrigin o d r)) =
    r <&> (* ratio ^. axis (perpendicular o)) & GeometricOrigin o d & FromGeometric
