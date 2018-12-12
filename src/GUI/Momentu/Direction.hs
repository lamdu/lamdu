{-# LANGUAGE TemplateHaskell #-}
module GUI.Momentu.Direction
    ( Orientation(..), _Horizontal, _Vertical
    , perpendicular, axis, rectRange
    , Order(..), _Forward, _Backward
    , reverseOrder, applyOrder
    , name
    ) where

import qualified Control.Lens as Lens
import           Data.String (IsString(..))
import           Data.Vector.Vector2 (Vector2(..))
import           GUI.Momentu.Rect (Rect, R)
import qualified GUI.Momentu.Rect as Rect

import           Lamdu.Prelude

data Orientation = Horizontal | Vertical
    deriving (Eq, Show, Ord, Generic)

axis :: Functor f => Orientation -> Lens.LensLike' f (Vector2 a) a
axis Horizontal = _1
axis Vertical = _2

perpendicular :: Orientation -> Orientation
perpendicular Horizontal = Vertical
perpendicular Vertical = Horizontal

rectRange :: Functor f => Orientation -> Lens.LensLike' f Rect (Rect.Range R)
rectRange Horizontal = Rect.horizontalRange
rectRange Vertical = Rect.verticalRange

data Order = Forward | Backward
    deriving (Eq, Show, Ord, Generic)

reverseOrder :: Order -> Order
reverseOrder Forward = Backward
reverseOrder Backward = Forward

applyOrder :: Order -> (a -> a -> b) -> a -> a -> b
applyOrder Forward = id
applyOrder Backward = flip

name :: IsString a => Orientation -> Order -> a
name Horizontal Backward = "left"
name Horizontal Forward = "right"
name Vertical Backward = "up"
name Vertical Forward = "down"

Lens.makePrisms ''Orientation
Lens.makePrisms ''Order
