{-# LANGUAGE TemplateHaskell #-}
module GUI.Momentu.Direction
    ( Orientation(..), _Horizontal, _Vertical
    , Order(..), _Forward, _Backward
    , reverseOrder, applyOrder
    , axis
    ) where

import qualified Control.Lens as Lens
import           Data.Vector.Vector2 (Vector2(..))

import           Lamdu.Prelude

data Orientation = Horizontal | Vertical
    deriving (Eq, Show, Ord, Generic)

axis :: Functor f => Orientation -> Lens.LensLike' f (Vector2 a) a
axis Horizontal = _1
axis Vertical = _2

data Order = Forward | Backward
    deriving (Eq, Show, Ord, Generic)

reverseOrder :: Order -> Order
reverseOrder Forward = Backward
reverseOrder Backward = Forward

applyOrder :: Order -> (a -> a -> b) -> a -> a -> b
applyOrder Forward = id
applyOrder Backward = flip

Lens.makePrisms ''Orientation
Lens.makePrisms ''Order
