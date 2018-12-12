{-# LANGUAGE TemplateHaskell #-}
module GUI.Momentu.Direction
    ( Orientation(..), _Horizontal, _Vertical
    , LogicalOrder(..), _Forward, _Backward
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

data LogicalOrder = Forward | Backward
    deriving (Eq, Show, Ord, Generic)

Lens.makePrisms ''Orientation
Lens.makePrisms ''LogicalOrder
