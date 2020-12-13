{-# LANGUAGE TemplateHaskell #-}
module GUI.Momentu.Direction
    ( Orientation(..), _Horizontal, _Vertical
    , perpendicular, axis
    , Order(..), _Forward, _Backward
    , reverseOrder, applyOrder
    , Texts(..), left, right, up, down
    , textLens
    , Layout(..), _LeftToRight, _RightToLeft
    ) where

import qualified Control.Lens as Lens
import qualified Data.Aeson.TH.Extended as JsonTH
import           Data.Vector.Vector2 (Vector2(..))

import           GUI.Momentu.Prelude

data Layout
    = LeftToRight -- ^ e.g: latin languages
    | RightToLeft -- ^ e.g: Hebrew/Arabic
    deriving (Eq, Ord, Show)
JsonTH.derivePrefixed "" ''Layout
Lens.makePrisms ''Layout

data Orientation = Horizontal | Vertical
    deriving (Eq, Show, Ord, Generic)

axis :: Functor f => Orientation -> Lens.LensLike' f (Vector2 a) a
axis Horizontal = _1
axis Vertical = _2

perpendicular :: Orientation -> Orientation
perpendicular Horizontal = Vertical
perpendicular Vertical = Horizontal

data Order = Forward | Backward
    deriving (Eq, Show, Ord, Generic)

reverseOrder :: Order -> Order
reverseOrder Forward = Backward
reverseOrder Backward = Forward

applyOrder :: Order -> (a -> a -> b) -> a -> a -> b
applyOrder Forward = id
applyOrder Backward = flip

data Texts a = Texts
    { _left :: a
    , _right :: a
    , _up :: a
    , _down :: a
    } deriving Eq

JsonTH.derivePrefixed "_" ''Texts

Lens.makeLenses ''Texts
Lens.makePrisms ''Order
Lens.makePrisms ''Orientation

textLens :: Functor f => Orientation -> Order -> Lens.LensLike' f (Texts a) a
textLens Horizontal Backward = left
textLens Horizontal Forward = right
textLens Vertical Backward = up
textLens Vertical Forward = down
