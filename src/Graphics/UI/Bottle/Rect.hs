{-# LANGUAGE NoImplicitPrelude, DeriveGeneric, TemplateHaskell, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Graphics.UI.Bottle.Rect
    ( R, Rect(..), topLeft, size
    , Range(..), rangeStart, rangeSize
    , topLeftAndSize, verticalRange, horizontalRange
    , left, top, right, bottom
    , width, height
    , bottomRight
    , center, centeredSize
    , distance
    ) where

import           Control.DeepSeq (NFData(..))
import           Control.DeepSeq.Generics (genericRnf)
import           Control.Lens (Traversal')
import qualified Control.Lens as Lens
import           Data.Vector.Vector2 (Vector2(..))
import           Foreign.C.Types.Instances ()
import           GHC.Generics (Generic)
import           Graphics.DrawingCombinators (R)

import           Lamdu.Prelude

data Rect = Rect
    { _topLeft :: !(Vector2 R)
    , _size :: !(Vector2 R)
    } deriving (Show, Generic)
Lens.makeLenses ''Rect

instance NFData Rect where rnf = genericRnf

{-# INLINE topLeftAndSize #-}
topLeftAndSize :: Traversal' Rect (Vector2 R)
topLeftAndSize f (Rect tl s) = Rect <$> f tl <*> f s

data Range a = Range
    { _rangeStart :: a
    , _rangeSize :: a
    } deriving (Functor, Foldable, Traversable)

Lens.makeLenses ''Range


{-# INLINE verticalRange #-}
verticalRange :: Lens' Rect (Range R)
verticalRange f (Rect (Vector2 l t) (Vector2 w h)) =
    f (Range t h) <&> \(Range t' h') -> Rect (Vector2 l t') (Vector2 w h')

{-# INLINE horizontalRange #-}
horizontalRange :: Lens' Rect (Range R)
horizontalRange f (Rect (Vector2 l t) (Vector2 w h)) =
    f (Range l w) <&> \(Range l' w') -> Rect (Vector2 l' t) (Vector2 w' h)

{-# INLINE bottomRight #-}
bottomRight :: Lens' Rect (Vector2 R)
bottomRight f (Rect tl s) =
    withNew <$> f (tl + s)
    where
        withNew newBottomRight =
            Rect tl (newBottomRight - tl)

{-# INLINE centeredSize #-}
centeredSize :: Lens' Rect (Vector2 R)
centeredSize f (Rect tl oldSize) =
    f oldSize <&> \newSize -> Rect (oldCenter - newSize / 2) newSize
    where
        oldCenter = tl + oldSize / 2

{-# INLINE center #-}
center :: Lens' Rect (Vector2 R)
center f (Rect tl s) =
    f oldCenter
    <&> \newCenter -> Rect (tl + newCenter - oldCenter) s
    where
        oldCenter = tl + s / 2

{-# INLINE left #-}
left :: Lens' Rect R
left = topLeft . _1

{-# INLINE top #-}
top :: Lens' Rect R
top = topLeft . _2

{-# INLINE right #-}
right :: Lens' Rect R
right = bottomRight . _1

{-# INLINE bottom #-}
bottom :: Lens' Rect R
bottom = bottomRight . _2

{-# INLINE width #-}
width :: Lens' Rect R
width = size . _1

{-# INLINE height #-}
height :: Lens' Rect R
height = size . _2

-- | Returns the linear distance between the 2 closest points in 2
-- rects
distance :: Rect -> Rect -> Vector2 R
distance r1 r2 =
    max
    <$> tl2 - br1
    <*> tl1 - br2
    <&> max 0
    where
        tl1 = r1 ^. topLeft
        tl2 = r2 ^. topLeft
        br1 = r1 ^. bottomRight
        br2 = r2 ^. bottomRight
