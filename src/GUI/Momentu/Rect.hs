{-# LANGUAGE TemplateHaskell #-}
module GUI.Momentu.Rect
    ( R, Rect(..), topLeft, size
    , Range(..), rangeStart, rangeSize, rangeStop
    , topLeftAndSize, verticalRange, horizontalRange
    , left, top, right, bottom
    , width, height
    , bottomRight
    , center, centeredSize
    , distances, rangeDistance
    , sqrDistance, sqrPointDistance
    , isWithin, overlap, rectWithin
    ) where

import           Control.DeepSeq (NFData(..))
import           Control.Lens (Traversal')
import qualified Control.Lens as Lens
import           Data.Vector.Vector2 (Vector2(..))
import qualified Data.Vector.Vector2 as Vector2
import           Foreign.C.Types.Instances ()

import           GUI.Momentu.Prelude

type R = Double

data Rect = Rect
    { _topLeft :: !(Vector2 R)
    , _size :: !(Vector2 R)
    }
    deriving stock (Show, Generic, Eq, Ord)
    deriving anyclass NFData
Lens.makeLenses ''Rect

{-# INLINE topLeftAndSize #-}
topLeftAndSize :: Traversal' Rect (Vector2 R)
topLeftAndSize f (Rect tl s) = Rect <$> f tl <*> f s

data Range a = Range
    { _rangeStart :: a
    , _rangeSize :: a
    } deriving (Functor, Foldable, Traversable)

Lens.makeLenses ''Range

rangeStop :: Num a => Lens' (Range a) a
rangeStop f (Range start sz) =
    f (start + sz) <&> subtract start <&> Range start

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
distances :: Rect -> Rect -> Vector2 R
distances r1 r2 =
    Vector2
    (rangeDistance (r1 ^. horizontalRange) (r2 ^. horizontalRange))
    (rangeDistance (r1 ^. verticalRange) (r2 ^. verticalRange))

rangeDistance :: (Num a, Ord a) => Range a -> Range a -> a
rangeDistance range0 range1  =
    max (l1 - r0) (l0 - r1) & max 0
    where
        l0 = range0 ^. rangeStart
        r0 = range0 ^. rangeStop
        l1 = range1 ^. rangeStart
        r1 = range1 ^. rangeStop

sqrDistance :: Rect -> Rect -> R
sqrDistance r1 r2 = Vector2.sqrNorm (distances r1 r2)

sqrPointDistance :: Vector2 R -> Rect -> R
sqrPointDistance p r = Vector2.sqrNorm (distances (Rect p 0) r)

both :: (Applicative f, Foldable f) => (a -> b -> Bool) -> f a -> f b -> Bool
both p v0 v1 = p <$> v0 <*> v1 & and

isWithin :: Vector2 R -> Rect -> Bool
v `isWithin` r =
    both inside (v - r ^. topLeft) (r ^. size)
    where
        inside x l = 0 <= x && x <= l

rectWithin :: Rect -> Rect -> Bool
r0 `rectWithin` r1 = all (`isWithin` r1) [r0 ^. topLeft, r0 ^. bottomRight]

overlap :: Rect -> Rect -> Bool
overlap r0 r1 =
    both (<) (r0 ^. topLeft) (r1 ^. bottomRight)
    && both (<) (r1 ^. topLeft) (r0 ^. bottomRight)
