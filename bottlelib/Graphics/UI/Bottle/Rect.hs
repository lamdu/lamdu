{-# LANGUAGE NoImplicitPrelude, DeriveGeneric, TemplateHaskell #-}
module Graphics.UI.Bottle.Rect
    ( R, Rect(..), topLeft, size
    , topLeftAndSize
    , left, top, right, bottom
    , width, height
    , bottomRight
    , center, centeredSize
    , distance
    ) where

import           Prelude.Compat

import           Control.Applicative (liftA2)
import           Control.DeepSeq (NFData(..))
import           Control.DeepSeq.Generics (genericRnf)
import           Control.Lens (Traversal', Lens')
import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Lens.Tuple
import           Data.Vector.Vector2 (Vector2(..))
import qualified Data.Vector.Vector2 as Vector2
import           Foreign.C.Types.Instances ()
import           GHC.Generics (Generic)
import           Graphics.DrawingCombinators (R)

data Rect = Rect
    { _topLeft :: Vector2 R
    , _size :: Vector2 R
    } deriving (Show, Generic)
Lens.makeLenses ''Rect

instance NFData Rect where rnf = genericRnf

{-# INLINE topLeftAndSize #-}
topLeftAndSize :: Traversal' Rect (Vector2 R)
topLeftAndSize f (Rect tl s) = liftA2 Rect (f tl) (f s)

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

distance :: Rect -> Rect -> R
distance r1 r2 = Vector2.sqrNorm dist2
    where
        max2 = liftA2 max
        dist2 = max2 (max2 0 (tl2 - br1)) (max2 0 (tl1 - br2))
        tl1 = r1 ^. topLeft
        tl2 = r2 ^. topLeft
        br1 = r1 ^. bottomRight
        br2 = r2 ^. bottomRight
