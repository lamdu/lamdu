{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE TemplateHaskell #-}

module Graphics.UI.Bottle.SizeRange
    (Coordinate, Size,
     SizeRange(..), atSrMinSize, atSrMaxSize,
     lift2, fixedSize, expanding,
     horizontallyExpanding, verticallyExpanding,
     )
where

import Data.Vector.Vector2         (Vector2(..))
import Control.Applicative         (pure, liftA2)
import Graphics.DrawingCombinators (R)
import qualified Data.AtFieldTH as AtFieldTH

type Coordinate = Vector2 R
type Size = Coordinate
type MaxSize = Vector2 (Maybe R)

data SizeRange = SizeRange {
  srMinSize :: Size,
  srMaxSize :: MaxSize
  }
  deriving (Eq, Ord, Show, Read)

AtFieldTH.make ''SizeRange

lift2 :: (a -> R -> R) -> Vector2 a -> SizeRange -> SizeRange
lift2 f v (SizeRange minSize maxSize) =
  SizeRange
  { srMinSize = liftA2 f v minSize
  , srMaxSize = (liftA2 . liftA2) f (fmap Just v) maxSize
  }

fixedSize :: Size -> SizeRange
fixedSize size = SizeRange size (fmap Just size)

horizontallyExpanding :: R -> R -> SizeRange
horizontallyExpanding fixedHeight minWidth =
  SizeRange (Vector2 minWidth fixedHeight)
            (Vector2 Nothing (Just fixedHeight))

verticallyExpanding :: R -> R -> SizeRange
verticallyExpanding fixedWidth minHeight =
  SizeRange (Vector2 fixedWidth minHeight)
            (Vector2 (Just fixedWidth) Nothing)

expanding :: R -> R -> SizeRange
expanding minWidth minHeight =
  SizeRange (Vector2 minWidth minHeight)
            (pure Nothing)
