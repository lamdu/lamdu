{-# OPTIONS -O2 -Wall #-}

module Graphics.UI.Bottle.SizeRange
    (Coordinate, Size,
     SizeRange(..),
     fixedSize, expanding,
     horizontallyExpanding, verticallyExpanding,
     )
where

import Data.Vector.Vector2         (Vector2(..))
import Control.Applicative         (pure)
import Graphics.DrawingCombinators (R)

type Coordinate = Vector2 R
type Size = Coordinate
type MaxSize = Vector2 (Maybe R)

data SizeRange = SizeRange {
  srMinSize :: Size,
  srMaxSize :: MaxSize
  }
  deriving (Eq, Ord, Show, Read)

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
