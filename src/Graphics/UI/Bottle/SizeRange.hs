{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE TemplateHaskell #-}

module Graphics.UI.Bottle.SizeRange
    (Coordinate, Size,
     SizeRange(SizeRange), srMinSize, srMaxSize,
     inSizeRange, fixedSize, expanding,
     horizontallyExpanding, verticallyExpanding,
     )
where

import Data.Record.Label           (mkLabels, (:->), lens)
import Data.Vector.Vector2         (Vector2(..))
import Control.Applicative         (pure)
import Graphics.DrawingCombinators (R)

type Coordinate = Vector2 R
type Size = Coordinate
type MaxSize = Vector2 (Maybe R)

data SizeRange = SizeRange {
  _srMinSize :: Size,
  _srMaxSize :: MaxSize
  }
  deriving (Eq, Ord, Show, Read)
$(mkLabels [''SizeRange])

inSizeRange ::
  (Size -> Size) -> (MaxSize -> MaxSize) ->
  SizeRange -> SizeRange
inSizeRange minFunc maxFunc (SizeRange minSize maxSize) =
  SizeRange (minFunc minSize) (maxFunc maxSize)

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
