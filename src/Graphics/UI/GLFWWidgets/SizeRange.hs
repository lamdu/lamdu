{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE TemplateHaskell #-}

module Graphics.UI.GLFWWidgets.SizeRange
    (Coordinate, Size,
     SizeRange(SizeRange), srMinSize, srMaxSize,
     inSizeRange, fixedSize, expanding,
     horizontallyExpanding, verticallyExpanding,
     )
where

import Data.Label                  (mkLabels)
import Data.Vector.Vector2         (Vector2(..))
import Control.Applicative         (pure)
import Graphics.DrawingCombinators (R)

type Scalar = R

type Coordinate = Vector2 Scalar
type Size = Coordinate
type MaxSize = Vector2 (Maybe Scalar)

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

horizontallyExpanding :: Scalar -> Scalar -> SizeRange
horizontallyExpanding fixedHeight minWidth =
  SizeRange (Vector2 minWidth fixedHeight)
            (Vector2 Nothing (Just fixedHeight))

verticallyExpanding :: Scalar -> Scalar -> SizeRange
verticallyExpanding fixedWidth minHeight =
  SizeRange (Vector2 fixedWidth minHeight)
            (Vector2 (Just fixedWidth) Nothing)

expanding :: Scalar -> Scalar -> SizeRange
expanding minWidth minHeight =
  SizeRange (Vector2 minWidth minHeight)
            (pure Nothing)
