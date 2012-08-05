{-# LANGUAGE DeriveFunctor, FlexibleInstances, MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}
module Graphics.UI.Bottle.Direction
  ( Direction(..), fold, inRelativePos
  , fromLeft, fromTop, fromRight, fromBottom
  ) where

import Control.Lens ((^.))
import Graphics.UI.Bottle.Rect (Rect(..))
import qualified Control.Lens as Lens
import qualified Graphics.UI.Bottle.Rect as Rect

-- RelativePos pos is relative to the top-left of the widget
data Direction = Outside | RelativePos Rect

fold :: r -> (Rect -> r) -> Direction -> r
fold outside _ Outside = outside
fold _ relativePos (RelativePos v) = relativePos v

inRelativePos :: (Rect -> Rect) -> Direction -> Direction
inRelativePos f = fold Outside (RelativePos . f)

verticalRelativePos :: Rect -> Direction
verticalRelativePos = RelativePos . Lens.set Rect.width 0

horizontalRelativePos :: Rect -> Direction
horizontalRelativePos = RelativePos . Lens.set Rect.height 0

fromLeft :: Rect -> Direction
fromLeft = verticalRelativePos

fromTop :: Rect -> Direction
fromTop = horizontalRelativePos

fromRight :: Rect -> Direction
fromRight rect =
  verticalRelativePos $
  Lens.adjust Rect.left (+ rect ^. Rect.width) rect

fromBottom :: Rect -> Direction
fromBottom rect =
  horizontalRelativePos $
  Lens.adjust Rect.top (+ rect ^. Rect.height) rect
