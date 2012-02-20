{-# OPTIONS -Wall #-}
{-# LANGUAGE DeriveFunctor, FlexibleInstances, MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}
module Graphics.UI.Bottle.Direction(
  Direction(..), fold, inRelativePos,
  fromLeft, fromTop, fromRight, fromBottom)
where

import Graphics.UI.Bottle.Rect (Rect(..))
import qualified Graphics.UI.Bottle.Rect as Rect

-- RelativePos pos is relative to the top-left of the widget
data Direction = Outside | RelativePos Rect

fold :: r -> (Rect -> r) -> Direction -> r
fold outside _ Outside = outside
fold _ relativePos (RelativePos v) = relativePos v

inRelativePos :: (Rect -> Rect) -> Direction -> Direction
inRelativePos f = fold Outside (RelativePos . f)

verticalRelativePos :: Rect -> Direction
verticalRelativePos = RelativePos . (Rect.atWidth . const) 0

horizontalRelativePos :: Rect -> Direction
horizontalRelativePos = RelativePos . (Rect.atHeight . const) 0

fromLeft :: Rect -> Direction
fromLeft = verticalRelativePos

fromTop :: Rect -> Direction
fromTop = horizontalRelativePos

fromRight :: Rect -> Direction
fromRight rect = verticalRelativePos $ Rect.atLeft (+ Rect.width rect) rect

fromBottom :: Rect -> Direction
fromBottom rect = horizontalRelativePos $ Rect.atTop (+ Rect.height rect) rect
