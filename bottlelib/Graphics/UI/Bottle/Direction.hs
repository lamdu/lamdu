{-# LANGUAGE DeriveFunctor, FlexibleInstances, MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}
module Graphics.UI.Bottle.Direction
  ( Direction(..), inRelativePos
  , fromLeft, fromTop, fromRight, fromBottom
  ) where

import Control.Lens ((^.))
import Graphics.UI.Bottle.Rect (Rect(..))
import qualified Control.Lens as Lens
import qualified Graphics.UI.Bottle.Rect as Rect

-- RelativePos pos is relative to the top-left of the widget
data Direction = Outside | RelativePos Rect

inRelativePos :: (Rect -> Rect) -> Direction -> Direction
inRelativePos _ Outside = Outside
inRelativePos f (RelativePos x) = RelativePos (f x)

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
  Lens.over Rect.left (+ rect ^. Rect.width) rect

fromBottom :: Rect -> Direction
fromBottom rect =
  horizontalRelativePos $
  Lens.over Rect.top (+ rect ^. Rect.height) rect
