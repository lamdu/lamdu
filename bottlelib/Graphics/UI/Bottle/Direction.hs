{-# LANGUAGE DeriveFunctor, FlexibleInstances, MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}
module Graphics.UI.Bottle.Direction
  ( Direction(..), atCoordinates
  ) where

import Data.Vector.Vector2 (Vector2(..))
import Graphics.UI.Bottle.Rect (R, Rect(..))
import qualified Control.Lens as Lens
import qualified Graphics.UI.Bottle.Rect as Rect

-- RelativePos pos is relative to the top-left of the widget
data Direction = Outside | PrevFocalArea Rect | Point (Vector2 R)

atCoordinates :: (Rect -> Rect) -> Direction -> Direction
atCoordinates _ Outside = Outside
atCoordinates f (PrevFocalArea x) = PrevFocalArea (f x)
atCoordinates f (Point x) = Point . Lens.view Rect.topLeft . f $ Rect x 0
