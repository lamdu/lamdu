{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.UI.Bottle.Widgets.Spacer
  ( make
  , makeWidget
  , makeHorizontal, makeVertical
  , makeHorizontalWidget
  , makeHorizLine
  , empty
  ) where

import Control.Monad (void)
import Data.Monoid (mempty)
import Data.Vector.Vector2 (Vector2(..))
import Graphics.UI.Bottle.View (View)
import Graphics.UI.Bottle.Widget (Widget)
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.Widget as Widget

widget :: View -> Widget f
widget = uncurry Widget.liftView

make :: Anim.Size -> View
make size = (size, mempty)

makeWidget :: Widget.Size -> Widget f
makeWidget = widget . make

makeHorizontal :: Anim.R -> View
makeHorizontal width = make $ Vector2 width 0

makeVertical :: Anim.R -> View
makeVertical height = make $ Vector2 0 height

makeHorizontalWidget :: Widget.R -> Widget f
makeHorizontalWidget = widget . makeHorizontal

horizLineFrame :: Anim.AnimId -> Widget.Size -> Anim.Frame
horizLineFrame animId size@(Vector2 w h) =
  Anim.simpleFrameDownscale animId size . void $ Draw.line (0, h/2) (w, h/2)

makeHorizLine :: Anim.AnimId -> Widget.Size -> Widget f
makeHorizLine animId size = Widget.liftView size $ horizLineFrame animId size

empty :: Widget f
empty = makeWidget 0
