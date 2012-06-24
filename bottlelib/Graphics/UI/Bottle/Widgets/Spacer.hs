{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.UI.Bottle.Widgets.Spacer
  ( make
  , makeWidget
  , indentRight, indentRightWidget
  , makeHorizontal
  , makeHorizontalExpanding
  , makeVerticalExpanding
  , makeHorizLine
  , makeHorizLineWidget
) where

import Control.Monad (void)
import Data.Monoid (mempty)
import Data.Vector.Vector2 (Vector2(..))
import Graphics.UI.Bottle.SizeRange (fixedSize, Size)
import Graphics.UI.Bottle.Sized (Sized, mkSized)
import Graphics.UI.Bottle.Widget (Widget)
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.SizeRange as SizeRange
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Grid as Grid
import qualified Graphics.UI.Bottle.Widgets.GridView as GridView

make :: Size -> Sized Anim.Frame
make size = mkSized (fixedSize size) mempty

makeWidget :: Size -> Widget a
makeWidget = Widget.liftView . make

makeHorizontal :: Widget.R -> Sized Anim.Frame
makeHorizontal width = make (Vector2 width 0)

makeVerticalExpanding :: Sized Anim.Frame
makeVerticalExpanding = mkSized (SizeRange.verticallyExpanding 0 0) mempty

makeHorizontalExpanding :: Sized Anim.Frame
makeHorizontalExpanding = mkSized (SizeRange.horizontallyExpanding 0 0) mempty

indentRight :: Widget.R -> Sized Anim.Frame -> Sized Anim.Frame
indentRight width img = GridView.make [[makeHorizontal width, img]]

indentRightWidget :: Widget.R -> Widget a -> Widget a
indentRightWidget width widget =
  Grid.toWidget $
  Grid.make [[Widget.liftView (makeHorizontal width), widget]]

horizLineFrame :: Anim.AnimId -> Vector2 Widget.R -> Anim.Frame
horizLineFrame animId size@(Vector2 w h) =
  Anim.simpleFrameDownscale animId size . void $ Draw.line (0, h/2) (w, h/2)

-- Fills only the given space (if align is used on this, it will be an empty widget)
makeHorizLine :: Anim.AnimId -> Sized Anim.Frame
-- TODO: 1 1 is ugly, but 0 0 is not compatible with animations
-- apparently.. Look into this
makeHorizLine animId = mkSized (fixedSize (Vector2 0 1)) (horizLineFrame animId)

makeHorizLineWidget :: Anim.AnimId -> Widget a
makeHorizLineWidget = Widget.liftView . makeHorizLine
