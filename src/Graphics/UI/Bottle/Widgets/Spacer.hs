{-# OPTIONS -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.UI.Bottle.Widgets.Spacer (
  make, makeWidget, indentRight, indentRightWidget, makeHorizontal,
  makeHorizontalExpanding, makeVerticalExpanding) where

import Data.Monoid(mempty)
import Data.Vector.Vector2(Vector2(..))
import Graphics.UI.Bottle.SizeRange (fixedSize, Size)
import Graphics.UI.Bottle.Sized (Sized(..))
import Graphics.UI.Bottle.Widget (Widget, liftView)
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.SizeRange as SizeRange
import qualified Graphics.UI.Bottle.Widgets.GridView as GridView

make :: Size -> Sized Anim.Frame
make size = Sized (fixedSize size) mempty

makeWidget :: Size -> Widget a
makeWidget = liftView . make

makeHorizontal :: Draw.R -> Sized Anim.Frame
makeHorizontal width = make (Vector2 width 0)

makeVerticalExpanding :: Sized Anim.Frame
makeVerticalExpanding = Sized (SizeRange.verticallyExpanding 0 0) mempty

makeHorizontalExpanding :: Sized Anim.Frame
makeHorizontalExpanding = Sized (SizeRange.horizontallyExpanding 0 0) mempty

indentRight :: Draw.R -> Sized Anim.Frame -> Sized Anim.Frame
indentRight width img = GridView.make [[makeHorizontal width, img]]

indentRightWidget :: Draw.R -> Widget a -> Widget a
indentRightWidget width widget =
  GridView.makeFromWidgets [[liftView (makeHorizontal width), widget]]
