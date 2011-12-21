{-# OPTIONS -Wall #-}
{-# LANGUAGE TypeOperators, GeneralizedNewtypeDeriving #-}
module Graphics.UI.Bottle.Widgets.Spacer (
  make, makeWidget, indentRight, indentRightWidget, makeHorizontal,
  makeHorizontalExpanding, makeVerticalExpanding) where

import Data.Monoid(mempty)
import Data.Vector.Vector2(Vector2(..))
import qualified Graphics.DrawingCombinators as Draw
import Graphics.DrawingCombinators.Utils (Image)
import Graphics.UI.Bottle.SizeRange (fixedSize, Size)
import qualified Graphics.UI.Bottle.SizeRange as SizeRange
import Graphics.UI.Bottle.Sized (Sized(..))
import Graphics.UI.Bottle.Widget (Widget, liftView)
import qualified Graphics.UI.Bottle.Widgets.GridView as GridView

make :: Size -> Sized Image
make size = Sized (fixedSize size) mempty

makeWidget :: Size -> Widget a
makeWidget = liftView . make

makeHorizontal :: Draw.R -> Sized Image
makeHorizontal width = make (Vector2 width 0)

makeVerticalExpanding :: Sized Image
makeVerticalExpanding = Sized (SizeRange.verticallyExpanding 0 0) mempty

makeHorizontalExpanding :: Sized Image
makeHorizontalExpanding = Sized (SizeRange.horizontallyExpanding 0 0) mempty

indentRight :: Draw.R -> Sized Image -> Sized Image
indentRight width img = GridView.make [[makeHorizontal width, img]]

indentRightWidget :: Draw.R -> Widget a -> Widget a
indentRightWidget width widget =
  GridView.makeFromWidgets [[liftView (makeHorizontal width), widget]]
