{-# OPTIONS -Wall #-}
{-# LANGUAGE TypeOperators, GeneralizedNewtypeDeriving #-}
module Graphics.UI.Bottle.Widgets.Spacer (make, makeWidget) where

import Data.Monoid(mempty)
import Graphics.DrawingCombinators.Utils (Image)
import Graphics.UI.Bottle.SizeRange (fixedSize, Size)
import Graphics.UI.Bottle.Sized (Sized(..))
import Graphics.UI.Bottle.Widget (Widget, liftView)

make :: Size -> Sized Image
make size = Sized (fixedSize size) mempty

makeWidget :: Size -> Widget a
makeWidget = liftView . make
