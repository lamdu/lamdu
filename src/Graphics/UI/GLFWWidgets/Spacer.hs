{-# OPTIONS -Wall #-}
{-# LANGUAGE TypeOperators, GeneralizedNewtypeDeriving #-}
module Graphics.UI.GLFWWidgets.Spacer (make, makeWidget) where

import Data.Monoid(mempty)
import Graphics.DrawingCombinators.Utils (Image)
import Graphics.UI.GLFWWidgets.SizeRange (fixedSize, Size)
import Graphics.UI.GLFWWidgets.Sized (Sized(..))
import Graphics.UI.GLFWWidgets.Widget (Widget, liftView)

make :: Size -> Sized Image
make size = Sized (fixedSize size) mempty

makeWidget :: Size -> Widget a
makeWidget = liftView . make
