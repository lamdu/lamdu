{-# OPTIONS -Wall #-}
module Graphics.UI.GLFWWidgets.TextView (make, makeWidget) where

import Graphics.DrawingCombinators.Utils (Image, textLinesSize, drawTextLines)
import Graphics.UI.GLFWWidgets.SizeRange (fixedSize)
import Graphics.UI.GLFWWidgets.Sized (Sized(..))
import Graphics.UI.GLFWWidgets.Widget (Widget, liftView)
import qualified Graphics.DrawingCombinators as Draw

make :: Draw.Font -> [String] -> Sized Image
make font textLines = Sized (fixedSize (textLinesSize font textLines)) $ const (drawTextLines font textLines)

makeWidget :: Draw.Font -> [String] -> Widget a
makeWidget font = liftView . make font
