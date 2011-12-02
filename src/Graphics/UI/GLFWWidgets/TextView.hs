{-# OPTIONS -Wall #-}
module Graphics.UI.GLFWWidgets.TextView (make, makeWidget) where

import Graphics.DrawingCombinators.Utils (Image, textLinesSize, drawTextLines)
import Graphics.UI.GLFWWidgets.SizeRange (fixedSize)
import Graphics.UI.GLFWWidgets.Sized (Sized(..))
import Graphics.UI.GLFWWidgets.Widget (Widget, liftView)
import qualified Graphics.DrawingCombinators as Draw

make :: Draw.Font -> Int -> [String] -> Sized Image
make font ptSize textLines =
  Sized (fixedSize (textLinesSize font ptSize textLines)) $
  const (drawTextLines font ptSize textLines)

makeWidget :: Draw.Font -> Int -> [String] -> Widget a
makeWidget font ptSize = liftView . make font ptSize
