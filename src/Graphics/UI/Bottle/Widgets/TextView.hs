{-# OPTIONS -Wall #-}
module Graphics.UI.Bottle.Widgets.TextView (make, makeWidget) where

import Graphics.DrawingCombinators.Utils (Image, textLinesSize, drawTextLines)
import Graphics.UI.Bottle.SizeRange (fixedSize)
import Graphics.UI.Bottle.Sized (Sized(..))
import Graphics.UI.Bottle.Widget (Widget, liftView)
import qualified Graphics.DrawingCombinators as Draw

make :: Draw.Font -> Int -> [String] -> Sized Image
make font ptSize textLines =
  Sized (fixedSize (textLinesSize font ptSize textLines)) $
  const (drawTextLines font ptSize textLines)

makeWidget :: Draw.Font -> Int -> [String] -> Widget a
makeWidget font ptSize = liftView . make font ptSize
