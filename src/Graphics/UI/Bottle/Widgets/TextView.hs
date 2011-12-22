{-# OPTIONS -Wall #-}
module Graphics.UI.Bottle.Widgets.TextView (Style(..), make, makeWidget) where

import Graphics.DrawingCombinators.Utils (Image, textLinesSize, drawTextLines)
import Graphics.UI.Bottle.SizeRange (fixedSize)
import Graphics.UI.Bottle.Sized (Sized(..))
import Graphics.UI.Bottle.Widget (Widget, liftView)
import qualified Graphics.DrawingCombinators as Draw

data Style = Style {
  styleFont :: Draw.Font,
  styleFontSize :: Int
  }

make :: Style -> [String] -> Sized Image
make (Style font ptSize) textLines =
  Sized (fixedSize (textLinesSize font ptSize textLines)) $
  const (drawTextLines font ptSize textLines)

makeWidget :: Style -> [String] -> Widget a
makeWidget style = liftView . make style
