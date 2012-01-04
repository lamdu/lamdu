{-# OPTIONS -Wall #-}
module Graphics.UI.Bottle.Widgets.TextView (Style(..), make, makeWidget) where

import Control.Applicative (pure)
import Graphics.DrawingCombinators.Utils (Image, textLinesSize, drawTextLines)
import Graphics.DrawingCombinators ((%%))
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
  Sized (fixedSize (pure sz * textLinesSize font textLines)) $
  const (Draw.scale sz sz %% drawTextLines font textLines)
  where
    sz = fromIntegral ptSize

makeWidget :: Style -> [String] -> Widget a
makeWidget style = liftView . make style
