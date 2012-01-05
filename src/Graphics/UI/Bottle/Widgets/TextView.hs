{-# OPTIONS -Wall #-}
module Graphics.UI.Bottle.Widgets.TextView (Style(..), make, makeWidget, drawText) where

import Data.Vector.Vector2(Vector2(..))
import Graphics.DrawingCombinators.Utils (textLinesSize, drawTextLines)
import Graphics.UI.Bottle.Animation (AnimId, Frame, simpleFrameDownscale, scale)
import Graphics.UI.Bottle.SizeRange (fixedSize)
import Graphics.UI.Bottle.Sized (Sized(..))
import Graphics.UI.Bottle.Widget (Widget, liftView)
import qualified Graphics.DrawingCombinators as Draw

data Style = Style {
  styleFont :: Draw.Font,
  styleFontSize :: Int
  }

drawText :: Style -> [String] -> (AnimId -> Frame, Vector2 Draw.R)
drawText (Style font ptSize) textLines =
  (draw, sz * textSize)
  where
    draw animId =
      scale sz . simpleFrameDownscale animId textSize $
      drawTextLines font textLines
    sz = fromIntegral ptSize
    textSize = textLinesSize font textLines

make :: Style -> [String] -> AnimId -> Sized Frame
make style textLines animId = Sized (fixedSize textSize) . const $ frame animId
  where
    (frame, textSize) = drawText style textLines

makeWidget :: Style -> [String] -> AnimId -> Widget a
makeWidget style textLines = liftView . make style textLines
