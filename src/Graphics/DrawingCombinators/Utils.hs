{-# OPTIONS -Wall #-}
module Graphics.DrawingCombinators.Utils (
  Image, square,
  textHeight, textWidth, textSize,
  textLinesWidth, textLinesHeight, textLinesSize,
  drawTextLines) where

import Control.Monad(void)
import Data.Monoid(Monoid(..))
import Data.Vector.Vector2(Vector2(..))
import Graphics.DrawingCombinators((%%))
import qualified Codec.Binary.UTF8.String as UTF8
import qualified Graphics.DrawingCombinators as Draw

type Image = Draw.Image ()

square :: Image
square = void $ Draw.convexPoly [ (0, 0), (1, 0), (1, 1), (0, 1) ]

textHeight :: Draw.R
textHeight = 2

textWidth :: Draw.Font -> String -> Draw.R
textWidth font = Draw.textWidth font . UTF8.encodeString

textSize :: Draw.Font -> String -> Vector2 Draw.R
textSize font str = Vector2 (textWidth font str) textHeight

textLinesHeight :: [String] -> Draw.R
textLinesHeight = (textHeight *) . fromIntegral . length

textLinesWidth :: Draw.Font -> [String] -> Draw.R
textLinesWidth font = maximum . map (textWidth font)

textLinesSize :: Draw.Font -> [String] -> Vector2 Draw.R
textLinesSize font textLines = Vector2 (textLinesWidth font textLines) (textLinesHeight textLines)

drawTextLines :: Draw.Font -> [String] -> Image
drawTextLines font =
  (Draw.translate (0, 1.5) %%) .
  (Draw.scale 1 (-1) %%) .
  void . foldr (step . Draw.text font . UTF8.encodeString) mempty
  where
    step lineImage restImage =
      mconcat [
        lineImage,
        Draw.translate (0, -textHeight) %% restImage
      ]
