{-# OPTIONS -Wall #-}
module Graphics.DrawingCombinators.Utils (
  Image, square,
  textHeight, textWidth,
  linesWidth, linesHeight,
  drawLines) where

import Control.Monad(void)
import Data.Monoid(Monoid(..))
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

linesHeight :: [String] -> Draw.R
linesHeight = (textHeight *) . fromIntegral . length

linesWidth :: Draw.Font -> [String] -> Draw.R
linesWidth font = maximum . map (textWidth font)

drawLines :: Draw.Font -> [String] -> Image
drawLines font =
  (Draw.translate (0, 1.5) %%) .
  (Draw.scale 1 (-1) %%) .
  void . foldr (step . Draw.text font . UTF8.encodeString) mempty
  where
    step lineImage restImage =
      mconcat [
        lineImage,
        Draw.translate (0, -textHeight) %% restImage
      ]
