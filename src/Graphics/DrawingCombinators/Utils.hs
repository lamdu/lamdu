{-# OPTIONS -Wall #-}
module Graphics.DrawingCombinators.Utils (
  Image, square,
  textHeight, textWidth, textSize,
  textLinesWidth, textLinesHeight, textLinesSize,
  drawTextLines, backgroundColor) where

import Control.Monad(void)
import Data.Monoid(Monoid(..))
import Data.Vector.Vector2(Vector2(..))
import Graphics.DrawingCombinators((%%))
import qualified Codec.Binary.UTF8.String as UTF8
import qualified Graphics.DrawingCombinators as Draw

type Image = Draw.Image ()

square :: Image
square = void $ Draw.convexPoly [ (0, 0), (1, 0), (1, 1), (0, 1) ]

textHeight :: Int -> Draw.R
textHeight ptSize = fromIntegral ptSize * 2

textWidth :: Draw.Font -> Int -> String -> Draw.R
textWidth font ptSize = (fromIntegral ptSize *) . Draw.textWidth font . UTF8.encodeString

textSize :: Draw.Font -> Int -> String -> Vector2 Draw.R
textSize font ptSize str = Vector2 (textWidth font ptSize str) (textHeight ptSize)

drawText :: Draw.Font -> Int -> String -> Image
drawText font ptSize =
  -- We want to reverse it so that higher y is down, and it is also
  -- moved to 0..2
  (Draw.scale (fromIntegral ptSize) (-fromIntegral ptSize) %%) .
  -- Text is normally at height -1.5..0.5.  We move it to be -2..0
  (Draw.translate (0, -1.5) %%) .
  void . Draw.text font . UTF8.encodeString

textLinesHeight :: Int -> [String] -> Draw.R
textLinesHeight ptSize = (textHeight ptSize *) . fromIntegral . length

textLinesWidth :: Draw.Font -> Int -> [String] -> Draw.R
textLinesWidth font ptSize = maximum . map (textWidth font ptSize)

textLinesSize :: Draw.Font -> Int -> [String] -> Vector2 Draw.R
textLinesSize font ptSize textLines = Vector2 (textLinesWidth font ptSize textLines) (textLinesHeight ptSize textLines)

drawTextLines :: Draw.Font -> Int -> [String] -> Image
drawTextLines font ptSize =
  foldr (step . drawText font ptSize) mempty
  where
    step lineImage restImage =
      mconcat [
        lineImage,
        Draw.translate (0, textHeight ptSize) %% restImage
      ]

backgroundColor :: Draw.Color -> Vector2 Draw.R -> Image -> Image
backgroundColor color (Vector2 width height) image =
  mconcat [
    image,
    Draw.tint color $ Draw.scale width height %% square
  ]
