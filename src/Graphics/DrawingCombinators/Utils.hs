{-# OPTIONS -Wall #-}
module Graphics.DrawingCombinators.Utils (
  Image, square,
  textHeight, textWidth, textSize,
  textLinesWidth, textLinesHeight, textLinesSize,
  drawTextLines, backgroundColor) where

import Control.Monad(void)
import Data.List(genericLength)
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

drawText :: Draw.Font -> String -> Image
drawText font =
  -- We want to reverse it so that higher y is down, and it is also
  -- moved to 0..2
  (Draw.scale 1 (-1) %%) .
  -- Text is normally at height -1.5..0.5.  We move it to be -2..0
  (Draw.translate (0, -1.5) %%) .
  void . Draw.text font . UTF8.encodeString

textLinesHeight :: [String] -> Draw.R
textLinesHeight = (textHeight *) . genericLength

textLinesWidth :: Draw.Font -> [String] -> Draw.R
textLinesWidth font = maximum . map (textWidth font)

textLinesSize :: Draw.Font -> [String] -> Vector2 Draw.R
textLinesSize font textLines = Vector2 (textLinesWidth font textLines) (textLinesHeight textLines)

drawTextLines :: Draw.Font -> [String] -> Image
drawTextLines font =
  foldr (step . drawText font) mempty
  where
    step lineImage restImage =
      mappend lineImage $
      Draw.translate (0, textHeight) %% restImage

backgroundColor :: Draw.Color -> Vector2 Draw.R -> Image -> Image
backgroundColor color (Vector2 width height) image =
  mappend image $
  Draw.tint color $ Draw.scale width height %% square
