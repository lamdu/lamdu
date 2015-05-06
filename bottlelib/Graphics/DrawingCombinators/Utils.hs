{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE CPP, StandaloneDeriving, DeriveGeneric #-}
module Graphics.DrawingCombinators.Utils
  ( Image
  , square
  , textHeight, textSize
  , textLinesWidth, textLinesHeight, textLinesSize
  , drawText, drawTextLines
  , backgroundColor
  , scale, translate
  , clearRenderSized
  ) where

import           Control.Lens.Operators
import           Control.Monad (void)
import           Data.Aeson (ToJSON(..), FromJSON(..))
import           Data.List (genericLength)
import           Data.Monoid (Monoid(..))
import           Data.Vector.Vector2 (Vector2(..))
import           Foreign.C.Types.Instances ()
import           GHC.Generics (Generic)
import qualified Graphics.DrawingCombinators as Draw
import           Graphics.DrawingCombinators ((%%))

type Image = Draw.Image ()

deriving instance Read Draw.Color
deriving instance Generic Draw.Color

instance ToJSON Draw.Color
instance FromJSON Draw.Color

clearRenderSized :: Draw.R2 -> Draw.Image a -> IO ()
#ifdef DRAWINGCOMBINATORS__SIZED
clearRenderSized = Draw.clearRenderSized
#else
clearRenderSized _ = Draw.clearRender
#endif

scale :: Vector2 Draw.R -> Draw.Affine
scale (Vector2 x y) = Draw.scale x y

translate :: Vector2 Draw.R -> Draw.Affine
translate (Vector2 x y) = Draw.translate (x, y)

square :: Image
square = void $ Draw.convexPoly [ (0, 0), (1, 0), (1, 1), (0, 1) ]

textHeight :: Draw.R
textHeight = 2

textSize :: Draw.Font -> String -> Vector2 Draw.R
textSize font str = Vector2 (Draw.textWidth font str) textHeight

descender :: Draw.Font -> Draw.R
#ifdef DRAWINGCOMBINATORS__FONT_APIS
descender = Draw.fontDescender
#else
descender _ = -0.5 -- approximate
#endif

drawText :: Draw.Font -> String -> Image
drawText font str =
  str
  & Draw.text font
  & void
  -- Text is normally at height -0.5..1.5.  We move it to be -textHeight..0
  & (translate (Vector2 0 (-textHeight - descender font)) %%)
  -- We want to reverse it so that higher y is down, and it is also
  -- moved to 0..2
  & (scale (Vector2 1 (-1)) %%)

textLinesHeight :: [String] -> Draw.R
textLinesHeight = (textHeight *) . genericLength

textLinesWidth :: Draw.Font -> [String] -> Draw.R
textLinesWidth font = maximum . map (Draw.textWidth font)

textLinesSize :: Draw.Font -> [String] -> Vector2 Draw.R
textLinesSize font textLines = Vector2 (textLinesWidth font textLines) (textLinesHeight textLines)

drawTextLines :: Draw.Font -> [String] -> Image
drawTextLines font =
  foldr (step . drawText font) mempty
  where
    step lineImage restImage =
      mappend lineImage $
      translate (Vector2 0 textHeight) %% restImage

backgroundColor :: Draw.Color -> Vector2 Draw.R -> Image -> Image
backgroundColor color size image =
  mappend image $
  Draw.tint color $ scale size %% square
