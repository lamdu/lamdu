{-# OPTIONS -Wall #-}
{-# LANGUAGE TemplateHaskell #-}
module Graphics.UI.Bottle.Widgets.TextView (
  Style(..), atStyleColor, atStyleFont, atStyleFontSize,
  make, makeWidget, drawText) where

import Control.Applicative (liftA2)
import Control.Arrow (first, second, (&&&))
import Data.List.Split (splitWhen)
import Data.List.Utils (enumerate)
import Data.Monoid (Monoid(..))
import Data.Vector.Vector2 (Vector2(..))
import Graphics.DrawingCombinators.Utils(square)
import Graphics.UI.Bottle.SizeRange (fixedSize)
import Graphics.UI.Bottle.Sized (Sized(..))
import Graphics.UI.Bottle.Widget (Widget, liftView)
import qualified Data.AtFieldTH as AtFieldTH
import qualified Data.ByteString.Char8 as SBS8
import qualified Data.Vector.Vector2 as Vector2
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.DrawingCombinators.Utils as DrawUtils
import qualified Graphics.UI.Bottle.Animation as Anim

data Style = Style {
  styleColor :: Draw.Color,
  styleFont :: Draw.Font,
  styleFontSize :: Int
  }

AtFieldTH.make ''Style

augment :: Show a => Anim.AnimId -> a -> Anim.AnimId
augment animId = Anim.joinId animId . (:[]) . SBS8.pack . show

drawText :: Bool -> Style -> String -> (Anim.AnimId -> Anim.Frame, Vector2 Draw.R)
drawText isSingleLetterImages (Style color font ptSize) text =
  (first . fmap) (Anim.scale sz) .
  second (* sz) .
  drawMany vertical $
  if isSingleLetterImages
  then
    map
      (lineMarker .
       second (drawMany horizontal . map (nestedFrame . second (useFont . (: []))))) .
    enumerate . splitWhen ((== '\n') . snd) $ enumerate text
  else
    map
      (lineMarker .
       second (nestedFrame . second useFont . first ((,) "Line"))) . enumerate . enumerate $
    splitWhen (== '\n') text
  where
    lineMarker (lineIndex, (mkFrame, size)) =
      (newMkFrame, size)
      where
        newMkFrame animId =
          mappend (mkFrame animId) . Anim.scale heightSize $
          Anim.simpleFrame (augment animId ["line marker", show lineIndex]) square
    useFont = (Draw.tint color . DrawUtils.drawText font) &&& DrawUtils.textSize font
    nestedFrame (i, (image, size)) = (draw, size)
      where
        draw animId = Anim.simpleFrameDownscale (augment animId i) size image

    heightSize = Vector2 0 DrawUtils.textHeight
    drawMany sizeToTranslate = (second . liftA2 max) heightSize . foldr step (mempty, 0)
      where
        step (drawX, sizeX) (drawXs, sizeXs) =
          (mappend drawX $ fmap (Anim.translate trans) drawXs,
           liftA2 max sizeX $ trans + sizeXs)
          where
            trans = sizeToTranslate sizeX

    horizontal = Vector2.second (const 0)
    vertical = Vector2.first (const 0)
    sz = fromIntegral ptSize

make :: Style -> String -> Anim.AnimId -> Sized Anim.Frame
make style text animId = Sized (fixedSize textSize) . const $ frame animId
  where
    (frame, textSize) = drawText False style text

makeWidget :: Style -> String -> Anim.AnimId -> Widget a
makeWidget style text = liftView . make style text
