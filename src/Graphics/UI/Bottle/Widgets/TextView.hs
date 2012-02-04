{-# OPTIONS -Wall #-}
{-# LANGUAGE TemplateHaskell #-}
module Graphics.UI.Bottle.Widgets.TextView (
  Style(..), atStyleColor, atStyleFont, atStyleFontSize,
  make, makeWidget,
  drawTextAsSingleLetters, drawTextAsLines) where

import Control.Applicative (liftA2)
import Control.Arrow (first, second, (&&&))
import Data.List.Split (splitWhen)
import Data.List.Utils (enumerate)
import Data.Monoid (Monoid(..))
import Data.Vector.Vector2 (Vector2(..))
import Graphics.DrawingCombinators((%%))
import Graphics.DrawingCombinators.Utils(square)
import Graphics.UI.Bottle.SizeRange (Size, fixedSize)
import Graphics.UI.Bottle.Sized (Sized(..))
import Graphics.UI.Bottle.Widget (Widget)
import Graphics.UI.Bottle.Animation(AnimId)
import qualified Data.AtFieldTH as AtFieldTH
import qualified Data.ByteString.Char8 as SBS8
import qualified Data.Vector.Vector2 as Vector2
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.DrawingCombinators.Utils as DrawUtils
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.Widget as Widget

data Style = Style {
  styleColor :: Draw.Color,
  styleFont :: Draw.Font,
  styleFontSize :: Int
  }

AtFieldTH.make ''Style

augment :: Show a => AnimId -> a -> AnimId
augment animId = Anim.joinId animId . (:[]) . SBS8.pack . show

heightSize :: Size
heightSize = Vector2 0 DrawUtils.textHeight

fontRender :: Style -> String -> (Draw.Image (), Size)
fontRender (Style color font ptSize) =
  ((Draw.scale sz sz %%) . Draw.tint color . DrawUtils.drawText font) &&&
  (fmap (sz *) . DrawUtils.textSize font)
  where
    sz = fromIntegral ptSize

drawMany ::
  (Size -> Size) ->
  [(AnimId -> Anim.Frame, Size)] ->
  (AnimId -> Anim.Frame, Size)
drawMany sizeToTranslate =
  (second . liftA2 max) heightSize . foldr step (mempty, 0)
  where
    step (drawX, sizeX) (drawXs, sizeXs) =
      (mappend drawX $ fmap (Anim.translate trans) drawXs,
       liftA2 max sizeX $ trans + sizeXs)
      where
        trans = sizeToTranslate sizeX

drawTextHelper ::
  [(AnimId -> Anim.Frame, Size)] ->
  (AnimId -> Anim.Frame, Size)
drawTextHelper =
  drawMany vertical . map lineMarker . enumerate
  where
    vertical = Vector2.first (const 0)
    lineMarker (lineIndex, (mkFrame, size)) =
      (newMkFrame, size)
      where
        newMkFrame animId =
          mappend (mkFrame animId) . Anim.scale heightSize $
          Anim.simpleFrame (augment animId ["line marker", show lineIndex]) square

nestedFrame ::
  Show a => (a, (Draw.Image (), Size)) -> (AnimId -> Anim.Frame, Size)
nestedFrame (i, (image, size)) =
  (draw, size)
  where
    draw animId =
      Anim.simpleFrameDownscale (augment animId i) size image

drawTextAsSingleLetters ::
  Style -> String -> (AnimId -> Anim.Frame, Size)
drawTextAsSingleLetters style text =
  drawTextHelper $
  map
  (drawMany horizontal .
   map (nestedFrame . second (fontRender style . (:[])))) .
  splitWhen ((== '\n') . snd) $ enumerate text
  where
    horizontal = Vector2.second (const 0)

drawTextAsLines :: Style -> String -> (AnimId -> Anim.Frame, Size)
drawTextAsLines style text =
  drawTextHelper $
  map (nestedFrame . second (fontRender style) . first ((,) "Line")) .
  enumerate $ splitWhen (== '\n') text



make :: Style -> String -> AnimId -> Sized Anim.Frame
make style text animId = Sized (fixedSize textSize) . const $ frame animId
  where
    (frame, textSize) = drawTextAsLines style text

makeWidget :: Style -> String -> AnimId -> Widget a
makeWidget style text = Widget.liftView . make style text
