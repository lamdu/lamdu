{-# OPTIONS -Wall #-}
{-# LANGUAGE TemplateHaskell #-}
module Graphics.UI.Bottle.Widgets.TextView (
  Style(..), atStyleColor, atStyleFont, atStyleFontSize,
  make, makeWidget,
  drawTextAsSingleLetters, drawTextAsLines,
  letterRects) where

import Control.Applicative (liftA2)
import Control.Arrow (first, second, (&&&))
import Data.List (foldl')
import Data.List.Split (splitWhen)
import Data.List.Utils (enumerate)
import Data.Monoid (Monoid(..))
import Data.Vector.Vector2 (Vector2(..))
import Graphics.DrawingCombinators((%%))
import Graphics.UI.Bottle.Animation(AnimId)
import Graphics.UI.Bottle.SizeRange (Size, fixedSize)
import Graphics.UI.Bottle.Sized (Sized(..))
import Graphics.UI.Bottle.Widget (Widget)
import qualified Data.AtFieldTH as AtFieldTH
import qualified Data.ByteString.Char8 as SBS8
import qualified Data.Vector.Vector2 as Vector2
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.DrawingCombinators.Utils as DrawUtils
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.Widget as Widget
import Graphics.UI.Bottle.Rect as Rect

data Style = Style {
  styleColor :: Draw.Color,
  styleFont :: Draw.Font,
  styleFontSize :: Int
  }

AtFieldTH.make ''Style

augment :: Show a => AnimId -> a -> AnimId
augment animId = Anim.joinId animId . (:[]) . SBS8.pack . show

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
  foldl' step (mempty, 0)
  where
    step (drawAcc, sizeAcc) (drawX, sizeX) =
      (mappend drawAcc $ Anim.translate trans . drawX,
       liftA2 max sizeAcc $ trans + sizeX)
      where
        trans = sizeToTranslate sizeAcc

joinLines ::
  [(AnimId -> Anim.Frame, Size)] ->
  (AnimId -> Anim.Frame, Size)
joinLines =
  drawMany vertical
  where
    vertical = Vector2.first (const 0)

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
  joinLines $
  map
  (second (max minLineSize) . drawMany horizontal .
   map (nestedFrame . second (fontRender style . (:[])))) .
  splitWhen ((== '\n') . snd) $ enumerate text
  where
    (_, minLineSize) = fontRender style ""
    horizontal = Vector2.second (const 0)

letterRects :: Style -> String -> [[Rect]]
letterRects style text =
  zipWith locateLineHeight (iterate (+ lineHeight) 0) textLines
  where
    textLines = map makeLine $ splitWhen (== '\n') text
    locateLineHeight y = (map . Rect.atTop) (+y)
    (_, Vector2 _ lineHeight) = fontRender style ""
    makeLine textLine =
      zipWith makeLetterRect sizes . scanl (+) 0 . map Vector2.fst $ sizes
      where
        sizes = map toSize textLine
        toSize = snd . fontRender style . (:[])
        makeLetterRect size xpos = Rect (Vector2 xpos 0) size

drawTextAsLines :: Style -> String -> (AnimId -> Anim.Frame, Size)
drawTextAsLines style text =
  joinLines $
  map (nestedFrame . second (fontRender style) . first ((,) "Line")) .
  enumerate $ splitWhen (== '\n') text

make :: Style -> String -> AnimId -> Sized Anim.Frame
make style text animId = Sized (fixedSize textSize) . const $ frame animId
  where
    (frame, textSize) = drawTextAsLines style text

makeWidget :: Style -> String -> AnimId -> Widget a
makeWidget style text = Widget.liftView . make style text
