{-# OPTIONS -Wall #-}
module Graphics.UI.Bottle.Widgets.TextView (Style(..), make, makeWidget, drawText) where

import Control.Applicative (liftA2)
import Control.Arrow (first, second, (&&&))
import Data.List.Utils(enumerate2d)
import Data.Monoid (Monoid(..))
import Data.Vector.Vector2(Vector2(..))
import Graphics.UI.Bottle.SizeRange (fixedSize)
import Graphics.UI.Bottle.Sized (Sized(..))
import Graphics.UI.Bottle.Widget (Widget, liftView)
import qualified Data.ByteString.Char8 as SBS8
import qualified Data.Vector.Vector2 as Vector2
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.DrawingCombinators.Utils as DrawUtils
import qualified Graphics.UI.Bottle.Animation as Anim

data Style = Style {
  styleFont :: Draw.Font,
  styleFontSize :: Int
  }

augment :: Show a => a -> Anim.AnimId -> Anim.AnimId
augment x = (SBS8.pack (show x) :)

drawText :: Style -> [String] -> (Anim.AnimId -> Anim.Frame, Vector2 Draw.R)
drawText (Style font ptSize) textLines =
  (first . fmap) (Anim.scale sz) .
  second (* sz) .
  drawMany vertical .
  map (drawMany horizontal) .
  (map . map) (nestedFrame . second useFont) .
  enumerate2d $
  textLines
  where
    useFont = (DrawUtils.drawText font &&& DrawUtils.textSize font) . (: [])
    nestedFrame (i, (image, size)) = (draw, size)
      where
        draw animId = Anim.simpleFrameDownscale (augment (i :: (Int, Int)) animId) size image

    resize = liftA2 max $ Vector2 0 DrawUtils.textHeight
    drawMany sizeToTranslate = second resize . foldr step (mempty, 0)
      where
        step (drawX, sizeX) (drawXs, sizeXs) =
          (mappend drawX $ fmap (Anim.translate trans) drawXs,
           liftA2 max sizeX $ trans + sizeXs)
          where
            trans = sizeToTranslate sizeX

    horizontal = Vector2.second (const 0)
    vertical = Vector2.first (const 0)
    sz = fromIntegral ptSize

make :: Style -> [String] -> Anim.AnimId -> Sized Anim.Frame
make style textLines animId = Sized (fixedSize textSize) . const $ frame animId
  where
    (frame, textSize) = drawText style textLines

makeWidget :: Style -> [String] -> Anim.AnimId -> Widget a
makeWidget style textLines = liftView . make style textLines
