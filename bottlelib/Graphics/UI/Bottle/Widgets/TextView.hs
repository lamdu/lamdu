{-# LANGUAGE TemplateHaskell #-}
module Graphics.UI.Bottle.Widgets.TextView
  ( Style(..), styleColor, styleFont, styleFontSize
  , make, makeWidget
  , label
  , drawTextAsSingleLetters, drawTextAsLines
  , letterRects
  ) where

import Control.Applicative (liftA2)
import Control.Arrow ((&&&))
import Control.Lens.Operators
import Data.List (foldl')
import Data.List.Split (splitWhen)
import Data.List.Utils (enumerate)
import Data.Monoid (Monoid(..))
import Data.Vector.Vector2 (Vector2(..))
import Graphics.DrawingCombinators((%%))
import Graphics.UI.Bottle.Animation(AnimId, Size)
import Graphics.UI.Bottle.Rect (Rect(Rect))
import Graphics.UI.Bottle.View (View)
import Graphics.UI.Bottle.Widget (Widget)
import qualified Control.Lens as Lens
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.DrawingCombinators.Utils as DrawUtils
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.Rect as Rect
import qualified Graphics.UI.Bottle.View as View
import qualified Graphics.UI.Bottle.Widget as Widget

data Style = Style {
  _styleColor :: Draw.Color,
  _styleFont :: Draw.Font,
  _styleFontSize :: Int
  }
Lens.makeLenses ''Style

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
    vertical = Lens._1 .~ 0

nestedFrame ::
  Show a => (a, (Draw.Image (), Size)) -> (AnimId -> Anim.Frame, Size)
nestedFrame (i, (image, size)) =
  (draw, size)
  where
    draw animId =
      Anim.simpleFrameDownscale (View.augmentAnimId animId i) size image

drawTextAsSingleLetters ::
  Style -> String -> (AnimId -> Anim.Frame, Size)
drawTextAsSingleLetters style text =
  joinLines $
  map
  ((Lens._2 %~ max minLineSize) . drawMany horizontal .
   map (nestedFrame . (Lens._2 %~ renderLetter))) .
  splitWhen ((== '\n') . snd) $ enumerate text
  where
    (_, minLineSize) = fontRender style ""
    horizontal = Lens._2 .~ 0
    renderLetter = fontRender style . (:[])

letterRects :: Style -> String -> [[Rect]]
letterRects style text =
  zipWith locateLineHeight (iterate (+ lineHeight) 0) textLines
  where
    textLines = map makeLine $ splitWhen (== '\n') text
    locateLineHeight y = Lens.mapped . Rect.top +~ y
    (_, Vector2 _ lineHeight) = fontRender style ""
    makeLine textLine =
      zipWith makeLetterRect sizes . scanl (+) 0 .
      map (^. Lens._1) $ sizes
      where
        sizes = map toSize textLine
        toSize = snd . fontRender style . (:[])
        makeLetterRect size xpos = Rect (Vector2 xpos 0) size

drawTextAsLines :: Style -> String -> (AnimId -> Anim.Frame, Size)
drawTextAsLines style text =
  joinLines $
  map (nestedFrame . (Lens._2 %~ fontRender style) . (Lens._1 %~ (,) "Line")) .
  enumerate $ splitWhen (== '\n') text

make :: Style -> String -> AnimId -> View
make style text animId = (textSize, frame animId)
  where
    (frame, textSize) = drawTextAsLines style text

makeWidget :: Style -> String -> AnimId -> Widget a
makeWidget style text = uncurry Widget.liftView . make style text

label :: Style -> AnimId -> String -> View
label style animId text = make style text $ View.augmentAnimId animId text
