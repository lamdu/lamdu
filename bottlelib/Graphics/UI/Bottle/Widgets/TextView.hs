{-# LANGUAGE TemplateHaskell #-}
module Graphics.UI.Bottle.Widgets.TextView
  ( Style(..), styleColor, styleFont, styleFontSize
  , make, makeWidget
  , label
  , drawTextAsSingleLetters, drawTextAsLines
  , letterRects
  ) where

import           Control.Applicative (liftA2)
import           Control.Arrow ((&&&))
import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Lens.Tuple
import           Data.List (foldl')
import           Data.List.Split (splitWhen)
import           Data.Monoid (Monoid(..))
import           Data.Vector.Vector2 (Vector2(..))
import qualified Graphics.DrawingCombinators as Draw
import           Graphics.DrawingCombinators ((%%))
import qualified Graphics.DrawingCombinators.Utils as DrawUtils
import qualified Graphics.UI.Bottle.Animation as Anim
import           Graphics.UI.Bottle.Animation (AnimId, Size)
import           Graphics.UI.Bottle.Rect (Rect(Rect))
import qualified Graphics.UI.Bottle.Rect as Rect
import           Graphics.UI.Bottle.View (View(..))
import qualified Graphics.UI.Bottle.View as View
import           Graphics.UI.Bottle.Widget (Widget)
import qualified Graphics.UI.Bottle.Widget as Widget

data Style = Style {
  _styleColor :: Draw.Color,
  _styleFont :: Draw.Font,
  _styleFontSize :: Int
  }
Lens.makeLenses ''Style

fontRender :: Style -> String -> (Draw.Image (), Size)
fontRender (Style color font ptSize) =
  ((DrawUtils.scale (fromIntegral ptSize) %%) . Draw.tint color . DrawUtils.drawText font) &&&
  (fmap (fromIntegral ptSize *) . DrawUtils.textSize font)

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
    vertical = _1 .~ 0

nestedFrame ::
  Show a => (a, (Draw.Image (), Size)) -> (AnimId -> Anim.Frame, Size)
nestedFrame (i, (image, size)) =
  (draw, size)
  where
    draw animId =
      Anim.sizedFrame (View.augmentAnimId animId i) size image

drawTextAsSingleLetters ::
  Style -> String -> (AnimId -> Anim.Frame, Size)
drawTextAsSingleLetters style text =
  joinLines $
  map
  ((_2 %~ max minLineSize) . drawMany horizontal .
   map (nestedFrame . (_2 %~ renderLetter))) .
  splitWhen ((== '\n') . snd) $ text ^@.. Lens.traversed
  where
    (_, minLineSize) = fontRender style ""
    horizontal = _2 .~ 0
    renderLetter = fontRender style . (:[])

-- | Returns at least one rect
letterRects :: Style -> String -> [[Rect]]
letterRects style text =
  zipWith locateLineHeight (iterate (+ lineHeight) 0) textLines
  where
    -- splitWhen returns at least one string:
    textLines = map makeLine $ splitWhen (== '\n') text
    locateLineHeight y = Lens.mapped . Rect.top +~ y
    (_, Vector2 _ lineHeight) = fontRender style ""
    makeLine textLine =
      -- scanl returns at least one element
      zipWith makeLetterRect sizes . scanl (+) 0 .
      map (^. _1) $ sizes
      where
        sizes = map toSize textLine
        toSize = snd . fontRender style . (:[])
        makeLetterRect size xpos = Rect (Vector2 xpos 0) size

drawTextAsLines :: Style -> String -> (AnimId -> Anim.Frame, Size)
drawTextAsLines style text =
  splitWhen (== '\n') text ^@.. Lens.traversed
  <&> _1 %~ (,) "Line"
  <&> _2 %~ fontRender style
  <&> nestedFrame
  & joinLines

make :: Style -> String -> AnimId -> View
make style text animId = View textSize (frame animId)
  where
    (frame, textSize) = drawTextAsLines style text

makeWidget :: Style -> String -> AnimId -> Widget a
makeWidget style text = Widget.fromView . make style text

label :: Style -> AnimId -> String -> View
label style animId text = make style text $ View.augmentAnimId animId text
