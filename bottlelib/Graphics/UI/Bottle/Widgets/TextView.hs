{-# LANGUAGE RecordWildCards, TemplateHaskell #-}
module Graphics.UI.Bottle.Widgets.TextView
    ( Style(..), styleColor, styleFont
    , lineHeight

    , make, makeWidget
    , label
    , drawTextAsSingleLetters, drawTextAsLines
    , letterRects
    ) where

import           Control.Applicative (liftA2)
import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Lens.Tuple
import           Data.List (foldl')
import           Data.List.Split (splitWhen)
import           Data.Monoid (Monoid(..))
import           Data.Vector.Vector2 (Vector2(..))
import qualified Graphics.DrawingCombinators.Utils as DrawUtils
import           Graphics.UI.Bottle.Animation (AnimId, Size)
import qualified Graphics.UI.Bottle.Animation as Anim
import           Graphics.UI.Bottle.Rect (Rect(Rect))
import qualified Graphics.UI.Bottle.Rect as Rect
import           Graphics.UI.Bottle.SizedFont (SizedFont)
import qualified Graphics.UI.Bottle.SizedFont as SizedFont
import           Graphics.UI.Bottle.View (View(..))
import qualified Graphics.UI.Bottle.View as View
import           Graphics.UI.Bottle.Widget (Widget)
import qualified Graphics.UI.Bottle.Widget as Widget

data Style = Style
    { _styleColor :: DrawUtils.Color
    , _styleFont :: SizedFont
    }
Lens.makeLenses ''Style

lineHeight :: Style -> Widget.R
lineHeight Style{..} = SizedFont.textHeight _styleFont

fontRender :: Style -> String -> (DrawUtils.Image, Size)
fontRender Style{..} str =
    ( str
      & SizedFont.render _styleFont
      & DrawUtils.tint _styleColor
    , str
      & SizedFont.textSize _styleFont
    )

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
    Show a => (a, (DrawUtils.Image, Size)) -> (AnimId -> Anim.Frame, Size)
nestedFrame (i, (image, size)) =
    (draw, size)
    where
        draw animId =
            Anim.sizedFrame (View.augmentAnimId animId i) size image

drawTextAsSingleLetters ::
    Style -> String -> (AnimId -> Anim.Frame, Size)
drawTextAsSingleLetters style text =
    text ^@.. Lens.traversed
    & splitWhen ((== '\n') . snd)
    <&> Lens.mapped . _2 %~ renderLetter
    <&> Lens.mapped %~ nestedFrame
    <&> drawMany horizontal
    <&> _2 %~ max minLineSize
    & joinLines
    where
        (_, minLineSize) = fontRender style ""
        horizontal = _2 .~ 0
        renderLetter = fontRender style . (:[])

-- | Returns at least one rect
letterRects :: Style -> String -> [[Rect]]
letterRects style text =
    zipWith locateLineHeight (iterate (+ height) 0) textLines
    where
        -- splitWhen returns at least one string:
        textLines = map makeLine $ splitWhen (== '\n') text
        locateLineHeight y = Lens.mapped . Rect.top +~ y
        (_, Vector2 _ height) = fontRender style ""
        makeLine textLine =
            sizes
            <&> (^. _1)
            -- scanl returns at least one element:
            & scanl (+) 0
            & zipWith makeLetterRect sizes
            where
                sizes = textLine <&> snd . fontRender style . (:[])
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
