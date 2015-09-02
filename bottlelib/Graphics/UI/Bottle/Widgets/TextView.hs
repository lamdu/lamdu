{-# LANGUAGE NoImplicitPrelude, BangPatterns, RecordWildCards, TemplateHaskell #-}
module Graphics.UI.Bottle.Widgets.TextView
    ( SizedFont.Underline(..), SizedFont.underlineColor, SizedFont.underlineWidth
    , Style(..), styleColor, styleFont, styleUnderline
    , lineHeight

    , make, makeWidget
    , label
    , RenderedText(..), renderedTextSize
    , drawTextAsSingleLetters, drawTextAsLines
    , letterRects
    ) where

import           Prelude.Compat

import           Control.Applicative (liftA2)
import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Lens.Tuple
import           Data.List (foldl')
import           Data.List.Split (splitWhen)
import           Data.Vector.Vector2 (Vector2(..))
import qualified Graphics.DrawingCombinators as Draw
import           Graphics.UI.Bottle.Animation (AnimId, Size)
import qualified Graphics.UI.Bottle.Animation as Anim
import           Graphics.UI.Bottle.Rect (Rect(Rect))
import qualified Graphics.UI.Bottle.Rect as Rect
import           Graphics.UI.Bottle.SizedFont (SizedFont, TextSize(..))
import qualified Graphics.UI.Bottle.SizedFont as SizedFont
import           Graphics.UI.Bottle.View (View(..))
import qualified Graphics.UI.Bottle.View as View
import           Graphics.UI.Bottle.Widget (Widget)
import qualified Graphics.UI.Bottle.Widget as Widget

data Style = Style
    { _styleColor :: Draw.Color
    , _styleFont :: SizedFont
    , _styleUnderline :: Maybe SizedFont.Underline
    }
Lens.makeLenses ''Style

lineHeight :: Style -> Widget.R
lineHeight Style{..} = SizedFont.textHeight _styleFont

data RenderedText a = RenderedText
    { _renderedTextSize :: TextSize Size
    , renderedText :: a
    }
Lens.makeLenses ''RenderedText

fontRender :: Style -> String -> RenderedText (Draw.Image ())
fontRender Style{..} str =
    SizedFont.render _styleFont _styleColor _styleUnderline str
    & uncurry RenderedText

drawMany ::
    (Size -> Size) ->
    [RenderedText (AnimId -> Anim.Frame)] ->
    RenderedText (AnimId -> Anim.Frame)
drawMany sizeToTranslate =
    foldl' step (RenderedText 0 mempty)
    where
        step (RenderedText sizeAcc drawAcc) (RenderedText sizeX drawX) =
            RenderedText
            TextSize
            { bounding = addAndMax bounding
            , advance = addAndMax advance
            }
            (mappend drawAcc (Anim.translate (advance trans) . drawX))
            where
                addAndMax component =
                    -- Add the relevant axis from the advance-so-far, to the
                    -- new component. The relevant axis gets added, and the
                    -- irrelevant axis gets to participate in the max below
                    sizeToTranslate (advance sizeAcc) + component sizeX
                    -- Keep previous accumulator as min bound on
                    -- irrelevant axis (does nothing to relevant axis):
                    & liftA2 max (component sizeAcc)
                trans = sizeToTranslate <$> sizeAcc

joinLines ::
    [RenderedText (AnimId -> Anim.Frame)] ->
    RenderedText (AnimId -> Anim.Frame)
joinLines =
    drawMany vertical
    where
        vertical = _1 .~ 0

nestedFrame ::
    Show a => (a, RenderedText (Draw.Image ())) -> RenderedText (AnimId -> Anim.Frame)
nestedFrame (i, RenderedText size img) =
    RenderedText size draw
    where
        draw animId =
            Anim.sizedFrame (View.augmentAnimId animId i) (bounding size) img

drawTextAsSingleLetters :: Style -> String -> RenderedText (AnimId -> Anim.Frame)
drawTextAsSingleLetters style text =
    text ^@.. Lens.traversed
    & splitWhen ((== '\n') . snd)
    <&> Lens.mapped . _2 %~ renderLetter
    <&> Lens.mapped %~ nestedFrame
    <&> drawMany horizontal
    <&> renderedTextSize . Lens.mapped . _2 %~ max minLineSize
    & joinLines
    where
        minLineSize = SizedFont.textHeight (_styleFont style)
        horizontal = _2 .~ 0
        renderLetter = fontRender style . (:[])

-- | Returns at least one rect
letterRects :: Style -> String -> [[Rect]]
letterRects Style{..} text =
    zipWith locateLineHeight (iterate (+ height) 0) textLines
    where
        -- splitWhen returns at least one string:
        textLines = map makeLine $ splitWhen (== '\n') text
        locateLineHeight y = Lens.mapped . Rect.top +~ y
        height = SizedFont.textHeight _styleFont
        makeLine textLine =
            sizes
            <&> fmap (^. _1)
            -- scanl returns at least one element:
            & scanl (+) 0
            & zipWith makeLetterRect sizes
            where
                sizes = textLine <&> SizedFont.textSize _styleFont . (:[])
                makeLetterRect size xpos =
                    Rect (Vector2 (advance xpos) 0) (bounding size)

drawTextAsLines :: Style -> String -> RenderedText (AnimId -> Anim.Frame)
drawTextAsLines style text =
    splitWhen (== '\n') text ^@.. Lens.traversed
    <&> _1 %~ (,) "Line"
    <&> _2 %~ fontRender style
    <&> nestedFrame
    & joinLines

make :: Style -> String -> AnimId -> View
make style text animId = View (bounding textSize) (frame animId)
    where
        RenderedText textSize frame = drawTextAsLines style text

makeWidget :: Style -> String -> AnimId -> Widget a
makeWidget style text = Widget.fromView . make style text

label :: Style -> AnimId -> String -> View
label style animId text = make style text $ View.augmentAnimId animId text
