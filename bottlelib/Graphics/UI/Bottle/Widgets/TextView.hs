{-# LANGUAGE NoImplicitPrelude, BangPatterns, RecordWildCards, TemplateHaskell #-}
module Graphics.UI.Bottle.Widgets.TextView
    ( Font.Underline(..), Font.underlineColor, Font.underlineWidth
    , Style(..), styleColor, styleFont, styleUnderline
    , lineHeight

    , make, makeWidget
    , label
    , RenderedText(..), renderedTextSize
    , drawText
    , letterRects
    ) where

import           Prelude.Compat

import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Lens.Tuple
import           Data.List.Split (splitWhen)
import           Data.Vector.Vector2 (Vector2(..))
import qualified Graphics.DrawingCombinators as Draw
import           Graphics.UI.Bottle.Animation (AnimId, Size)
import qualified Graphics.UI.Bottle.Animation as Anim
import           Graphics.UI.Bottle.Rect (Rect(Rect))
import qualified Graphics.UI.Bottle.Rect as Rect
import           Graphics.UI.Bottle.Font (TextSize(..))
import qualified Graphics.UI.Bottle.Font as Font
import           Graphics.UI.Bottle.View (View(..))
import           Graphics.UI.Bottle.Widget (Widget)
import qualified Graphics.UI.Bottle.Widget as Widget

data Style = Style
    { _styleColor :: Draw.Color
    , _styleFont :: Draw.Font
    , _styleUnderline :: Maybe Font.Underline
    }
Lens.makeLenses ''Style

lineHeight :: Style -> Widget.R
lineHeight Style{..} = Font.height _styleFont

data RenderedText a = RenderedText
    { _renderedTextSize :: TextSize Size
    , renderedText :: a
    }
Lens.makeLenses ''RenderedText

fontRender :: Style -> String -> RenderedText (Draw.Image ())
fontRender Style{..} str =
    Font.render _styleFont _styleColor _styleUnderline str
    & uncurry RenderedText

nestedFrame ::
    Show a =>
    Style ->
    (a, RenderedText (Draw.Image ())) -> RenderedText (AnimId -> Anim.Frame)
nestedFrame style (i, RenderedText size img) =
    RenderedText size draw
    where
        draw animId =
            Anim.sizedFrame (Anim.augmentId animId i) anchorSize img
        anchorSize = pure (lineHeight style)

-- | Returns at least one rect
letterRects :: Style -> String -> [[Rect]]
letterRects Style{..} text =
    zipWith locateLineHeight (iterate (+ height) 0) textLines
    where
        -- splitWhen returns at least one string:
        textLines = map makeLine $ splitWhen (== '\n') text
        locateLineHeight y = Lens.mapped . Rect.top +~ y
        height = Font.height _styleFont
        makeLine textLine =
            sizes
            <&> fmap (^. _1)
            -- scanl returns at least one element:
            & scanl (+) 0
            & zipWith makeLetterRect sizes
            where
                sizes = textLine <&> Font.textSize _styleFont . (:[])
                makeLetterRect size xpos =
                    Rect (Vector2 (advance xpos) 0) (bounding size)

drawText :: Style -> String -> RenderedText (AnimId -> Anim.Frame)
drawText style text = nestedFrame style ("text", fontRender style text)

make :: Style -> String -> AnimId -> View
make style text animId =
    View (bounding textSize) (frame animId)
    where
        RenderedText textSize frame = drawText style text

makeWidget :: Style -> String -> AnimId -> Widget a
makeWidget style text = Widget.fromView . make style text

label :: Style -> AnimId -> String -> View
label style animId text = make style text $ Anim.augmentId animId text
