{-# LANGUAGE NoImplicitPrelude, BangPatterns, RecordWildCards, TemplateHaskell, OverloadedStrings #-}
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

import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Lens.Tuple
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Vector.Vector2 (Vector2(..))
import qualified Graphics.DrawingCombinators as Draw
import           Graphics.UI.Bottle.Animation (AnimId, Size)
import qualified Graphics.UI.Bottle.Animation as Anim
import           Graphics.UI.Bottle.Rect (Rect(Rect))
import qualified Graphics.UI.Bottle.Rect as Rect
import           Graphics.UI.Bottle.Font (TextSize(..))
import qualified Graphics.UI.Bottle.Font as Font
import           Graphics.UI.Bottle.View (View(..))
import qualified Graphics.UI.Bottle.View as View
import           Graphics.UI.Bottle.Widget (Widget)
import qualified Graphics.UI.Bottle.Widget as Widget

import           Prelude.Compat

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

fontRender :: Style -> Text -> RenderedText (Draw.Image ())
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
letterRects :: Style -> Text -> [[Rect]]
letterRects Style{..} text =
    zipWith locateLineHeight (iterate (+ height) 0) textLines
    where
        -- splitOn returns at least one string:
        textLines = map makeLine $ Text.splitOn "\n" text
        locateLineHeight y = Lens.mapped . Rect.top +~ y
        height = Font.height _styleFont
        makeLine textLine =
            sizes
            <&> fmap (^. _1)
            -- scanl returns at least one element:
            & scanl (+) 0
            & zipWith makeLetterRect sizes
            where
                sizes =
                    Text.unpack textLine
                    <&> Font.textSize _styleFont . Text.singleton
                makeLetterRect size xpos =
                    Rect (Vector2 (advance xpos) 0) (bounding size)

drawText :: Style -> Text -> RenderedText (AnimId -> Anim.Frame)
drawText style text = nestedFrame style ("text" :: Text, fontRender style text)

make :: Style -> Text -> AnimId -> View
make style text animId =
    View.make (bounding textSize) (frame animId)
    where
        RenderedText textSize frame = drawText style text

makeWidget :: Style -> Text -> AnimId -> Widget a
makeWidget style text = Widget.fromView . make style text

label :: Style -> AnimId -> Text -> View
label style animId text = make style text $ Anim.augmentId animId text
