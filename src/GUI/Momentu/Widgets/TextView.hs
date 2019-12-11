{-# LANGUAGE TemplateHaskell, BangPatterns #-}
module GUI.Momentu.Widgets.TextView
    ( Font.Underline(..), Font.underlineColor, Font.underlineWidth
    , Style(..), styleColor, styleFont, styleUnderline, whiteText
    , color, font, underline
    , lineHeight

    , make, makeFocusable
    , Font.TextSize(..), bounding, advance
    , RenderedText(..), renderedText, renderedTextSize
    , drawText
    , letterRects
    ) where

import qualified Control.Lens as Lens
import qualified Data.Text as Text
import qualified Data.Text.Bidi as Bidi
import           Data.Vector.Vector2 (Vector2(..))
import           GUI.Momentu.Align (WithTextPos(..), TextWidget)
import qualified GUI.Momentu.Align as Align
import           GUI.Momentu.Animation (AnimId)
import qualified GUI.Momentu.Animation as Anim
import qualified GUI.Momentu.Direction as Dir
import           GUI.Momentu.Font (Font, RenderedText(..), renderedText, renderedTextSize, bounding, advance)
import qualified GUI.Momentu.Font as Font
import           GUI.Momentu.Rect (Rect(Rect))
import qualified GUI.Momentu.Rect as Rect
import qualified GUI.Momentu.State as State
import           GUI.Momentu.View (View(..))
import qualified GUI.Momentu.View as View
import qualified GUI.Momentu.Widget as Widget
import qualified Graphics.DrawingCombinators.Extended as Draw

import           GUI.Momentu.Prelude

data Style = Style
    { _styleColor :: Draw.Color
    , _styleFont :: Font
    , _styleUnderline :: Maybe Font.Underline
    }
Lens.makeLenses ''Style

underline :: Has Style env => Lens' env (Maybe Font.Underline)
underline = has . styleUnderline

font :: Has Style env => Lens' env Font
font = has . styleFont

color :: Has Style env => Lens' env Draw.Color
color = has . styleColor

whiteText :: Font -> Style
whiteText f =
    Style
    { _styleColor = Draw.Color 1 1 1 1
    , _styleFont = f
    , _styleUnderline = Nothing
    }

lineHeight :: Style -> Widget.R
lineHeight s = s ^. styleFont & Font.height

fontRender :: Style -> Text -> RenderedText (Draw.Image ())
fontRender s =
    Font.render (s ^. styleFont) (s ^. styleColor) (s ^. styleUnderline) . Bidi.toVisual

nestedFrame ::
    (Show a, Has Dir.Layout env, Has Style env) =>
    env ->
    (a, RenderedText (Draw.Image ())) -> RenderedText (AnimId -> Anim.Frame)
nestedFrame env (i, RenderedText size img) =
    RenderedText size draw
    where
        toFrame animId = Anim.singletonFrame anchorSize (Anim.augmentId i animId)
        widthV = Vector2 (size ^. bounding . _1) 0
        draw animId =
            case env ^. has of
            Dir.LeftToRight -> toFrame animId img
            Dir.RightToLeft ->
                (Draw.translateV (-widthV) Draw.%% img) & toFrame animId
                & Anim.translate widthV
        anchorSize = env ^. has & lineHeight & pure

-- | Returns at least one rect
letterRects :: Style -> Text -> [[Rect]]
letterRects s text =
    zipWith locateLineHeight (iterate (+ height) 0) textLines
    where
        -- splitOn returns at least one string:
        textLines = map makeLine $ Text.splitOn "\n" text
        locateLineHeight y = Lens.mapped . Rect.top +~ y
        height = lineHeight s
        makeLine textLine =
            sizes
            <&> fmap (^. _1)
            -- scanl returns at least one element:
            & scanl (+) 0
            & zipWith makeLetterRect sizes
            where
                sizes =
                    Text.unpack textLine
                    <&> Font.textSize (s ^. styleFont) . Text.singleton
                makeLetterRect size xpos =
                    Rect (Vector2 (xpos ^. advance) 0) (size ^. bounding)

drawText ::
    (MonadReader env m, Has Dir.Layout env, Has Style env) =>
    m (Text -> RenderedText (AnimId -> Anim.Frame))
drawText =
    Lens.view id
    <&> \env text ->
    nestedFrame env ("text" :: Text, fontRender (env ^. has) text)

make ::
    (MonadReader env m, Has Dir.Layout env, Has Style env) =>
    m (Text -> AnimId -> WithTextPos View)
make =
    drawText <&> \draw text animId ->
    let RenderedText textSize frame = draw text
    in  WithTextPos
        { _textTop = 0
        , _tValue = View.make (textSize ^. bounding) (frame animId)
        }

makeFocusable ::
    ( MonadReader env m, Applicative f, State.HasCursor env
    , Has Dir.Layout env, Has Style env
    ) =>
    m (Text -> Widget.Id -> TextWidget f)
makeFocusable =
    do
        toFocusable <- Widget.makeFocusableView
        mkText <- make
        pure $ \text myId ->
            mkText text (Widget.toAnimId myId)
            & Align.tValue %~ toFocusable myId
