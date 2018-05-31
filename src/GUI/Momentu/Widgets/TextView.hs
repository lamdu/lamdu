{-# LANGUAGE BangPatterns, TemplateHaskell #-}
module GUI.Momentu.Widgets.TextView
    ( Font.Underline(..), Font.underlineColor, Font.underlineWidth
    , Style(..), styleColor, styleFont, styleUnderline, whiteText
    , color, font, underline
    , lineHeight
    , HasStyle(..)

    , make, makeLabel, makeFocusable, makeFocusableLabel
    , Font.TextSize(..), bounding, advance
    , RenderedText(..), renderedText, renderedTextSize
    , drawText
    , letterRects
    ) where

import qualified Control.Lens as Lens
import qualified Data.Text as Text
import           Data.Text.Encoding (encodeUtf8)
import           Data.Vector.Vector2 (Vector2(..))
import           GUI.Momentu.Align (WithTextPos(..))
import qualified GUI.Momentu.Align as Align
import           GUI.Momentu.Animation (AnimId)
import qualified GUI.Momentu.Animation as Anim
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.Font (Font, RenderedText(..), renderedText, renderedTextSize, bounding, advance)
import qualified GUI.Momentu.Font as Font
import           GUI.Momentu.Rect (Rect(Rect))
import qualified GUI.Momentu.Rect as Rect
import qualified GUI.Momentu.State as State
import           GUI.Momentu.View (View(..))
import qualified GUI.Momentu.View as View
import           GUI.Momentu.Widget (Widget)
import qualified GUI.Momentu.Widget as Widget
import qualified Graphics.DrawingCombinators as Draw

import           Lamdu.Prelude

data Style = Style
    { _styleColor :: Draw.Color
    , _styleFont :: Font
    , _styleUnderline :: Maybe Font.Underline
    }
Lens.makeLenses ''Style

class HasStyle env where style :: Lens' env Style
instance HasStyle Style where style = id

underline :: HasStyle env => Lens' env (Maybe Font.Underline)
underline = style . styleUnderline

font :: HasStyle env => Lens' env Font
font = style . styleFont

color :: HasStyle env => Lens' env Draw.Color
color = style . styleColor

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
    Font.render (s ^. styleFont) (s ^. styleColor) (s ^. styleUnderline)

nestedFrame ::
    Show a =>
    Style ->
    (a, RenderedText (Draw.Image ())) -> RenderedText (AnimId -> Anim.Frame)
nestedFrame s (i, RenderedText size img) =
    RenderedText size draw
    where
        draw animId = Anim.singletonFrame anchorSize (Anim.augmentId i animId) img
        anchorSize = pure (lineHeight s)

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
    (MonadReader env m, HasStyle env) =>
    m (Text -> RenderedText (AnimId -> Anim.Frame))
drawText =
    Lens.view style <&> \s text -> nestedFrame s ("text" :: Text, fontRender s text)

make ::
    (MonadReader env m, HasStyle env) =>
    m (Text -> AnimId -> WithTextPos View)
make =
    drawText <&> \draw text animId ->
    let RenderedText textSize frame = draw text
    in  WithTextPos
        { _textTop = 0
        , _tValue = View.make (textSize ^. bounding) (frame animId)
        }

makeLabel ::
    (MonadReader env m, HasStyle env, Element.HasAnimIdPrefix env) =>
    Text -> m (WithTextPos View)
makeLabel text = (make ?? text) <*> Element.subAnimId [encodeUtf8 text]

makeFocusable ::
    (MonadReader env m, Applicative f, State.HasCursor env, HasStyle env) =>
    m (Text -> Widget.Id -> WithTextPos (Widget (f State.Update)))
makeFocusable =
    do
        toFocusable <- Widget.makeFocusableView
        mkText <- make
        pure $ \text myId ->
            mkText text (Widget.toAnimId myId)
            & Align.tValue %~ toFocusable myId

makeFocusableLabel ::
    (MonadReader env m, Applicative f, State.HasCursor env, HasStyle env, Element.HasAnimIdPrefix env) =>
    Text -> m (WithTextPos (Widget (f State.Update)))
makeFocusableLabel text =
    do
        toFocusable <- Widget.makeFocusableView
        widgetId <- Element.subAnimId [encodeUtf8 text] <&> Widget.Id
        makeLabel text <&> Align.tValue %~ toFocusable widgetId
