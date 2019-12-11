{-# LANGUAGE TemplateHaskell, DerivingVia #-}
-- | A font attached to its size

module GUI.Momentu.Font
    ( Font
    , Underline(..), underlineColor, underlineWidth
    , render
    , RenderedText(..), renderedTextSize, renderedText
    , TextSize(..), bounding, advance
    , LCDSubPixelEnabled(..), openFont
    , height
    , textSize, textWidth
    , renderText
    ) where

import           Control.Applicative (liftA2)
import qualified Control.Lens as Lens
import qualified Data.Text as Text
import           Data.Vector.Vector2 (Vector2(..))
import           Graphics.DrawingCombinators ((%%), R)
import qualified Graphics.DrawingCombinators.Extended as Draw

import           GUI.Momentu.Prelude

data Font
    = Font Draw.Font
    | FontDebug R
Lens.makePrisms ''Font

data Underline = Underline
    { _underlineColor :: Draw.Color
    , _underlineWidth :: R
    }
Lens.makeLenses ''Underline

data TextSize a = TextSize
    { _bounding :: a
    , _advance :: a
    } deriving stock (Generic, Generic1, Functor, Foldable, Traversable)
    deriving Applicative via Generically1 TextSize
Lens.makeLenses ''TextSize

instance Num a => Num (TextSize a) where
    fromInteger = pure . fromInteger
    (+) = liftA2 (+)
    (*) = liftA2 (*)
    abs = fmap abs
    signum = fmap signum
    negate = fmap negate

data RenderedText a = RenderedText
    { _renderedTextSize :: TextSize (Vector2 R)
    , _renderedText :: a
    }
Lens.makeLenses ''RenderedText

height :: Font -> R
height (Font f) = Draw.fontHeight f
height (FontDebug x) = x

descender :: Font -> R
descender (Font f) = Draw.fontDescender f
descender (FontDebug x) = x * 0.75

render ::
    Font -> Draw.Color -> Maybe Underline -> Text ->
    RenderedText (Draw.Image ())
render font color mUnderline str =
    r
    & renderedText <>~
        foldMap (drawUnderline font (r ^. renderedTextSize . bounding)) mUnderline
    where
        r = renderText font attrs str
        attrs =
            Draw.defTextAttrs
            { Draw.gamma = 1.0
            , Draw.foregroundColor = color
            }

drawUnderline :: Font -> Vector2 R -> Underline -> Draw.Image ()
drawUnderline font size (Underline color relativeWidth) =
    Draw.square
    & (Draw.scale (size ^. _1) width %%)
    & (Draw.translate (0, size ^. _2 + descender font + width/2) %%)
    & Draw.tint color
    where
        width = relativeWidth * height font

textWidth :: Font -> Text -> TextSize R
textWidth (Font font) str =
    TextSize
    { _bounding =
      Draw.textBoundingWidth font str
      -- max with advance because spaces are counted only in advance
      -- but not the bounding width
      & max adv
    , _advance = adv
    }
    where
        adv = Draw.textAdvance font str
textWidth (FontDebug x) text =
    TextSize
    { _bounding = r
    , _advance = r
    }
    where
        r = x * fromIntegral (maximum (0 : (Text.lines text <&> Text.length)))

textSize :: Font -> Text -> TextSize (Vector2 R)
textSize font str =
    (`Vector2` totalHeight) <$> textWidth font str
    where
        totalHeight = height font * fromIntegral numLines
        numLines = 1 + Text.count "\n" str

renderText :: Font -> Draw.TextAttrs -> Text -> RenderedText (Draw.Image ())
renderText font textAttrs str =
    RenderedText
    { _renderedTextSize = size
    , _renderedText =
        case font of
        Font f ->
            Draw.text f str textAttrs
            & void
            -- Text is normally at height -0.5..1.5.  We move it to be -textHeight..0
            & (Draw.translate (0, -Draw.fontHeight f - Draw.fontDescender f) %%)
            -- We want to reverse it so that higher y is down, and it is also
            -- moved to 0..2
            & (Draw.scale 1 (-1) %%)
        FontDebug{} -> Draw.scaleV (size ^. bounding) %% Draw.square
    }
    where
        size = textSize font str

data LCDSubPixelEnabled = LCDSubPixelEnabled | LCDSubPixelDisabled

openFont :: LCDSubPixelEnabled -> Float -> FilePath -> IO Font
openFont _ size "" = FontDebug (realToFrac size) & pure
openFont subPixel size path =
    open size path <&> Font
    where
        open =
            case subPixel of
            LCDSubPixelEnabled -> Draw.openFont
            LCDSubPixelDisabled -> Draw.openFontNoLCD
