{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
-- | A font attached to its size

module GUI.Momentu.Font
    ( Underline(..), underlineColor, underlineWidth
    , render
    , RenderedText(..), renderedTextSize, renderedText
    , TextSize(..), bounding, advance
    , Draw.Font, Draw.openFont, Draw.openFontNoLCD
    , height
    , textSize
    , renderText
    ) where

import           Control.Applicative (liftA2)
import qualified Control.Lens as Lens
import           Data.Text
import qualified Data.Text as Text
import           Data.Vector.Vector2 (Vector2(..))
import           Graphics.DrawingCombinators ((%%))
import qualified Graphics.DrawingCombinators as Draw
import           Graphics.DrawingCombinators.Utils (Image)
import qualified Graphics.DrawingCombinators.Utils as DrawUtils

import           Lamdu.Prelude

data Underline = Underline
    { _underlineColor :: Draw.Color
    , _underlineWidth :: Draw.R
    }
Lens.makeLenses ''Underline

data TextSize a = TextSize
    { _bounding :: a
    , _advance :: a
    } deriving (Functor, Foldable, Traversable)
Lens.makeLenses ''TextSize

instance Applicative TextSize where
    pure x = TextSize x x
    TextSize f1 f2 <*> TextSize x1 x2 = TextSize (f1 x1) (f2 x2)
instance Num a => Num (TextSize a) where
    fromInteger = pure . fromInteger
    (+) = liftA2 (+)
    (*) = liftA2 (*)
    abs = fmap abs
    signum = fmap signum
    negate = fmap negate

data RenderedText a = RenderedText
    { _renderedTextSize :: TextSize (Vector2 Draw.R)
    , _renderedText :: a
    }
Lens.makeLenses ''RenderedText

height :: Draw.Font -> Draw.R
height = Draw.fontHeight

render ::
    Draw.Font -> Draw.Color -> Maybe Underline -> Text ->
    RenderedText Image
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

drawUnderline :: Draw.Font -> Vector2 Draw.R -> Underline -> Draw.Image ()
drawUnderline font size (Underline color relativeWidth) =
    DrawUtils.square
    & (DrawUtils.scale (Vector2 (size ^. _1) width) %%)
    & (DrawUtils.translate (Vector2 0 (size ^. _2 + descender + width/2)) %%)
    & Draw.tint color
    where
        width = relativeWidth * height font
        descender = Draw.fontDescender font

textWidth :: Draw.Font -> Text -> TextSize Draw.R
textWidth font str =
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

textSize :: Draw.Font -> Text -> TextSize (Vector2 Draw.R)
textSize font str =
    (`Vector2` totalHeight) <$> textWidth font str
    where
        totalHeight = height font * fromIntegral numLines
        numLines = 1 + Text.count "\n" str

renderText :: Draw.Font -> Draw.TextAttrs -> Text -> RenderedText Image
renderText font textAttrs str =
    RenderedText
    { _renderedTextSize = textSize font str
    , _renderedText =
        Draw.text font str textAttrs
        & void
        -- Text is normally at height -0.5..1.5.  We move it to be -textHeight..0
        & (DrawUtils.translate (Vector2 0 (-Draw.fontHeight font - Draw.fontDescender font)) %%)
        -- We want to reverse it so that higher y is down, and it is also
        -- moved to 0..2
        & (DrawUtils.scale (Vector2 1 (-1)) %%)
    }
