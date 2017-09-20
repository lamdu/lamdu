{-# LANGUAGE TemplateHaskell #-}
-- | A font attached to its size

module GUI.Momentu.Font
    ( Underline(..), underlineColor, underlineWidth
    , render
    , RenderedText(..), DrawUtils.renderedText, DrawUtils.renderedTextSize
    , TextSize(..), DrawUtils.bounding, DrawUtils.advance
    , DrawUtils.textSize
    , height
    ) where

import qualified Control.Lens as Lens
import           Data.Vector.Vector2 (Vector2(..))
import           Graphics.DrawingCombinators ((%%))
import qualified Graphics.DrawingCombinators as Draw
import           Graphics.DrawingCombinators.Utils (TextSize(..), Image, RenderedText, renderedText, renderedTextSize)
import qualified Graphics.DrawingCombinators.Utils as DrawUtils

import           Lamdu.Prelude

data Underline = Underline
    { _underlineColor :: Draw.Color
    , _underlineWidth :: Draw.R
    }
Lens.makeLenses ''Underline

height :: Draw.Font -> Draw.R
height = DrawUtils.fontHeight

render ::
    Draw.Font -> Draw.Color -> Maybe Underline -> Text ->
    RenderedText Image
render font color mUnderline str =
    r
    & renderedText <>~
        maybe mempty (drawUnderline font (r ^. renderedTextSize . DrawUtils.bounding)) mUnderline
    where
        r = DrawUtils.renderText font attrs str
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
