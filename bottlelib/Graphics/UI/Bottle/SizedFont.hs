{-# LANGUAGE TemplateHaskell, RecordWildCards #-}
-- | A font attached to its size

module Graphics.UI.Bottle.SizedFont
    ( SizedFont(..), font, fontSize
    , Underline(..), underlineColor, underlineWidth
    , DrawUtils.TextSize(..)
    , render, textSize, textHeight
    ) where

import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Lens.Tuple
import           Data.Monoid ((<>))
import           Data.Vector.Vector2 (Vector2(..))
import           Graphics.DrawingCombinators ((%%))
import qualified Graphics.DrawingCombinators as Draw
import           Graphics.DrawingCombinators.Utils (TextSize(..))
import qualified Graphics.DrawingCombinators.Utils as DrawUtils

data SizedFont = SizedFont
    { _font :: Draw.Font
    , _fontSize :: Double
    }
Lens.makeLenses ''SizedFont

data Underline = Underline
    { _underlineColor :: Draw.Color
    , _underlineWidth :: Draw.R
    }
Lens.makeLenses ''Underline

render ::
    SizedFont -> Draw.Color -> Maybe Underline -> String ->
    (TextSize (Vector2 Draw.R), Draw.Image ())
render sizedFont color mUnderline str =
    ( size
    , ( ( DrawUtils.scale (realToFrac (sizedFont ^. fontSize)) %%
          DrawUtils.drawText (sizedFont ^. font) str
        ) & Draw.tint color
      ) <> maybe mempty (drawUnderline sizedFont (DrawUtils.bounding size)) mUnderline
    )
    where
        size = textSize sizedFont str

drawUnderline :: SizedFont -> Vector2 Draw.R -> Underline -> Draw.Image ()
drawUnderline sizedFont size (Underline color width) =
    DrawUtils.square
    & (DrawUtils.scale (Vector2 (size ^. _1) width) %%)
    & (DrawUtils.translate (Vector2 0 (size ^. _2 + descender + width/2)) %%)
    & Draw.tint color
    where
        descender =
            realToFrac (sizedFont ^. fontSize) *
            Draw.fontDescender (sizedFont ^. font)

textHeight :: SizedFont -> Draw.R
textHeight SizedFont{..} = DrawUtils.textHeight * realToFrac _fontSize

textSize :: SizedFont -> String -> TextSize (Vector2 Draw.R)
textSize SizedFont{..} str = (realToFrac _fontSize *) <$> DrawUtils.textSize _font str
