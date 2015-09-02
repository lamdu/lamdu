{-# LANGUAGE TemplateHaskell, RecordWildCards #-}
-- | A font attached to its size

module Graphics.UI.Bottle.SizedFont
    ( SizedFont(..), font, fontSize
    , DrawUtils.TextSize(..)
    , render, textSize, textHeight
    ) where

import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Data.Vector.Vector2 (Vector2)
import           Graphics.DrawingCombinators ((%%))
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.DrawingCombinators.Utils as DrawUtils

data SizedFont = SizedFont
    { _font :: Draw.Font
    , _fontSize :: Double
    }
Lens.makeLenses ''SizedFont

render :: SizedFont -> String -> (DrawUtils.TextSize (Vector2 Draw.R), Draw.Image ())
render sizedFont str =
    ( textSize sizedFont str
    , DrawUtils.scale (realToFrac (sizedFont ^. fontSize)) %%
      DrawUtils.drawText (sizedFont ^. font) str
    )

textHeight :: SizedFont -> Draw.R
textHeight SizedFont{..} = DrawUtils.textHeight * realToFrac _fontSize

textSize :: SizedFont -> String -> DrawUtils.TextSize (Vector2 Draw.R)
textSize SizedFont{..} str = (realToFrac _fontSize *) <$> DrawUtils.textSize _font str
