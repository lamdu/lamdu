{-# LANGUAGE TemplateHaskell, RecordWildCards #-}
-- | A font attached to its size

module Graphics.UI.Bottle.SizedFont
    ( SizedFont(..), font, fontSize
    , render
    , textHeight
    , textSize
    ) where

import qualified Control.Lens as Lens
import           Data.Vector.Vector2 (Vector2)
import           Graphics.DrawingCombinators.Utils ((%%))
import qualified Graphics.DrawingCombinators.Utils as DrawUtils

data SizedFont = SizedFont
    { _font :: DrawUtils.Font
    , _fontSize :: Double
    }
Lens.makeLenses ''SizedFont

render :: SizedFont -> String -> DrawUtils.Image
render SizedFont{..} str =
    DrawUtils.scale (realToFrac _fontSize) %%
    DrawUtils.drawText _font str

textHeight :: SizedFont -> DrawUtils.R
textHeight SizedFont{..} = DrawUtils.textHeight * realToFrac _fontSize

textSize :: SizedFont -> String -> Vector2 DrawUtils.R
textSize SizedFont{..} str = realToFrac _fontSize * DrawUtils.textSize _font str
