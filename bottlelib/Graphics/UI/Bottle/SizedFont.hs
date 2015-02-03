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
import           Graphics.DrawingCombinators ((%%))
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.DrawingCombinators.Utils as DrawUtils

data SizedFont = SizedFont
    { _font :: Draw.Font
    , _fontSize :: Double
    }
Lens.makeLenses ''SizedFont

render :: SizedFont -> String -> Draw.Image ()
render SizedFont{..} str =
    DrawUtils.scale (realToFrac _fontSize) %%
    DrawUtils.drawText _font str

textHeight :: SizedFont -> Draw.R
textHeight SizedFont{..} = DrawUtils.textHeight * realToFrac _fontSize

textSize :: SizedFont -> String -> Vector2 Draw.R
textSize SizedFont{..} str = realToFrac _fontSize * DrawUtils.textSize _font str
