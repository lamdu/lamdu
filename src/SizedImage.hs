module SizedImage where

import qualified Graphics.DrawingCombinators as Draw
import SizeRange (Size, SizeRange)

data SizedImage = SizedImage
    { requestedSize :: SizeRange
    , imageOfSize :: Size -> Draw.Image ()
    }
