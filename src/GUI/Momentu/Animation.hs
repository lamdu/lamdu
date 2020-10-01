{-# LANGUAGE TemplateHaskell, DerivingVia, GeneralizedNewtypeDeriving #-}

module GUI.Momentu.Animation
    ( R, Size
    , Image(..), iAnimId, iUnitImage, iRect
    , Frame(..), frameImages, unitImages, images
    , draw
    , mapIdentities
    , unitSquare, emptyRectangle
    , coloredRectangle
    , translate, scale
    , singletonFrame, singletonUnitImage
    , module GUI.Momentu.Animation.Id
    ) where

import           Control.DeepSeq (NFData(..), deepseq)
import qualified Control.Lens as Lens
import           Data.Vector.Vector2 (Vector2(..))
import           GUI.Momentu.Animation.Id
import           GUI.Momentu.Rect (Rect(Rect))
import qualified GUI.Momentu.Rect as Rect
import           Graphics.DrawingCombinators (R, (%%))
import qualified Graphics.DrawingCombinators.Extended as Draw

import           GUI.Momentu.Prelude

type Size = Vector2 R

data Image = Image
    { _iAnimId :: AnimId
    , _iUnitImage :: !(Draw.Image ())
        -- iUnitImage always occupies (0,0)..(1,1),
        -- the translation/scaling occurs when drawing
    , _iRect :: !Rect
    } deriving (Generic)
Lens.makeLenses ''Image

-- Custom instance that skips iUnitImage which has no real idea of forcing
instance NFData Image where
    rnf (Image animId _image rect) = animId `deepseq` rect `deepseq` ()

newtype Frame = Frame
    { _frameImages :: [Image]
    }
    deriving stock Generic
    deriving newtype NFData
    deriving (Semigroup, Monoid) via Generically Frame
Lens.makeLenses ''Frame

{-# INLINE images #-}
images :: Lens.Traversal' Frame Image
images = frameImages . traverse

{-# INLINE unitImages #-}
unitImages :: Lens.Traversal' Frame (Draw.Image ())
unitImages = images . iUnitImage

singletonFrame :: Size -> AnimId -> Draw.Image () -> Frame
singletonFrame size animId =
    scale size .
    singletonUnitImage animId .
    (Draw.scaleV (1 / size) %%)

singletonUnitImage :: AnimId -> Draw.Image () -> Frame
singletonUnitImage animId image = Frame [Image animId image (Rect 0 1)]

draw :: Frame -> Draw.Image ()
draw frame =
    frame
    ^. frameImages
    <&> posImage
    & mconcat
    where
        posImage (Image _ img rect) =
            Draw.translateV (rect ^. Rect.topLeft) %%
            Draw.scaleV (rect ^. Rect.size) %%
            img

mapIdentities :: (AnimId -> AnimId) -> Frame -> Frame
mapIdentities f = images . iAnimId %~ f

unitSquare :: AnimId -> Frame
unitSquare animId = singletonFrame 1 animId Draw.square

emptyRectangle :: Vector2 R -> Vector2 R -> AnimId -> Frame
emptyRectangle (Vector2 fX fY) totalSize@(Vector2 sX sY) animId =
    mconcat
    [ rect 0                      (Vector2 sX fY)
    , rect (Vector2 0 (sY - fY))  (Vector2 sX fY)
    , rect (Vector2 0 fY)         (Vector2 fX (sY - fY*2))
    , rect (Vector2 (sX - fX) fY) (Vector2 fX (sY - fY*2))
    ]
    & singletonFrame totalSize animId
    where
        rect origin size =
            Draw.square
            & (Draw.scaleV size %%)
            & (Draw.translateV origin %%)

coloredRectangle :: AnimId -> Draw.Color -> Frame
coloredRectangle animId color =
    unitSquare animId
    & unitImages %~ Draw.tint color

translate :: Vector2 R -> Frame -> Frame
translate pos = images . iRect . Rect.topLeft +~ pos

scale :: Vector2 R -> Frame -> Frame
scale factor = images . iRect . Rect.topLeftAndSize *~ factor
