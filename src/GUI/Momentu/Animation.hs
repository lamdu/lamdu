{-# LANGUAGE NoImplicitPrelude, DeriveGeneric, TemplateHaskell #-}

module GUI.Momentu.Animation
    ( R, Size
    , Image(..), iAnimId, iUnitImage, iRect
    , Frame(..), frameImages, unitImages, images
    , draw
    , mapIdentities
    , unitSquare, emptyRectangle
    , backgroundColor
    , translate, scale
    , singletonFrame
    , module GUI.Momentu.Animation.Id
    ) where

import qualified Control.Lens as Lens
import           Data.Vector.Vector2 (Vector2(..))
import           GHC.Generics (Generic)
import           Graphics.DrawingCombinators (R, (%%))
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.DrawingCombinators.Utils as DrawUtils
import           GUI.Momentu.Animation.Id
import           GUI.Momentu.Rect (Rect(Rect))
import qualified GUI.Momentu.Rect as Rect

import           Lamdu.Prelude

type Size = Vector2 R

data Image = Image
    { _iAnimId :: AnimId
    , _iUnitImage :: !(Draw.Image ())
        -- iUnitImage always occupies (0,0)..(1,1),
        -- the translation/scaling occurs when drawing
    , _iRect :: !Rect
    } deriving (Generic)
Lens.makeLenses ''Image

newtype Frame = Frame
    { _frameImages :: [Image]
    } deriving (Generic)
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
    singletonUnitImage .
    (DrawUtils.scale (1 / size) %%)
    where
        singletonUnitImage image = Frame [Image animId image (Rect 0 1)]

instance Monoid Frame where
    mempty = Frame mempty
    mappend (Frame m0) (Frame m1) = Frame (m0 ++ m1)

draw :: Frame -> Draw.Image ()
draw frame =
    frame
    ^. frameImages
    <&> posImage
    & mconcat
    where
        posImage (Image _ img rect) =
            DrawUtils.translate (rect ^. Rect.topLeft) %%
            DrawUtils.scale (rect ^. Rect.size) %%
            img

mapIdentities :: (AnimId -> AnimId) -> Frame -> Frame
mapIdentities f = images . iAnimId %~ f

unitSquare :: AnimId -> Frame
unitSquare animId = singletonFrame 1 animId DrawUtils.square

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
            DrawUtils.square
            & (DrawUtils.scale size %%)
            & (DrawUtils.translate origin %%)

backgroundColor :: AnimId -> Draw.Color -> Vector2 R -> Frame
backgroundColor animId color size =
    unitSquare animId
    & unitImages %~ Draw.tint color
    & scale size

translate :: Vector2 R -> Frame -> Frame
translate pos = images . iRect . Rect.topLeft +~ pos

scale :: Vector2 R -> Frame -> Frame
scale factor = images . iRect . Rect.topLeftAndSize *~ factor
