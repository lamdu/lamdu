{-# LANGUAGE NoImplicitPrelude, FlexibleContexts, FlexibleInstances #-}
module GUI.Momentu.Element
    ( Element(..), SizedElement(..), Size
    , HasAnimIdPrefix(..), subAnimId
    , topLayer, bottomLayer
    , width, height
    , tint
    , hoverInPlaceOf
    , padToSizeAlign
    ) where

import           Control.Applicative (liftA2)
import qualified Control.Lens as Lens
import qualified Data.ByteString as SBS
import           Data.Vector.Vector2 (Vector2(..))
import           GUI.Momentu.Animation (AnimId, R)
import qualified GUI.Momentu.Animation as Anim
import           GUI.Momentu.View (View(..))
import qualified GUI.Momentu.View as View
import qualified Graphics.DrawingCombinators as Draw

import           Lamdu.Prelude

type Size = View.Size

class Element a where
    setLayers :: Lens.IndexedSetter' Size a View.Layers
    hoverLayers :: a -> a
    -- Different `SetLayers`s do additional things when padding
    -- (Moving focal points, alignments, etc)
    pad :: Vector2 R -> a -> a
    pad p = assymetricPad p p
    assymetricPad :: Vector2 R -> Vector2 R -> a -> a
    scale :: Vector2 R -> a -> a
    empty :: a

class Element a => SizedElement a where size :: Lens' a Size

bottomLayer :: Element a => Lens.IndexedSetter' Size a Anim.Frame
bottomLayer = setLayers <. (View.layers . Lens.ix 0)

topLayer :: Element a => Lens.IndexedSetter' Size a Anim.Frame
topLayer = setLayers <. (View.layers . Lens.reversed . Lens.ix 0)

instance Element View where
    setLayers f (View sz ls) = Lens.indexed f sz ls <&> View sz
    hoverLayers = setLayers . View.layers %~ (mempty:)
    assymetricPad leftAndTop rightAndBottom x =
        x
        & size +~ leftAndTop + rightAndBottom
        & View.vAnimLayers %~ View.translateLayers leftAndTop
    scale ratio x =
        x
        & size *~ ratio
        & View.animFrames %~ Anim.scale ratio
    empty = View.make 0 mempty

instance SizedElement View where size = View.vSize

tint :: Element a => Draw.Color -> a -> a
tint color = setLayers . View.layers . traverse . Anim.unitImages %~ Draw.tint color

width :: SizedElement a => Lens' a R
width = size . _1

height :: SizedElement a => Lens' a R
height = size . _2

class HasAnimIdPrefix env where animIdPrefix :: Lens' env AnimId
instance HasAnimIdPrefix [SBS.ByteString] where animIdPrefix = id

subAnimId :: (MonadReader env m, HasAnimIdPrefix env) => AnimId -> m AnimId
subAnimId suffix = Lens.view animIdPrefix <&> (++ suffix)

padToSizeAlign :: SizedElement a => Size -> Vector2 R -> a -> a
padToSizeAlign newSize alignment x =
    x
    & setLayers %~ View.translateLayers (sizeDiff * alignment)
    & size %~ liftA2 max newSize
    where
        sizeDiff = max <$> 0 <*> newSize - x ^. size

-- TODO: Remove this
hoverInPlaceOf :: Element a => View -> a -> a
hoverInPlaceOf onTop =
    setLayers . View.layers .~ mempty : onTop ^. View.vAnimLayers . View.layers
