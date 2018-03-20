{-# LANGUAGE TemplateHaskell, FlexibleContexts, FlexibleInstances #-}
module GUI.Momentu.Element
    ( Element(..), SizedElement(..), Size
    , HasAnimIdPrefix(..), subAnimId
    , Layers(..), layers, translateLayers, addLayersAbove, render
    , topLayer, bottomLayer
    , width, height
    , tint
    , padToSizeAlign
    ) where

import           Control.Applicative (liftA2)
import qualified Control.Lens as Lens
import           Data.Vector.Vector2 (Vector2(..))
import           GUI.Momentu.Animation (AnimId, R, Size)
import qualified GUI.Momentu.Animation as Anim
import qualified Graphics.DrawingCombinators as Draw

import           Lamdu.Prelude

-- | Layers is a list of animation frames that overlay on top of each
-- other (first element is most obscured one). When composing Views,
-- the layers at the same list index are composed together and all
-- obscure the layers from a lower index.
newtype Layers = Layers { _layers :: [Anim.Frame] }
Lens.makeLenses ''Layers

instance Semigroup Layers where
    xs <> Layers [] = xs
    Layers [] <> ys = ys
    Layers (x:xs) <> Layers (y:ys) =
        Layers (x<>y : rest ^. layers)
        where
            rest = Layers xs <> Layers ys

instance Monoid Layers where
    mempty = Layers []
    mappend = (<>)

addLayersAbove :: Layers -> Layers -> Layers
addLayersAbove (Layers xs) (Layers ys) = Layers (ys ++ xs)

translateLayers :: Vector2 R -> Layers -> Layers
translateLayers pos = layers . traverse %~ Anim.translate pos

render :: Layers -> Anim.Frame
render x = x ^. layers . Lens.reversed . traverse

class Element a where
    setLayers :: Lens.IndexedSetter' Size a Layers
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
bottomLayer = setLayers <. (layers . Lens.ix 0)

topLayer :: Element a => Lens.IndexedSetter' Size a Anim.Frame
topLayer = setLayers <. (layers . Lens.reversed . Lens.ix 0)

tint :: Element a => Draw.Color -> a -> a
tint color = setLayers . layers . traverse . Anim.unitImages %~ Draw.tint color

width :: SizedElement a => Lens' a R
width = size . _1

height :: SizedElement a => Lens' a R
height = size . _2

class HasAnimIdPrefix env where animIdPrefix :: Lens' env AnimId
instance HasAnimIdPrefix [ByteString] where animIdPrefix = id

subAnimId :: (MonadReader env m, HasAnimIdPrefix env) => AnimId -> m AnimId
subAnimId suffix = Lens.view animIdPrefix <&> (++ suffix)

padToSizeAlign :: SizedElement a => Size -> Vector2 R -> a -> a
padToSizeAlign newSize alignment x =
    x
    & setLayers %~ translateLayers (sizeDiff * alignment)
    & size %~ liftA2 max newSize
    where
        sizeDiff = max <$> 0 <*> newSize - x ^. size
