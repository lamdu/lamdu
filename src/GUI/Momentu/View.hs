{-# LANGUAGE NoImplicitPrelude, TemplateHaskell, FlexibleContexts, FlexibleInstances, OverloadedStrings #-}
module GUI.Momentu.View
    ( View(..), vAnimLayers, make
    , Layers(..), layers, translateLayers, addLayersAbove
        , topLayer, bottomLayer
    , HasSize(..), SetLayers(..), Resizable(..)
    , render
    , animFrames, bottomFrame
    , width, height
    , Size, R
    , tint
    , HasAnimIdPrefix(..), subAnimId
    , addDiagonal, addInnerFrame , backgroundColor
    , hoverInPlaceOf
    , padToSizeAlign
    ) where

import           Control.Applicative (liftA2)
import qualified Control.Lens as Lens
import qualified Data.ByteString as SBS
import           Data.Vector.Vector2 (Vector2(..))
import qualified Graphics.DrawingCombinators as Draw
import           GUI.Momentu.Animation (AnimId, R)
import qualified GUI.Momentu.Animation as Anim

import           Lamdu.Prelude

type Size = Anim.Size

-- | Layers is a list of animation frames that overlay on top of each
-- other (first element is most obscured one). When composing Views,
-- the layers at the same list index are composed together and all
-- obscure the layers from a lower index.
newtype Layers = Layers { _layers :: [Anim.Frame] }
Lens.makeLenses ''Layers

instance Monoid Layers where
    mempty = Layers []
    mappend xs (Layers []) = xs
    mappend (Layers []) ys = ys
    mappend (Layers (x:xs)) (Layers (y:ys)) =
        Layers (x<>y : rest ^. layers)
        where
            rest = Layers xs <> Layers ys

addLayersAbove :: Layers -> Layers -> Layers
addLayersAbove (Layers xs) (Layers ys) = Layers (ys ++ xs)

bottomLayer :: Lens.Setter' Layers Anim.Frame
bottomLayer = layers . Lens.ix 0

topLayer :: Lens.Setter' Layers Anim.Frame
topLayer = layers . Lens.reversed . Lens.ix 0

data View = View
    { _vSize :: Size
    , _vAnimLayers :: Layers
    }
Lens.makeLenses ''View

class SetLayers a where
    setLayers :: Lens.IndexedSetter' Size a Layers
    hoverLayers :: a -> a

class SetLayers a => Resizable a where
    -- Different `SetLayers`s do additional things when padding
    -- (Moving focal points, alignments, etc)
    pad :: Vector2 R -> a -> a
    pad p = assymetricPad p p
    assymetricPad :: Vector2 R -> Vector2 R -> a -> a
    scale :: Vector2 R -> a -> a
    empty :: a

class HasSize a where size :: Lens' a Size

instance SetLayers View where
    setLayers f (View sz ls) = Lens.indexed f sz ls <&> View sz
    hoverLayers = setLayers . layers %~ (mempty:)

instance Resizable View where
    assymetricPad leftAndTop rightAndBottom x =
        x
        & size +~ leftAndTop + rightAndBottom
        & vAnimLayers %~ translateLayers leftAndTop
    scale ratio x =
        x
        & size *~ ratio
        & animFrames %~ Anim.scale ratio
    empty = make 0 mempty

instance HasSize View where size = vSize

make :: Size -> Anim.Frame -> View
make sz frame = View sz (Layers [frame])

render :: Layers -> Anim.Frame
render x = x ^. layers . Lens.reversed . traverse

animFrames :: Lens.Traversal' View Anim.Frame
animFrames = vAnimLayers . layers . traverse

width :: HasSize a => Lens' a R
width = size . _1

height :: HasSize a => Lens' a R
height = size . _2

tint :: SetLayers a => Draw.Color -> a -> a
tint color = setLayers . layers . traverse . Anim.unitImages %~ Draw.tint color

bottomFrame :: SetLayers a => Lens.Setter' a Anim.Frame
bottomFrame = setLayers . bottomLayer

class HasAnimIdPrefix env where animIdPrefix :: Lens' env AnimId
instance HasAnimIdPrefix [SBS.ByteString] where animIdPrefix = id

subAnimId :: (MonadReader env m, HasAnimIdPrefix env) => AnimId -> m AnimId
subAnimId suffix = Lens.view animIdPrefix <&> (++ suffix)

backgroundColor ::
    (MonadReader env m, HasAnimIdPrefix env, SetLayers a) =>
    m (Draw.Color -> a -> a)
backgroundColor =
    subAnimId ["bg"] <&>
    \animId color -> setLayers %@~ \sz x ->
    x
    & layers %~ addBg (Anim.backgroundColor animId color sz)
    where
        addBg bg [] = [bg]
        addBg bg (x:xs) = x <> bg : xs

-- | Add a diagonal line (top-left to right-bottom). Useful as a
-- "deletion" GUI annotation
addDiagonal ::
    (MonadReader env m, HasAnimIdPrefix env, SetLayers a) =>
    m (Draw.Color -> R -> a -> a)
addDiagonal =
    subAnimId ["diagonal"] <&>
    \animId color thickness -> setLayers %@~
    \sz -> topLayer <>~
    ( Draw.convexPoly
        [ (0, thickness)
        , (0, 0)
        , (thickness, 0)
        , (1, 1-thickness)
        , (1, 1)
        , (1-thickness, 1)
        ]
        & Draw.tint color
        & void
        & Anim.simpleFrame (animId ++ ["diagonal"])
        & Anim.scale sz
    )

addInnerFrame ::
    (MonadReader env m, HasAnimIdPrefix env, SetLayers a) =>
    m (Draw.Color -> Vector2 R -> a -> a)
addInnerFrame =
    subAnimId ["inner-frame"] <&>
    \animId color frameWidth -> setLayers %@~ \sz ->
    layers . Lens.ix 0 %~
        mappend
        ( Anim.emptyRectangle frameWidth sz animId
            & Anim.unitImages %~ Draw.tint color
        )

translateLayers :: Vector2 R -> Layers -> Layers
translateLayers pos = layers . traverse %~ Anim.translate pos

padToSizeAlign :: (HasSize a, SetLayers a) => Size -> Vector2 R -> a -> a
padToSizeAlign newSize alignment x =
    x
    & setLayers %~ translateLayers (sizeDiff * alignment)
    & size %~ liftA2 max newSize
    where
        sizeDiff = max <$> 0 <*> newSize - x ^. size

hoverInPlaceOf :: SetLayers a => View -> a -> a
hoverInPlaceOf onTop =
    setLayers . layers .~ mempty : onTop ^. vAnimLayers . layers
