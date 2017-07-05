{-# LANGUAGE NoImplicitPrelude, RecordWildCards, RankNTypes, OverloadedStrings, TemplateHaskell, TypeSynonymInstances, FlexibleInstances #-}
module Graphics.UI.Bottle.View
    ( View(..), vMakeLayers, make
    , empty
    , Surrounding, sLeft, sTop, sRight, sBottom
    , Layers(..), layers
    , translateMakeLayers, addLayersAbove
    , assymetricPad, scale
    , HasSize(..), MkView(..), Pad(..)
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
import           Data.Vector.Vector2 (Vector2(..))
import qualified Graphics.DrawingCombinators as Draw
import           Graphics.UI.Bottle.Animation (AnimId, R)
import qualified Graphics.UI.Bottle.Animation as Anim

import           Lamdu.Prelude

type Size = Anim.Size

-- Area on screen around a view.
-- Used for positioning of tooltips and hovers around Views.
data Surrounding = Surrounding
    { _sLeft :: !R
    , _sTop :: !R
    , _sRight :: !R
    , _sBottom :: !R
    } deriving (Eq, Ord, Show)
Lens.makeLenses ''Surrounding

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

addLayersAbove :: (Surrounding -> Layers) -> (Surrounding -> Layers) -> Surrounding -> Layers
addLayersAbove x y =
    f <$> x <*> y
    where
        f (Layers xs) (Layers ys) = Layers (ys ++ xs)

data View = View
    { _vSize :: Size
    , _vMakeLayers :: Surrounding -> Layers
    }
Lens.makeLenses ''View

class MkView a where
    setView :: Lens.Setter' a View

class MkView a => Pad a where
    -- Different `MkView`s do additional things when padding
    -- (Moving focal points, alignments, etc)
    pad :: Vector2 R -> a -> a

instance MkView View where setView = id
instance Pad View where pad p = assymetricPad p p

class HasSize a where size :: Lens' a Size
instance HasSize View where size = vSize

make :: Size -> Anim.Frame -> View
make sz frame = View sz (\_surrounding -> Layers [frame])

render :: View -> Anim.Frame
render x = (x ^. vMakeLayers) (Surrounding 0 0 0 0) ^. layers . Lens.reversed . traverse

animFrames :: Lens.Setter' View Anim.Frame
animFrames = vMakeLayers . Lens.mapped . layers . traverse

empty :: View
empty = make 0 mempty

width :: HasSize a => Lens' a R
width = size . _1

height :: HasSize a => Lens' a R
height = size . _2

tint :: MkView a => Draw.Color -> a -> a
tint color = setView . animFrames . Anim.unitImages %~ Draw.tint color

bottomFrame :: MkView a => Lens.Setter' a Anim.Frame
bottomFrame = setView . vMakeLayers . Lens.mapped . layers . Lens.ix 0

class HasAnimIdPrefix env where animIdPrefix :: Lens' env AnimId
instance HasAnimIdPrefix AnimId where animIdPrefix = id

subAnimId :: (MonadReader env m, HasAnimIdPrefix env) => AnimId -> m AnimId
subAnimId suffix = Lens.view animIdPrefix <&> (++ suffix)

backgroundColor ::
    (MonadReader env m, HasAnimIdPrefix env, MkView a) =>
    m (Draw.Color -> a -> a)
backgroundColor =
    subAnimId ["bg"] <&>
    \animId color -> setView %~ \x ->
    x
    & vMakeLayers . Lens.mapped . layers %~ addBg (Anim.backgroundColor animId color (x ^. size))
    where
        addBg bg [] = [bg]
        addBg bg (x:xs) = x <> bg : xs

-- | Add a diagonal line (top-left to right-bottom). Useful as a
-- "deletion" GUI annotation
addDiagonal ::
    (MonadReader env m, HasAnimIdPrefix env, MkView a) =>
    m (Draw.Color -> R -> a -> a)
addDiagonal =
    subAnimId ["diagonal"] <&>
    \animId color thickness -> setView %~ \x ->
    x
    & vMakeLayers . Lens.mapped . layers . Lens.reversed . Lens.ix 0 <>~
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
        & Anim.scale (x ^. size)
    )

addInnerFrame ::
    (MonadReader env m, HasAnimIdPrefix env, MkView a) =>
    m (Draw.Color -> Vector2 R -> a -> a)
addInnerFrame =
    subAnimId ["inner-frame"] <&>
    \animId color frameWidth -> setView %~ \x ->
    x & bottomFrame %~
        mappend
        ( Anim.emptyRectangle frameWidth (x ^. size) animId
            & Anim.unitImages %~ Draw.tint color
        )

scale :: Vector2 Draw.R -> View -> View
scale ratio v =
    v
    & size *~ ratio
    & vMakeLayers . Lens.argument . sRight -~ (v ^. width) * (ratio ^. _1 - 1)
    & vMakeLayers . Lens.argument . sBottom -~ (v ^. height) * (ratio ^. _2 - 1)
    & animFrames %~ Anim.scale ratio

translateMakeLayers :: Vector2 R -> (Surrounding -> Layers) -> Surrounding -> Layers
translateMakeLayers pos mkLayers surrounding =
    surrounding
    & sLeft +~ pos ^. _1
    & sTop +~ pos ^. _2
    & sRight -~ pos ^. _1
    & sBottom -~ pos ^. _2
    & mkLayers
    & layers . traverse %~ Anim.translate pos

assymetricPad :: Vector2 R -> Vector2 R -> View -> View
assymetricPad leftAndTop rightAndBottom x =
    x
    & size +~ leftAndTop + rightAndBottom
    & vMakeLayers %~ translateMakeLayers leftAndTop

padToSizeAlign :: Size -> Vector2 R -> View -> View
padToSizeAlign newSize alignment x =
    x
    & vMakeLayers %~ translateMakeLayers (sizeDiff * alignment)
    & size %~ liftA2 max newSize
    where
        sizeDiff = max <$> 0 <*> newSize - x ^. size

hoverInPlaceOf :: MkView a => View -> a -> a
hoverInPlaceOf onTop =
    setView . vMakeLayers .~ (onTop ^. vMakeLayers <&> layers %~ (mempty :))
