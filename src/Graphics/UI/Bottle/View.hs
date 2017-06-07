{-# LANGUAGE NoImplicitPrelude, RecordWildCards, RankNTypes, OverloadedStrings, TemplateHaskell, TypeSynonymInstances, FlexibleInstances #-}
module Graphics.UI.Bottle.View
    ( View(..), make
    , empty
    , Layers(..), layers
    , pad, assymetricPad, translate, scale
    , HasView(..), MkView(..)
    , size, animLayers
    , render
    , animFrames, bottomFrame
    , width, height
    , Size, R
    , tint
    , HasAnimIdPrefix(..), subAnimId
    , addDiagonal, addInnerFrame , backgroundColor
    ) where

import qualified Control.Lens as Lens
import           Data.Vector.Vector2 (Vector2(..))
import qualified Graphics.DrawingCombinators as Draw
import           Graphics.UI.Bottle.Animation (AnimId, R)
import qualified Graphics.UI.Bottle.Animation as Anim

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

data View = View
    { _vSize :: Size
    , _vAnimLayers :: Layers
    }
Lens.makeLenses ''View

class MkView a where setView :: Lens.Setter' a View
instance MkView View where setView = id

class MkView a => HasView a where view :: Lens' a View
instance HasView View where view = id

size :: HasView a => Lens' a Size
size = view . vSize

animLayers :: HasView a => Lens' a Layers
animLayers = view . vAnimLayers

make :: Size -> Anim.Frame -> View
make sz frame = View sz (Layers [frame])

render :: HasView a => a -> Anim.Frame
render x = x ^. animLayers . layers . Lens.reversed . traverse

animFrames :: HasView a => Lens.Traversal' a Anim.Frame
animFrames = animLayers . layers . traverse

empty :: View
empty = make 0 mempty

width :: HasView a => Lens' a R
width = view . size . _1

height :: HasView a => Lens' a R
height = view . size . _2

tint :: HasView a => Draw.Color -> a -> a
tint color = animFrames . Anim.unitImages %~ Draw.tint color

bottomFrame :: MkView a => Lens.Setter' a Anim.Frame
bottomFrame = setView . animLayers . layers . Lens.ix 0

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
    & animLayers . layers %~ addBg (Anim.backgroundColor animId color (x ^. size))
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
    & animLayers . layers . Lens.reversed . Lens.ix 0 <>~
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

-- `scale`, `pad`, `translate`, and `assymetricPad` are not lifted to any `HasView` instance,
-- because all their uses in `Widget`s require additional changes in the widget.

scale :: Vector2 Draw.R -> View -> View
scale ratio x =
    x
    & size *~ ratio
    & animFrames %~ Anim.scale ratio

pad :: Vector2 R -> View -> View
pad p = assymetricPad p p

translate :: Vector2 R -> View -> View
translate pos = animFrames %~ Anim.translate pos

assymetricPad :: Vector2 R -> Vector2 R -> View -> View
assymetricPad leftAndTop rightAndBottom x =
    x
    & size +~ leftAndTop + rightAndBottom
    & translate leftAndTop
