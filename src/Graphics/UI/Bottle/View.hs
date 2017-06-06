{-# LANGUAGE NoImplicitPrelude, RecordWildCards, RankNTypes, OverloadedStrings, TemplateHaskell #-}
module Graphics.UI.Bottle.View
    ( View(..), make
    , empty
    , size, animLayers
    , Layers(..), layers
    , render
    , animFrames, bottomFrame
    , width, height
    , pad, assymetricPad
    , Size, R
    , addDiagonal, addInnerFrame
    , backgroundColor
    , translate, scale, tint
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
    { _size :: Size
    , _animLayers :: Layers
    }
Lens.makeLenses ''View

make :: Size -> Anim.Frame -> View
make sz frame = View sz (Layers [frame])

render :: View -> Anim.Frame
render view = view ^. animLayers . layers . Lens.reversed . traverse

animFrames :: Lens.Traversal' View Anim.Frame
animFrames = animLayers . layers . traverse

empty :: View
empty = make 0 mempty

width :: Lens' View R
width = size . _1

height :: Lens' View R
height = size . _2

-- | Add a diagonal line (top-left to right-bottom). Useful as a
-- "deletion" GUI annotation
addDiagonal :: R -> AnimId -> Draw.Color -> View -> View
addDiagonal thickness animId color view=
    view & animLayers . layers . Lens.reversed . Lens.ix 0 <>~ line
    where
        line =
            Draw.convexPoly
            [ (0, thickness)
            , (0, 0)
            , (thickness, 0)
            , (1, 1-thickness)
            , (1, 1)
            , (1-thickness, 1)
            ]
            & Draw.tint color
            & void
            & Anim.simpleFrame animId
            & Anim.scale (view ^. size)

backgroundColor :: AnimId -> Draw.Color -> View -> View
backgroundColor animId color view =
    view
    & animLayers . layers %~ addBg
    where
        bgAnimId = animId ++ ["bg"]
        bg = Anim.backgroundColor bgAnimId color (view ^. size)
        addBg [] = [bg]
        addBg (x:xs) = x <> bg : xs

scale :: Vector2 Draw.R -> View -> View
scale ratio view =
    view
    & size *~ ratio
    & animFrames %~ Anim.scale ratio

pad :: Vector2 R -> View -> View
pad p = assymetricPad p p

translate :: Vector2 R -> View -> View
translate pos = animFrames %~ Anim.translate pos

assymetricPad :: Vector2 R -> Vector2 R -> View -> View
assymetricPad leftAndTop rightAndBottom view =
    view
    & size +~ leftAndTop + rightAndBottom
    & translate leftAndTop

tint :: Draw.Color -> View -> View
tint color = animFrames . Anim.unitImages %~ Draw.tint color

bottomFrame :: Lens.Traversal' View Anim.Frame
bottomFrame = animLayers . layers . Lens.ix 0

addInnerFrame :: AnimId -> Draw.Color -> Vector2 R -> View -> View
addInnerFrame animId color frameWidth view =
    view & bottomFrame %~ mappend emptyRectangle
    where
        emptyRectangle =
            Anim.emptyRectangle frameWidth (view ^. size) animId
            & Anim.unitImages %~ Draw.tint color
