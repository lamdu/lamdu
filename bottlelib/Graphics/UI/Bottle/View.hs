{-# LANGUAGE NoImplicitPrelude, RecordWildCards, RankNTypes, OverloadedStrings, TemplateHaskell #-}
module Graphics.UI.Bottle.View
    ( View(..)
    , empty
    , size, animFrame
    , width, height
    , pad, assymetricPad
    , Size, R
    , addDiagonal
    , backgroundColor
    , translate, scale, tint
    ) where

import           Control.Lens (Lens')
import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Lens.Tuple
import           Control.Monad (void)
import           Data.Vector.Vector2 (Vector2(..))
import qualified Graphics.DrawingCombinators as Draw
import           Graphics.UI.Bottle.Animation (AnimId, Layer, R)
import qualified Graphics.UI.Bottle.Animation as Anim

import           Prelude.Compat

type Size = Anim.Size

data View = View
    { _size :: Size
    , _animFrame :: Anim.Frame
    }
Lens.makeLenses ''View

empty :: View
empty = View 0 mempty

width :: Lens' View R
width = size . _1

height :: Lens' View R
height = size . _2

-- | Add a diagonal line (top-left to right-bottom). Useful as a
-- "deletion" GUI annotation
addDiagonal :: R -> AnimId -> Layer -> Draw.Color -> View -> View
addDiagonal thickness animId layer color view=
    view & animFrame <>~ line
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
            & Anim.layers +~ layer
            & Anim.scale (view ^. size)

backgroundColor :: Layer -> AnimId -> Draw.Color -> View -> View
backgroundColor layer animId color view =
    view
    & animFrame <>~ Anim.backgroundColor bgAnimId layer color (view ^. size)
    where
        bgAnimId = animId ++ ["bg"]

scale :: Vector2 Draw.R -> View -> View
scale ratio (View sz frm) =
    View (sz*ratio) (Anim.scale ratio frm)

pad :: Vector2 R -> View -> View
pad p = assymetricPad p p

translate :: Vector2 R -> View -> View
translate pos = animFrame %~ Anim.translate pos

assymetricPad :: Vector2 R -> Vector2 R -> View -> View
assymetricPad leftAndTop rightAndBottom view =
    view
    & size +~ leftAndTop + rightAndBottom
    & translate leftAndTop

tint :: Draw.Color -> View -> View
tint color = animFrame . Anim.unitImages %~ Draw.tint color
