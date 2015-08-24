{-# LANGUAGE NoImplicitPrelude, RecordWildCards, RankNTypes, OverloadedStrings #-}
module Graphics.UI.Bottle.View
    ( View(..)
    , empty
    , size, animFrame
    , width, height
    , pad, assymetricPad
    , Size, R
    , augmentAnimId, backgroundColor
    , translate, scale, tint
    ) where

import           Prelude.Compat

import           Control.Lens (Lens')
import           Control.Lens.Operators
import           Control.Lens.Tuple
import qualified Data.ByteString.Char8 as SBS8
import           Data.Vector.Vector2 (Vector2(..))
import qualified Graphics.DrawingCombinators as Draw
import           Graphics.UI.Bottle.Animation (AnimId, Layer, R)
import qualified Graphics.UI.Bottle.Animation as Anim

type Size = Anim.Size

data View = View
    { _size :: Size
    , _animFrame :: Anim.Frame
    }

empty :: View
empty = View 0 mempty

size :: Lens' View Size
size f View{..} = f _size <&> \_size -> View{..}

animFrame :: Lens' View Anim.Frame
animFrame f View{..} = f _animFrame <&> \_animFrame -> View{..}

width :: Lens' View R
width = size . _1

height :: Lens' View R
height = size . _2

augmentAnimId :: Show a => AnimId -> a -> AnimId
augmentAnimId animId = Anim.joinId animId . (:[]) . SBS8.pack . show

backgroundColor :: AnimId -> Layer -> Draw.Color -> View -> View
backgroundColor animId layer color view =
    view
    & animFrame %~ Anim.backgroundColor bgAnimId layer color (view ^. size)
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
