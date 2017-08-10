{-# LANGUAGE NoImplicitPrelude, TemplateHaskell #-}
module GUI.Momentu.View
    ( View(..), vSize, vAnimLayers, make
    , Layers(..), layers, translateLayers, addLayersAbove
    , render
    , animFrames
    , Size, R
    ) where

import qualified Control.Lens as Lens
import           Data.Vector.Vector2 (Vector2(..))
import qualified Graphics.DrawingCombinators as Draw
import           GUI.Momentu.Animation (R, Size)
import qualified GUI.Momentu.Animation as Anim

import           Lamdu.Prelude

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

data View = View
    { _vSize :: Size
    , _vAnimLayers :: Layers
    }
Lens.makeLenses ''View

make :: Size -> Anim.Frame -> View
make sz frame = View sz (Layers [frame])

render :: Layers -> Anim.Frame
render x = x ^. layers . Lens.reversed . traverse

animFrames :: Lens.Traversal' View Anim.Frame
animFrames = vAnimLayers . layers . traverse

translateLayers :: Vector2 R -> Layers -> Layers
translateLayers pos = layers . traverse %~ Anim.translate pos
