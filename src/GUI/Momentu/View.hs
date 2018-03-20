{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, TypeFamilies #-}
module GUI.Momentu.View
    ( View(..), vSize, vAnimLayers, make
    , animFrames
    , Size, R
    , unitSquare
    ) where

import qualified Control.Lens as Lens
import           GUI.Momentu.Animation (R, Size)
import qualified GUI.Momentu.Animation as Anim
import           GUI.Momentu.Element (Element, SizedElement)
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.Glue (Glue)
import qualified GUI.Momentu.Glue as Glue

import           Lamdu.Prelude

data View = View
    { _vSize :: Size
    , _vAnimLayers :: Element.Layers
    }
Lens.makeLenses ''View

instance Element View where
    setLayers f (View sz ls) = Lens.indexed f sz ls <&> View sz
    hoverLayers = Element.setLayers . Element.layers %~ (mempty:)
    assymetricPad leftAndTop rightAndBottom x =
        x
        & Element.size +~ leftAndTop + rightAndBottom
        & vAnimLayers %~ Element.translateLayers leftAndTop
    scale ratio x =
        x
        & Element.size *~ ratio
        & animFrames %~ Anim.scale ratio
    empty = make 0 mempty

instance SizedElement View where size = vSize

instance Glue View View where
    type Glued View View = View
    glue = Glue.glueH $ \v0 v1 -> v0 & vAnimLayers <>~ v1 ^. vAnimLayers

make :: Size -> Anim.Frame -> View
make sz frame = View sz (Element.Layers [frame])

animFrames :: Lens.Traversal' View Anim.Frame
animFrames = vAnimLayers . Element.layers . traverse

unitSquare :: (MonadReader env m, Element.HasAnimIdPrefix env) => m View
unitSquare =
    Lens.view Element.animIdPrefix
    <&> Anim.unitSquare
    <&> make 1
