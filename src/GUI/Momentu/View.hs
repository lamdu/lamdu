{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
module GUI.Momentu.View
    ( View(..), vSize, vAnimLayers, make
    , animFrames
    , Size, R
    , unitSquare
    ) where

import qualified Control.Lens as Lens
import           GUI.Momentu.Animation (R, Size)
import qualified GUI.Momentu.Animation as Anim
import qualified GUI.Momentu.Direction as Dir
import           GUI.Momentu.Element (Element, SizedElement)
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.Glue (Glue)
import qualified GUI.Momentu.Glue as Glue

import           GUI.Momentu.Prelude

data View = View
    { _vSize :: Size
    , _vAnimLayers :: Element.LayeredImage
    }
Lens.makeLenses ''View

instance Element View where
    setLayeredImage f (View sz ls) = Lens.indexed f sz ls <&> View sz
    hoverLayeredImage = Element.setLayeredImage . Element.layers %~ (mempty:)
    padImpl leftAndTop rightAndBottom x =
        x
        & vSize +~ leftAndTop + rightAndBottom
        & vAnimLayers %~ Element.translateLayeredImage leftAndTop
    scale ratio x =
        x
        & vSize *~ ratio
        & animFrames %~ Anim.scale ratio
    empty = make 0 mempty

instance SizedElement View where size = vSize

instance Has Dir.Layout env => Glue env View View where
    type Glued View View = View
    glue = Glue.glueH $ \v0 v1 -> v0 & vAnimLayers <>~ v1 ^. vAnimLayers

make :: Size -> Anim.Frame -> View
make sz frame = View sz (Element.LayeredImage [frame])

animFrames :: Lens.Traversal' View Anim.Frame
animFrames = vAnimLayers . Element.layers . traverse

unitSquare :: (MonadReader env m, Element.HasAnimIdPrefix env) => m View
unitSquare =
    Lens.view Element.animIdPrefix
    <&> Anim.unitSquare
    <&> make 1
