{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, ConstraintKinds #-}
module GUI.Momentu.Glue
    ( Glue(..), GluesTo
    , (/|/), (/-/)
    , box, hbox, vbox
    , glueH
    ) where

import           Data.Vector.Vector2 (Vector2(..))
import           GUI.Momentu.Direction (Orientation(..), axis, perpendicular)
import           GUI.Momentu.Element (Element, SizedElement)
import qualified GUI.Momentu.Element as Element

import           Lamdu.Prelude

class (Glued b a ~ Glued a b) => Glue a b where
    type Glued a b
    glue :: Orientation -> a -> b -> Glued a b

type GluesTo a b c = (Glue a b, Glue b a, Glued a b ~ c)

-- Horizontal glue
(/|/) :: Glue a b => a -> b -> Glued a b
(/|/) = glue Horizontal

-- Vertical glue
(/-/) :: Glue a b => a -> b -> Glued a b
(/-/) = glue Vertical

glueH ::
    (SizedElement a, SizedElement b) =>
    (a -> b -> c) -> Orientation -> a -> b -> c
glueH f orientation v0 v1 =
    f
    (Element.pad v0pre v0post v0)
    (Element.pad v1pre v1post v1)
    where
        v0pre = 0
        v0post = v1s & perp -~ v0s ^. perp & perp %~ max 0
        v1pre = v0s & perp .~ 0
        v1post = v0s & ax .~ 0 & perp -~ v1s ^. perp & perp %~ max 0
        ax = axis orientation
        perp :: Lens' (Vector2 a) a
        perp = axis (perpendicular orientation)
        v0s = v0 ^. Element.size
        v1s = v1 ^. Element.size

box :: (Element a, GluesTo a a a) => Orientation -> [a] -> a
box orientation = foldr (glue orientation) Element.empty

hbox :: (Element a, GluesTo a a a) => [a] -> a
hbox = box Horizontal

vbox :: (Element a, GluesTo a a a) => [a] -> a
vbox = box Vertical
