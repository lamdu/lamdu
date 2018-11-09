{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, ConstraintKinds #-}
module GUI.Momentu.Glue
    ( Glue(..), GluesTo
    , (/|/), (/-/)
    , Orientation(..)
    , box, hbox, vbox
    , glueH
    ) where

import           Data.Vector.Vector2 (Vector2(..))
import           GUI.Momentu.Element (Element, SizedElement)
import qualified GUI.Momentu.Element as Element

import           Lamdu.Prelude

data Orientation = Horizontal | Vertical
    deriving (Eq, Show, Ord, Generic)

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
    f (v0 & Element.size .~ newSize)
    ( Element.assymetricPad t 0 v1
    & Element.size .~ newSize
    )
    where
        Vector2 w0 h0 = v0 ^. Element.size
        Vector2 w1 h1 = v1 ^. Element.size
        (newSize, t) =
            case orientation of
            Horizontal -> (Vector2 (w0 + w1) (max h0 h1), Vector2 w0 0)
            Vertical -> (Vector2 (max w0 w1) (h0 + h1), Vector2 0 h0)

box :: (Element a, GluesTo a a a) => Orientation -> [a] -> a
box orientation = foldr (glue orientation) Element.empty

hbox :: (Element a, GluesTo a a a) => [a] -> a
hbox = box Horizontal

vbox :: (Element a, GluesTo a a a) => [a] -> a
vbox = box Vertical
