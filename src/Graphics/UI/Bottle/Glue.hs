{-# LANGUAGE NoImplicitPrelude, TypeFamilies, MultiParamTypeClasses, ConstraintKinds #-}
module Graphics.UI.Bottle.Glue
    ( Glue(..), GluesTo
    , (/|/), (/-/)
    , Orientation(..)
    , box, hbox, vbox
    , glueH
    ) where

import           Data.Vector.Vector2 (Vector2(..))
import           Graphics.UI.Bottle.View (View)
import qualified Graphics.UI.Bottle.View as View

import           Lamdu.Prelude

data Orientation = Horizontal | Vertical
    deriving (Eq, Show, Ord)

class Glue a b where
    type Glued a b
    glue :: Orientation -> a -> b -> Glued a b

type GluesTo a b c = (Glue a b, Glued a b ~ c)

instance Glue View View where
    type Glued View View = View
    glue = glueH $ \v0 v1 -> v0 & View.vAnimLayers <>~ v1 ^. View.vAnimLayers

-- Horizontal glue
(/|/) :: Glue a b => a -> b -> Glued a b
(/|/) = glue Horizontal

-- Vertical glue
(/-/) :: Glue a b => a -> b -> Glued a b
(/-/) = glue Vertical

glueH ::
    (View.HasSize a, View.HasSize b, View.Resizable b) =>
    (a -> b -> Glued a b) -> Orientation -> a -> b -> Glued a b
glueH f orientation v0 v1 =
    f (v0 & View.size .~ newSize) (View.assymetricPad t 0 v1 & View.size .~ newSize)
    where
        Vector2 w0 h0 = v0 ^. View.size
        Vector2 w1 h1 = v1 ^. View.size
        (newSize, t) =
            case orientation of
            Horizontal -> (Vector2 (w0 + w1) (max h0 h1), Vector2 w0 0)
            Vertical -> (Vector2 (max w0 w1) (h0 + h1), Vector2 0 h0)

box :: (View.Resizable a, GluesTo a a a) => Orientation -> [a] -> a
box orientation = foldr (glue orientation) View.empty

hbox :: (View.Resizable a, GluesTo a a a) => [a] -> a
hbox = box Horizontal

vbox :: (View.Resizable a, GluesTo a a a) => [a] -> a
vbox = box Vertical
