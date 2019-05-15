{-# LANGUAGE TemplateHaskell, TypeFamilies, MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds, RankNTypes, DerivingVia #-}
module GUI.Momentu.Glue
    ( Texts(..), stroll, back, ahead
        , strollDoc
    , HasTexts(..)
    , Glue(..), GluesTo
    , (/|/), (/-/)
    , box, hbox, vbox
    , glueH
    , mkGlue
    , Orientation(..)
    , Poly(..), mkPoly
    ) where

import qualified Control.Lens as Lens
import qualified Data.Aeson.TH.Extended as JsonTH
import           Data.Vector.Vector2 (Vector2(..))
import           GUI.Momentu.Direction (Orientation(..), axis, perpendicular)
import qualified GUI.Momentu.Direction as Dir
import           GUI.Momentu.Element (Element, SizedElement)
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.EventMap as EventMap

import           Lamdu.Prelude

data Texts a = Texts
    { _stroll :: a
    , _back :: a
    , _ahead :: a
    }
    deriving stock (Generic, Generic1, Eq, Ord, Show, Functor, Foldable, Traversable)
    deriving Applicative via (Generically1 Texts)

Lens.makeLenses ''Texts

class
    ( Has (Dir.Texts Text) env, Has Dir.Layout env
    ) => HasTexts env where
    texts :: Lens' env (Texts Text)
JsonTH.derivePrefixed "_" ''Texts

strollDoc :: HasTexts env => env -> Lens.ALens' (Texts Text) Text -> EventMap.Doc
strollDoc env dirLens =
    EventMap.Doc
    [ env ^. has . Dir.navigation
    , env ^# texts . stroll
    , env ^# texts . dirLens
    ]

class (Glued b a ~ Glued a b) => Glue env a b where
    type Glued a b
    glue :: env -> Orientation -> a -> b -> Glued a b

type GluesTo env a b c = (Glue env a b, Glue env b a, Glued a b ~ c)

newtype Poly env = Poly { polyGlue :: forall a b. Glue env a b => a -> b -> Glued a b }

mkPoly :: MonadReader env m => m (Orientation -> Poly env)
mkPoly = Lens.view id <&> \env orientation -> Poly (glue env orientation)

mkGlue ::
    (MonadReader env m, Glue env a b) =>
    m (Orientation -> a -> b -> Glued a b)
mkGlue = Lens.view id <&> glue

-- Horizontal glue
(/|/) ::
    (MonadReader env m, Glue env a b) =>
    m a -> m b -> m (Glued a b)
l /|/ r = (mkGlue ?? Horizontal) <*> l <*> r

-- Vertical glue
(/-/) ::
    (MonadReader env m, Glue env a b) =>
    m a -> m b -> m (Glued a b)
l /-/ r = (mkGlue ?? Vertical) <*> l <*> r

glueH ::
    (SizedElement a, SizedElement b, Has Dir.Layout env) =>
    (a -> b -> c) -> env -> Orientation -> a -> b -> c
glueH f direction orientation v0 v1 =
    f
    (Element.pad direction v0pre v0post v0)
    (Element.pad direction v1pre v1post v1)
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

box ::
    (Element a, GluesTo env a a a, MonadReader env m) =>
    m (Orientation -> [a] -> a)
box = mkGlue <&> \g orientation -> foldr (g orientation) Element.empty

hbox ::
    (Element a, GluesTo env a a a, MonadReader env m) =>
    m ([a] -> a)
hbox = box ?? Horizontal

vbox ::
    (Element a, GluesTo env a a a, MonadReader env m) =>
    m ([a] -> a)
vbox = box ?? Vertical
