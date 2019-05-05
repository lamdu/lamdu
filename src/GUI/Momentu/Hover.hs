{-# LANGUAGE TemplateHaskell, FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}
{-# LANGUAGE FlexibleContexts, RankNTypes, UndecidableInstances, DerivingVia #-}
module GUI.Momentu.Hover
    ( Style(..), frameColor, framePadding, bgColor, bgPadding
    , Hover, hover, sequenceHover
    , backgroundColor
    , HasStyle(..)
    , AnchoredWidget, anchor
    , hoverInPlaceOf, hoverBesideOptions
    , Ordered(..), forward, backward
    , hoverBesideOptionsAxis
    , Orientation(..)
    , hoverBeside
    , emplaceAt
    ) where

import qualified Control.Lens as Lens
import qualified Data.Aeson.TH.Extended as JsonTH
import           Data.List.Extended (minimumOn)
import           Data.Vector.Vector2 (Vector2(..))
import           GUI.Momentu.Align (Aligned(..), value)
import           GUI.Momentu.Direction (Orientation(..), Order(..), HasLayoutDir)
import qualified GUI.Momentu.Direction as Dir
import qualified GUI.Momentu.Draw as Draw
import           GUI.Momentu.Element (Element, SizedElement)
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.Glue (Glue, GluesTo)
import qualified GUI.Momentu.Glue as Glue
import           GUI.Momentu.Rect (Rect(..))
import           GUI.Momentu.State (Gui)
import qualified GUI.Momentu.State as State
import           GUI.Momentu.View (View)
import qualified GUI.Momentu.View as View
import           GUI.Momentu.Widget (Widget(..), R)
import qualified GUI.Momentu.Widget as Widget

import           Lamdu.Prelude

data Style = Style
    { _frameColor :: Draw.Color
    , _framePadding :: Vector2 R
    , _bgColor :: Draw.Color
    , _bgPadding :: Vector2 R
    } deriving (Eq, Generic, Show)
JsonTH.derivePrefixed "_" ''Style

Lens.makeLenses ''Style

class HasStyle env where style :: Lens' env Style
instance HasStyle Style where style = id

backgroundColor :: HasStyle env => Lens' env Draw.Color
backgroundColor = style . bgColor

data AnchoredWidget a = AnchoredWidget
    { _anchorPoint :: Vector2 R
    , _anchored :: Widget a
    } deriving Functor
Lens.makeLenses ''AnchoredWidget

newtype Hover a = Hover { _unHover :: a }
Lens.makeLenses ''Hover

instance Element a => Element (Hover a) where
    setLayers = unHover . Element.setLayers
    hoverLayers = unHover %~ Element.hoverLayers
    padImpl p0 p1 = unHover %~ Element.padImpl p0 p1
    scale r = unHover %~ Element.scale r
    empty = Hover Element.empty

instance SizedElement a => SizedElement (Hover a) where
    size = unHover . Element.size

instance Widget.HasWidget AnchoredWidget where widget = anchored

instance (Functor f, a ~ f State.Update) => Element (AnchoredWidget a) where
    setLayers = anchored . Element.setLayers
    hoverLayers = anchored %~ Element.hoverLayers
    empty = AnchoredWidget 0 Element.empty
    padImpl tl br (AnchoredWidget point w) =
        AnchoredWidget
        { _anchorPoint = point + tl
        , _anchored = Element.padImpl tl br w
        }
    scale ratio (AnchoredWidget point w) =
        AnchoredWidget
        { _anchorPoint = point * ratio
        , _anchored = Element.scale ratio w
        }

instance (Functor f, a ~ f State.Update) => SizedElement (AnchoredWidget a) where
    size = anchored . Element.size

instance
    ( Functor f, a ~ f State.Update, HasLayoutDir env
    ) => Glue env (AnchoredWidget a) (Hover View) where
    type Glued (AnchoredWidget a) (Hover View) =
        Hover (AnchoredWidget a)
    glue env o ow (Hover ov) =
        Glue.glueH f env o ow ov & Hover
        where
            f w v = w & Element.setLayers <>~ v ^. View.vAnimLayers

instance
    ( Functor f, a ~ f State.Update, HasLayoutDir env
    ) => Glue env (Hover View) (AnchoredWidget a) where
    type Glued (Hover View) (AnchoredWidget a) =
        Hover (AnchoredWidget a)
    glue env o (Hover ov) =
        Glue.glueH f env o ov <&> Hover
        where
            f v w = w & Element.setLayers <>~ v ^. View.vAnimLayers

instance
    ( Applicative f, a ~ b, b ~ f State.Update, Glue.HasTexts env
    ) => Glue env (AnchoredWidget a) (Hover (Widget b)) where
    type Glued (AnchoredWidget a) (Hover (Widget b)) =
        Hover (AnchoredWidget a)
    glue env orientation ow0 (Hover ow1) =
        Glue.glueH f env orientation ow0 ow1 & Hover
        where
            f (AnchoredWidget pos w0) w1 =
                Widget.glueStates env orientation Forward w0 w1
                & AnchoredWidget pos

instance
    ( Applicative f, a ~ b, b ~ f State.Update, Glue.HasTexts env
    ) => Glue env (Hover (Widget a)) (AnchoredWidget b) where
    type Glued (Hover (Widget a)) (AnchoredWidget b) =
        Hover (AnchoredWidget a)
    glue env orientation (Hover ow0) =
        Glue.glueH f env orientation ow0 <&> Hover
        where
            f w0 (AnchoredWidget pos w1) =
                -- The hover is always logically "after" the
                -- lower-layer widgets, no matter if it is glued
                -- before/after geometrically
                Widget.glueStates env orientation Backward w0 w1
                & AnchoredWidget pos

data Ordered a = Ordered
    { _forward :: a
    , _backward :: a
    } deriving stock (Generic, Generic1, Functor, Foldable, Traversable)
    deriving Applicative via Generically1 Ordered
Lens.makeLenses ''Ordered

hoverBesideOptionsAxis ::
    ( MonadReader env m, GluesTo env a b c, HasLayoutDir env
    , SizedElement a, SizedElement b, SizedElement c
    ) =>
    m (Orientation -> Ordered a -> b -> [c])
hoverBesideOptionsAxis =
    Glue.mkPoly <&> \poly o (Ordered fwd bwd) src ->
    do
        let Glue.Poly glue = poly o
        x <- [0, 1]
        let aSrc = Aligned x src
        [ glue aSrc (Aligned x fwd)
            , glue (Aligned x bwd) aSrc]
            <&> (^. value)

anchor ::
    (MonadReader env m, HasLayoutDir env) =>
    m (Widget a -> AnchoredWidget a)
anchor =
    Lens.view Dir.layoutDir
    <&> \dir w -> case dir of
    Dir.LeftToRight -> AnchoredWidget 0 w
    Dir.RightToLeft -> AnchoredWidget (Vector2 (w ^. Widget.wSize . _1) 0) w

hoverBesideOptions ::
    ( MonadReader env m, GluesTo env a b c, HasLayoutDir env
    , SizedElement a, SizedElement b, SizedElement c
    ) =>
    m (a -> b -> [c])
hoverBesideOptions =
    hoverBesideOptionsAxis <&> \doHover h src ->
    do
        o <- [Vertical, Horizontal]
        doHover o (Ordered h h) src

addFrame ::
    (MonadReader env m, HasStyle env, SizedElement a, Element.HasAnimIdPrefix env) =>
    m (a -> a)
addFrame =
    (,) <$> Lens.view style <*> Element.subAnimId
    <&> \(s, subAnimId) gui ->
    if gui ^. Element.size == 0 then gui
    else
    gui
    & Element.padAround (s ^. bgPadding)
    & Draw.backgroundColor (subAnimId ["hover bg"]) (s ^. bgColor)
    & Element.padAround (s ^. framePadding)
    & Draw.backgroundColor (subAnimId ["hover frame"]) (s ^. frameColor)

hover ::
    (MonadReader env m, SizedElement a, HasStyle env, Element.HasAnimIdPrefix env) =>
    m (a -> Hover a)
hover = addFrame <&> ((Hover . Element.hoverLayers) .)

sequenceHover :: Functor f => Hover (f a) -> f (Hover a)
sequenceHover (Hover x) = x <&> Hover

emplaceAt ::
    Functor f =>
    Gui AnchoredWidget f ->
    Gui AnchoredWidget f ->
    Gui Widget f
emplaceAt h place =
    Element.padImpl translation postPad (h ^. anchored)
    where
        postPad =
            place ^. Element.size - h ^. Element.size - translation <&> max 0
        translation = place ^. anchorPoint - h ^. anchorPoint

-- TODO: Second argument here is really only (anchorPoint,size), take
-- it as such?
hoverInPlaceOf ::
    Functor f =>
    [Hover (Gui AnchoredWidget f)] ->
    Gui AnchoredWidget f -> Gui Widget f
hoverInPlaceOf [] _ = error "no hover options!"
hoverInPlaceOf hoverOptions@(Hover defaultOption:_) place
    | null focusedOptions =
        defaultOption `emplaceAt` place
    | otherwise =
        Widget
        { Widget._wSize = place ^. Element.size
        , Widget._wState = Widget.StateFocused makeFocused
        }
    where
        translation h = place ^. anchorPoint - h ^. anchorPoint
        -- All hovers *should* be same the - either focused or unfocused..
        focusedOptions =
            do
                Hover x <- hoverOptions
                mkFocused <- x ^.. anchored . Widget.wState . Widget._StateFocused
                pure (x, mkFocused)
        makeFocused surrounding =
            surrounding
            & Widget.sRight -~ sizeDiff ^. _1
            & Widget.sBottom -~ sizeDiff ^. _2
            & Widget.translateFocused (translation h) hMakeFocused
            & Widget.fFocalAreas %~ (Rect 0 (place ^. Element.size) :)
            where
                (h, hMakeFocused) = pickOption surrounding
                sizeDiff = h ^. Element.size - place ^. Element.size
        pickOption surrounding = minimumOn (negate . remainSurrouding surrounding . (^. _1)) focusedOptions
        remainSurrouding surrounding h =
            filter (>= 0)
            [ surrounding ^. Widget.sLeft - tl ^. _1
            , surrounding ^. Widget.sTop - tl ^. _2
            , surrounding ^. Widget.sRight - br ^. _1
            , surrounding ^. Widget.sBottom - br ^. _2
            ]
            & length
            where
                tl = negate (translation h)
                br = h ^. Element.size - place ^. Element.size - tl

hoverBeside ::
    ( GluesTo env (Hover w) (Gui AnchoredWidget f) (Hover (Gui AnchoredWidget f))
    , MonadReader env m, Functor f, SizedElement w
    , Element.HasAnimIdPrefix env, HasStyle env, HasLayoutDir env
    ) =>
    (forall a b. Lens (t a) (t b) a b) ->
    m
    ( t (Gui Widget f) ->
      w -> t (Gui Widget f)
    )
hoverBeside lens =
    (,,) <$> hoverBesideOptions <*> hover <*> anchor
    <&> \(doHoverBesides, mkHover, anc) layout h ->
    let a = layout & lens %~ anc
    in  a & lens %~
        hoverInPlaceOf
        (doHoverBesides (mkHover h) (a ^. lens))
