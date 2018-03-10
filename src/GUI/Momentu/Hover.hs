{-# LANGUAGE NoImplicitPrelude, DeriveGeneric, TemplateHaskell, DeriveTraversable, FlexibleInstances, MultiParamTypeClasses, TypeFamilies, FlexibleContexts, OverloadedStrings, RankNTypes #-}
module GUI.Momentu.Hover
    ( Style(..)
    , Hover, hover, sequenceHover
    , backgroundColor
    , HasStyle(..)
    , AnchoredWidget, anchor
    , hoverInPlaceOf, hoverBesideOptions
    , Ordered(..), forward, backward
    , hoverBesideOptionsAxis
    , Orientation(..)
    , hoverBeside
    ) where

import qualified Control.Lens as Lens
import           Data.Aeson.TH (deriveJSON)
import           Data.Aeson.Types (defaultOptions)
import           Data.List.Utils (minimumOn)
import           Data.Vector.Vector2 (Vector2(..))
import           GUI.Momentu.Align (Aligned(..), value)
import qualified GUI.Momentu.Draw as Draw
import           GUI.Momentu.Element (Element, SizedElement)
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.Glue (Glue(..), Orientation, GluesTo)
import qualified GUI.Momentu.Glue as Glue
import           GUI.Momentu.Rect (Rect(..))
import qualified GUI.Momentu.State as State
import           GUI.Momentu.View (View)
import qualified GUI.Momentu.View as View
import           GUI.Momentu.Widget (Widget(..), R)
import qualified GUI.Momentu.Widget as Widget

import           Lamdu.Prelude

data Style = Style
    { frameColor :: Draw.Color
    , framePadding :: Vector2 R
    , bgColor :: Draw.Color
    , bgPadding :: Vector2 R
    } deriving (Eq, Generic, Show)
deriveJSON defaultOptions ''Style

Lens.makeLensesFor [("bgColor", "bgColorL")] ''Style

class HasStyle env where style :: Lens' env Style
instance HasStyle Style where style = id

backgroundColor :: HasStyle env => Lens' env Draw.Color
backgroundColor = style . bgColorL

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
    assymetricPad p0 p1 = unHover %~ Element.assymetricPad p0 p1
    scale r = unHover %~ Element.scale r
    empty = Hover Element.empty

instance SizedElement a => SizedElement (Hover a) where
    size = unHover . Element.size

instance Widget.HasWidget AnchoredWidget where widget = anchored

instance Functor f => Element (AnchoredWidget (f State.Update)) where
    setLayers = anchored . Element.setLayers
    hoverLayers = anchored %~ Element.hoverLayers
    empty = AnchoredWidget 0 Element.empty
    assymetricPad tl br (AnchoredWidget point w) =
        AnchoredWidget
        { _anchorPoint = point + tl
        , _anchored = Element.assymetricPad tl br w
        }
    scale ratio (AnchoredWidget point w) =
        AnchoredWidget
        { _anchorPoint = point * ratio
        , _anchored = Element.scale ratio w
        }

instance Functor f => SizedElement (AnchoredWidget (f State.Update)) where
    size = anchored . Element.size

instance Functor f => Glue (AnchoredWidget (f State.Update)) (Hover View) where
    type Glued (AnchoredWidget (f State.Update)) (Hover View) = AnchoredWidget (f State.Update)
    glue o ow (Hover ov) =
        Glue.glueH f o ow ov
        where
            f w v = w & Element.setLayers <>~ v ^. View.vAnimLayers

instance Functor f => Glue (Hover View) (AnchoredWidget (f State.Update)) where
    type Glued (Hover View) (AnchoredWidget (f State.Update)) = AnchoredWidget (f State.Update)
    glue o (Hover ov) =
        Glue.glueH f o ov
        where
            f v w = w & Element.setLayers <>~ v ^. View.vAnimLayers

instance Functor f => Glue (AnchoredWidget (f State.Update)) (Hover (Widget (f State.Update))) where
    type Glued (AnchoredWidget (f State.Update)) (Hover (Widget (f State.Update))) = AnchoredWidget (f State.Update)
    glue orientation ow0 (Hover ow1) =
        Glue.glueH f orientation ow0 ow1
        where
            f (AnchoredWidget pos w0) w1 =
                AnchoredWidget pos (Widget.glueStates orientation w0 w1)

instance Functor f => Glue (Hover (Widget (f State.Update))) (AnchoredWidget (f State.Update)) where
    type Glued (Hover (Widget (f State.Update))) (AnchoredWidget (f State.Update)) = AnchoredWidget (f State.Update)
    glue orientation (Hover ow0) =
        Glue.glueH f orientation ow0
        where
            f w0 (AnchoredWidget pos w1) =
                AnchoredWidget pos (Widget.glueStates orientation w0 w1)

data Ordered a = Ordered
    { _forward :: a
    , _backward :: a
    } deriving (Functor, Foldable, Traversable)
Lens.makeLenses ''Ordered

instance Applicative Ordered where
    pure = join Ordered
    Ordered fa fb <*> Ordered xa xb =
        Ordered (fa xa) (fb xb)

hoverBesideOptionsAxis ::
    ( Glue a b, Glue b a
    , SizedElement a, SizedElement b, SizedElement (Glued a b)
    ) =>
    Orientation -> Ordered a -> b -> [Glued a b]
hoverBesideOptionsAxis o (Ordered fwd bwd) src =
    do
        x <- [0, 1]
        let aSrc = Aligned x src
        [glue o aSrc (Aligned x fwd), glue o (Aligned x bwd) aSrc]
            <&> (^. value)

anchor :: Widget a -> AnchoredWidget a
anchor = AnchoredWidget 0

hoverBesideOptions ::
    ( Glue a b, Glue b a
    , SizedElement a, SizedElement b, SizedElement (Glued a b)
    ) =>
    a -> b -> [Glued a b]
hoverBesideOptions h src =
    do
        o <- [Glue.Vertical, Glue.Horizontal]
        hoverBesideOptionsAxis o (Ordered h h) src

addFrame ::
    (MonadReader env m, HasStyle env, Element a, Element.HasAnimIdPrefix env) =>
    m (a -> a)
addFrame =
    do
        s <- Lens.view style
        animId <- Lens.view Element.animIdPrefix
        pure $ \gui ->
            gui
            & Element.pad (bgPadding s)
            & Draw.backgroundColor (animId <> ["hover bg"]) (bgColor s)
            & Element.pad (framePadding s)
            & Draw.backgroundColor (animId <> ["hover frame"]) (frameColor s)

hover ::
    (MonadReader env m, Element a, HasStyle env, Element.HasAnimIdPrefix env) =>
    m (a -> Hover a)
hover =
    do
        frame <- addFrame
        pure (Hover . Element.hoverLayers . frame)

sequenceHover :: Functor f => Hover (f a) -> f (Hover a)
sequenceHover (Hover x) = x <&> Hover

-- TODO: Second argument here is really only (anchorPoint,size), take
-- it as such?
hoverInPlaceOf ::
    Functor f =>
    [AnchoredWidget (f State.Update)] ->
    AnchoredWidget (f State.Update) -> Widget (f State.Update)
hoverInPlaceOf [] _ = error "no hover options!"
hoverInPlaceOf hoverOptions@(defaultOption:_) place
    | null focusedOptions =
        Element.assymetricPad (translation defaultOption) 0 (defaultOption ^. anchored)
        & Element.size .~ place ^. Element.size
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
                x <- hoverOptions
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
    ( GluesTo (Hover w) (AnchoredWidget (f State.Update)) (AnchoredWidget (f State.Update))
    , SizedElement w
    , Element.HasAnimIdPrefix env, HasStyle env, MonadReader env m
    , Functor f
    ) =>
    (forall a b. Lens (t a) (t b) a b) ->
    m
    ( t (Widget (f State.Update)) ->
      w -> t (Widget (f State.Update))
    )
hoverBeside lens =
    hover <&>
    \mkHover layout h ->
    let a = layout & lens %~ anchor
    in  a & lens %~
        hoverInPlaceOf
        (hoverBesideOptions (mkHover h) (a ^. lens))
