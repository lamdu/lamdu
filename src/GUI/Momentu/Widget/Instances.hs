{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleInstances #-}
module GUI.Momentu.Widget.Instances
    ( sizedState, stateLayers, stateLens, enterResult, wFocused
    , glueStates
    , translateFocusedGeneric, translateUpdate
    , translate, fromView
    , combineEnterPoints, combineMEnters
    , eventMapMaker
    , strollAheadKeys, strollBackKeys
    ) where

import           Control.Lens (LensLike)
import qualified Control.Lens as Lens
import           Data.Maybe.Extended (unionMaybeWith)
import qualified Data.Semigroup as Semigroup
import           Data.Vector.Vector2 (Vector2(..))
import           GUI.Momentu.Animation (R, Size)
import qualified GUI.Momentu.Animation as Anim
import           GUI.Momentu.Direction (Orientation(..), Order(..), reverseOrder, applyOrder)
import qualified GUI.Momentu.Direction as Dir
import           GUI.Momentu.Element (Element, SizedElement)
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.EventMap as EventMap
import           GUI.Momentu.FocusDirection (FocusDirection(..))
import qualified GUI.Momentu.FocusDirection as FDir
import           GUI.Momentu.Glue (Glue(..))
import qualified GUI.Momentu.Glue as Glue
import qualified GUI.Momentu.I18N as MomentuTexts
import           GUI.Momentu.MetaKey (MetaKey(..))
import qualified GUI.Momentu.MetaKey as MetaKey
import           GUI.Momentu.ModKey (ModKey(..))
import           GUI.Momentu.Rect (Rect(..))
import qualified GUI.Momentu.Rect as Rect
import qualified GUI.Momentu.State as State
import           GUI.Momentu.View (View(..))
import qualified GUI.Momentu.View as View
import           GUI.Momentu.Widget.Types
import           GUI.Momentu.Widgets.StdKeys (stdDirKeys, dirKey)

import           GUI.Momentu.Prelude

sizedState :: Lens.IndexedLens' Size (Widget f) (State (f State.Update))
sizedState f (Widget sz state) = Lens.indexed f sz state <&> Widget sz

widgetSize :: Lens' (Widget a) (Vector2 Double)
widgetSize f w =
    w
    & wSize f
    <&> sizedState <. (_StateFocused . Lens.argument) %@~ fixSurrounding
    where
        fixSurrounding (Vector2 nw nh) surrounding =
            surrounding
            & sRight +~ nw - ow
            & sBottom +~ nh - oh
        Vector2 ow oh = w ^. wSize

instance Functor f => Element (Widget f) where
    setLayeredImage = sizedState <. stateLayers
    hoverLayeredImage w =
        w
        & Element.setLayeredImage . Element.layers %~ (mempty :)
        & enterResult . enterResultLayer +~ 1
    empty = fromView Element.empty
    padImpl leftAndTop rightAndBottom w =
        w
        & wState .~ translate leftAndTop w
        & widgetSize +~ leftAndTop + rightAndBottom
    scale mult w =
        w
        & Element.setLayeredImage . Element.layers . Lens.mapped %~ Anim.scale mult
        & widgetSize *~ mult
        & wFocused . fFocalAreas . traverse . Rect.topLeftAndSize *~ mult
        & eventMapMaker . Lens.argument . eVirtualCursor . State.vcRect . Rect.topLeftAndSize //~ mult
        & enterResult . enterResultRect . Rect.topLeftAndSize *~ mult
        & wState . _StateUnfocused . uMEnter . Lens._Just . Lens.argument %~ FDir.scale (1 / mult)
        & wFocused . fMEnterPoint . Lens._Just . Lens.argument //~ mult
        & wState . Lens.mapped . Lens.mapped . State.uVirtualCursor . Lens.mapped . State.vcRect . Rect.topLeftAndSize *~ mult

instance Functor f => SizedElement (Widget f) where
    size = widgetSize

instance
    ( Functor f, Has Dir.Layout env
    ) => Glue env (Widget f) View where
    type Glued (Widget f) View = Widget f
    glue = Glue.glueH $ \w v -> w & Element.setLayeredImage <>~ v ^. View.vAnimLayers

instance
    ( Functor f, Has Dir.Layout env
    ) => Glue env View (Widget f) where
    type Glued View (Widget f) = Widget f
    glue = Glue.glueH $ \v w -> w & Element.setLayeredImage <>~ v ^. View.vAnimLayers

instance
    ( Applicative f, f ~ g
    , Glue.HasTexts env
    ) => Glue env (Widget f) (Widget g) where
    type Glued (Widget f) (Widget g) = Widget f
    glue env orientation =
        Glue.glueH (glueStates env orientation Forward Forward) env orientation

glueStates ::
    (Applicative f, Glue.HasTexts env) =>
    env -> Orientation -> Order -> Order ->
    Widget f -> Widget f -> Widget f
glueStates env orientation order strollOrder w0 w1 =
    w0
    & wState .~
        combineStates env orientation order strollOrder (w0 ^. wState) (w1 ^. wState)

combineStates ::
    (Applicative f, Glue.HasTexts env) =>
    env -> Orientation -> Order -> Order ->
    State (f State.Update) -> State (f State.Update) -> State (f State.Update)
combineStates _ _ _ _ StateFocused{} StateFocused{} = error "joining two focused widgets!!"
combineStates env o _ strollOrder (StateUnfocused u0) (StateUnfocused u1) =
    Unfocused e
    (applyOrder strollOrder (<>) (u0 ^. uMStroll) (u1 ^. uMStroll))
    (u0 ^. uLayers <> u1 ^. uLayers) & StateUnfocused
    where
        e = combineMEnters env o (u0 ^. uMEnter) (u1 ^. uMEnter)
combineStates env orientation order strollOrder (StateUnfocused u) (StateFocused f) =
    combineStates env orientation (reverseOrder order) (reverseOrder strollOrder)
    (StateFocused f) (StateUnfocused u)
combineStates env orientation order strollOrder (StateFocused f) (StateUnfocused u) =
    f
    <&> fMEnterPoint %~
        unionMaybeWith combineEnterPoints (u ^. uMEnter <&> (. Point))
    <&> fEventMap . Lens.imapped %@~ addEvents
    <&> fLayers <>~ u ^. uLayers
    & StateFocused
    where
        chooseRange =
            case orientation of
            Horizontal -> Rect.verticalRange
            Vertical   -> Rect.horizontalRange
        applyStrollPreference events =
            -- If the unfocused one has a stroll destination for us
            -- Use it in each event that prefers the stroll position
            case u ^. uMStroll of
            Just (Semigroup.First fwd, _) | strollOrder == Forward ->
                events <&> Lens.mapped %~
                \e ->
                if e ^. State.uPreferStroll . Lens._Wrapped
                then
                    e
                    & State.uCursor .~ (Just fwd ^. Lens._Unwrapped)
                    & State.uPreferStroll .~ mempty
                else
                    e
            _ -> events
        addEvents eventContext events =
            applyStrollPreference events
            <> foldMap (enterEvents eventContext) (u ^. uMEnter)
            <> foldMap strollEvents (u ^. uMStroll)
        enterEvents eventContext enter =
            eventContext ^. eVirtualCursor . State.vcRect . chooseRange
            & dirCons
            & enter
            & (^. enterResultEvent)
            & EventMap.keyPresses
                (dirKey dir orientation order stdDirKeys <&> ModKey mempty)
            (EventMap.Doc
                [ env ^. has . MomentuTexts.navigation
                , env ^. has . MomentuTexts.move
                , env ^. has . Dir.textLens orientation order])
        dir = env ^. has
        strollEvents (Semigroup.First fwd, Semigroup.Last bwd)
            | strollOrder == Backward =
                EventMap.keysEventMapMovesCursor strollBackKeys
                (Glue.strollDoc env MomentuTexts.backward) (pure bwd)
            | otherwise =
                EventMap.keysEventMapMovesCursor strollAheadKeys
                (Glue.strollDoc env MomentuTexts.forward) (pure fwd)
        dirCons =
            case (dir, orientation, order) of
            (_, Vertical  , Backward) -> FromBelow
            (_, Vertical  , Forward ) -> FromAbove
            (Dir.LeftToRight, Horizontal, Backward) -> FromRight
            (Dir.LeftToRight, Horizontal, Forward ) -> FromLeft
            (Dir.RightToLeft, Horizontal, Backward) -> FromLeft
            (Dir.RightToLeft, Horizontal, Forward ) -> FromRight

strollAheadKeys :: [MetaKey]
strollAheadKeys = [MetaKey MetaKey.noMods MetaKey.Key'Tab]

strollBackKeys :: [MetaKey]
strollBackKeys = [MetaKey.shift MetaKey.Key'Tab]

combineMEnters ::
    Has Dir.Layout env =>
    env -> Orientation ->
    Maybe (FocusDirection -> EnterResult a) ->
    Maybe (FocusDirection -> EnterResult a) ->
    Maybe (FocusDirection -> EnterResult a)
combineMEnters env = unionMaybeWith . combineEnters (env ^. has)

combineEnters ::
    Dir.Layout -> Orientation ->
    (FocusDirection -> EnterResult a) ->
    (FocusDirection -> EnterResult a) ->
    FocusDirection -> EnterResult a
combineEnters ldir o e0 e1 dir = chooseEnter ldir o dir (e0 dir) (e1 dir)

combineEnterPoints ::
    (Vector2 R -> EnterResult a) -> (Vector2 R -> EnterResult a) ->
    Vector2 R -> EnterResult a
combineEnterPoints e0 e1 p = closerGeometric p (e0 p) (e1 p)

closerGeometric :: Vector2 R -> EnterResult a -> EnterResult a -> EnterResult a
closerGeometric p r0 r1
    | Rect.sqrPointDistance p (r0 ^. enterResultRect) <=
      Rect.sqrPointDistance p (r1 ^. enterResultRect) = r0
    | otherwise = r1

closer ::
    Lens.ALens' Rect (Rect.Range R) -> Rect.Range R ->
    EnterResult a -> EnterResult a -> EnterResult a
closer axis r r0 r1
    | Rect.rangeDistance r (r0 ^# enterResultRect . axis) <=
      Rect.rangeDistance r (r1 ^# enterResultRect . axis) = r0
    | otherwise = r1

chooseEnter ::
    Dir.Layout -> Orientation -> FocusDirection ->
    EnterResult a -> EnterResult a -> EnterResult a
chooseEnter _ _          FromOutside r0 _  = r0 -- left-biased
chooseEnter _ _          (Point p) r0 r1 = closerGeometric p r0 r1
chooseEnter _ Vertical   FromAbove{} r0 _  = r0
chooseEnter _ Vertical   FromBelow{} _  r1 = r1
chooseEnter _ Horizontal (FromAbove r) r0 r1 =
    closer Rect.horizontalRange r r0 r1
chooseEnter _ Horizontal (FromBelow r) r0 r1 =
    closer Rect.horizontalRange r r0 r1
chooseEnter _ Vertical (FromLeft r) r0 r1 =
    closer Rect.verticalRange r r0 r1
chooseEnter _ Vertical (FromRight r) r0 r1 =
    closer Rect.verticalRange r r0 r1
chooseEnter Dir.LeftToRight Horizontal FromLeft{}  r0 _  = r0
chooseEnter Dir.LeftToRight Horizontal FromRight{} _  r1 = r1
chooseEnter Dir.RightToLeft Horizontal FromLeft{}  _  r1 = r1
chooseEnter Dir.RightToLeft Horizontal FromRight{} r0 _  = r0

stateLayers :: Lens.Setter' (State a) Element.LayeredImage
stateLayers = stateLens uLayers (Lens.mapped . fLayers)

enterResult :: Lens.Setter' (Widget f) (EnterResult (f State.Update))
enterResult = wState . stateEnterResult

wFocused :: Lens.IndexedSetter' Size (Widget f) (Focused (f State.Update))
wFocused = sizedState <. _StateFocused . Lens.mapped

eventMapMaker :: Lens.Setter' (Widget f) (EventContext -> EventMap (f State.Update))
eventMapMaker = wFocused . fEventMap

stateLens ::
    Functor f =>
    LensLike f (Unfocused s) (Unfocused t) a b ->
    LensLike f (Surrounding -> Focused s) (Surrounding -> Focused t) a b ->
    LensLike f (State s) (State t) a b
stateLens uLens fLens f = onState (uLens f) (fLens f)

stateEnterResult :: Lens.Setter' (State a) (EnterResult a)
stateEnterResult =
    stateLens
    (uMEnter . Lens._Just . Lens.mapped)
    (Lens.mapped . fMEnterPoint . Lens._Just . Lens.mapped)

onState ::
    Functor f =>
    (Unfocused a -> f (Unfocused b)) ->
    ((Surrounding -> Focused a) -> f (Surrounding -> Focused b)) ->
    State a -> f (State b)
onState onUnfocused _ (StateUnfocused x) = onUnfocused x <&> StateUnfocused
onState _   onFocused (StateFocused   x) = onFocused   x <&> StateFocused

-- TODO: This actually makes an incorrect widget because its size
-- remains same, but it is now translated away from 0..size
-- Should expose higher-level combinators instead?
translate :: Functor f => Vector2 R -> Widget f -> State (f State.Update)
translate pos = translateGeneric (fmap (translateUpdate pos)) pos

translateUpdate :: Vector2 R -> State.Update -> State.Update
translateUpdate = (State.uVirtualCursor . Lens.mapped . State.vcRect . Rect.topLeft +~)

translateGeneric :: (f State.Update -> b) -> Vector2 R -> Widget f -> State b
translateGeneric f pos w =
    w ^. wState
    & onStatePure translateUnfocused (translateFocusedGeneric f pos)
    where
        onStatePure onU onF =
            runIdentity . onState (Identity . onU) (Identity . onF)
        translateUnfocused u =
            u
            & uMEnter . Lens._Just %~ translateEnter
            & uLayers %~ Element.translateLayeredImage pos
            <&> f
        translateEnter enter =
            enter
            & Lens.argument %~ FDir.translate (negate pos)
            <&> enterResultRect . Rect.topLeft +~ pos

translateFocusedGeneric ::
    (a -> b) -> Vector2 R ->
    (Surrounding -> Focused a) ->
    Surrounding -> Focused b
translateFocusedGeneric f pos x =
    x
    & Lens.argument %~ translateSurrounding
    <&> onFocused
    where
        translateSurrounding s =
            s
            & sLeft +~ pos ^. _1
            & sRight -~ pos ^. _1
            & sTop +~ pos ^. _2
            & sBottom -~ pos ^. _2
        onFocused focused =
            focused
            & fMEnterPoint . Lens._Just . Lens.argument -~ pos
            & fMEnterPoint . Lens._Just . Lens.mapped . enterResultRect . Rect.topLeft +~ pos
            & fFocalAreas . traverse . Rect.topLeft +~ pos
            & fEventMap . Lens.argument . eVirtualCursor . State.vcRect . Rect.topLeft -~ pos
            & fLayers %~ Element.translateLayeredImage pos
            <&> f

fromView :: View -> Widget a
fromView (View size mkLayers) =
    Widget
    { _wSize = size
    , _wState =
        StateUnfocused Unfocused
        { _uMEnter = Nothing
        , _uMStroll = Nothing
        , _uLayers = mkLayers
        }
    }
