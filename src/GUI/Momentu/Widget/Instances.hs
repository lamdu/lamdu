{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeFamilies, FlexibleContexts, UndecidableInstances #-}
module GUI.Momentu.Widget.Instances
    ( sizedState, stateLayers, stateLens, enterResult
    , glueStates
    , translateFocusedGeneric, translateUpdate
    , translate, fromView
    , combineEnterPoints
    , eventMapMaker
    ) where

import           Control.Lens (LensLike)
import qualified Control.Lens as Lens
import           Data.Maybe.Utils (unionMaybeWith)
import           Data.Vector.Vector2 (Vector2(..))
import           GUI.Momentu.Animation (R, Size)
import qualified GUI.Momentu.Animation as Anim
import           GUI.Momentu.Direction (Direction)
import qualified GUI.Momentu.Direction as Direction
import           GUI.Momentu.Element (Element, SizedElement)
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.EventMap as EventMap
import           GUI.Momentu.Glue (Glue(..), Orientation(..))
import qualified GUI.Momentu.Glue as Glue
import           GUI.Momentu.ModKey (ModKey(..))
import qualified GUI.Momentu.ModKey as ModKey
import           GUI.Momentu.Rect (Rect(..))
import qualified GUI.Momentu.Rect as Rect
import           GUI.Momentu.State (Update)
import qualified GUI.Momentu.State as State
import           GUI.Momentu.View (View(..))
import qualified GUI.Momentu.View as View
import           GUI.Momentu.Widget.Types
import           GUI.Momentu.Widgets.StdKeys (DirKeys(..), stdDirKeys)

import           Lamdu.Prelude

sizedState :: Lens.IndexedLens' Size (Widget a) (State a)
sizedState f (Widget sz state) = Lens.indexed f sz state <&> Widget sz

instance Functor f => Element (Widget (f Update)) where
    setLayers = sizedState <. stateLayers
    hoverLayers w =
        w
        & Element.setLayers . Element.layers %~ (mempty :)
        & enterResult . enterResultLayer +~ 1
    empty = fromView Element.empty
    assymetricPad leftAndTop rightAndBottom w =
        w
        & wState .~ translate leftAndTop w
        & Element.size +~ leftAndTop + rightAndBottom
    scale mult w =
        w
        & Element.setLayers . Element.layers . Lens.mapped %~ Anim.scale mult
        & Element.size *~ mult
        & wState . _StateFocused . Lens.mapped . fFocalAreas . traverse . Rect.topLeftAndSize *~ mult
        & eventMapMaker . Lens.argument . eVirtualCursor . State.vcRect . Rect.topLeftAndSize //~ mult
        & enterResult . enterResultRect . Rect.topLeftAndSize *~ mult
        & wState . _StateUnfocused . uMEnter . Lens._Just . Lens.argument %~ Direction.scale (1 / mult)
        & wState . _StateFocused . Lens.mapped . fMEnterPoint . Lens._Just . Lens.argument //~ mult
        & Lens.mapped . Lens.mapped . State.uVirtualCursor . Lens.mapped . State.vcRect . Rect.topLeftAndSize *~ mult

instance Functor f => SizedElement (Widget (f Update)) where
    size f w =
        w
        & wSize f
        <&> sizedState <. (_StateFocused . Lens.argument) %@~ fixSurrounding
        where
            fixSurrounding (Vector2 nw nh) surrounding =
                surrounding
                & sRight +~ nw - ow
                & sBottom +~ nh - oh
            Vector2 ow oh = w ^. wSize

instance (Functor f, a ~ f Update) => Glue (Widget a) View where
    type Glued (Widget a) View = Widget a
    glue = Glue.glueH $ \w v -> w & Element.setLayers <>~ v ^. View.vAnimLayers

instance (Functor f, a ~ f Update) => Glue View (Widget a) where
    type Glued View (Widget a) = Widget a
    glue = Glue.glueH $ \v w -> w & Element.setLayers <>~ v ^. View.vAnimLayers

instance (Functor f, a ~ b, a ~ f Update) => Glue (Widget a) (Widget b) where
    type Glued (Widget a) (Widget b) = Widget a
    glue orientation = Glue.glueH (glueStates orientation) orientation

data NavDir = NavDir
    { dirCons :: Rect.Range R -> Direction
    , dirName :: Text
    , dirKeys :: [ModKey.Key]
    }

glueStates ::
    Functor f =>
    Orientation -> Widget (f Update) -> Widget (f Update) -> Widget (f Update)
glueStates orientation w0 w1 =
    w0
    & wState .~ combineStates orientation dirPrev dirNext (w0 ^. wState) (w1 ^. wState)
    where
        (dirPrev, dirNext) =
            case orientation of
            Horizontal ->
                ( NavDir Direction.FromRight "left"  (keysLeft stdDirKeys )
                , NavDir Direction.FromLeft  "right" (keysRight stdDirKeys)
                )
            Vertical ->
                ( NavDir Direction.FromBelow "up"    (keysUp stdDirKeys   )
                , NavDir Direction.FromAbove "down"  (keysDown stdDirKeys )
                )

combineStates ::
    Functor f =>
    Orientation -> NavDir -> NavDir ->
    State (f Update) -> State (f Update) -> State (f Update)
combineStates _ _ _ StateFocused{} StateFocused{} = error "joining two focused widgets!!"
combineStates o _ _ (StateUnfocused u0) (StateUnfocused u1) =
    Unfocused e (u0 ^. uLayers <> u1 ^. uLayers) & StateUnfocused
    where
        e = unionMaybeWith (combineEnters o) (u0 ^. uMEnter) (u1 ^. uMEnter)
combineStates orientation _ nextDir (StateFocused f) (StateUnfocused u) =
    f
    <&> fMEnterPoint %~ unionMaybeWith combineEnterPoints (u ^. uMEnter <&> (. Direction.Point))
    <&> fEventMap . Lens.imapped %@~ addEvents
    <&> fLayers <>~ u ^. uLayers
    & StateFocused
    where
        chooseRange =
            case orientation of
            Horizontal -> Rect.verticalRange
            Vertical   -> Rect.horizontalRange
        addEvents eventContext =
            case u ^. uMEnter of
            Nothing -> mempty
            Just enter ->
                enter (dirCons nextDir (eventContext ^. eVirtualCursor . State.vcRect . chooseRange))
                ^. enterResultEvent
                & EventMap.keyPresses (dirKeys nextDir <&> ModKey mempty) (EventMap.Doc ["Navigation", "Move", dirName nextDir])
            & flip mappend
combineStates orientation dirPrev dirNext (StateUnfocused u) (StateFocused f) =
    combineStates orientation dirNext dirPrev (StateFocused f) (StateUnfocused u)

combineEnters ::
    Orientation ->
    (Direction -> EnterResult a) -> (Direction -> EnterResult a) ->
    Direction -> EnterResult a
combineEnters o e0 e1 dir = chooseEnter o dir (e0 dir) (e1 dir)

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

chooseEnter :: Orientation -> Direction -> EnterResult a -> EnterResult a -> EnterResult a
chooseEnter _          Direction.Outside   r0 _  = r0 -- left-biased
chooseEnter _          (Direction.Point p) r0 r1 = closerGeometric p r0 r1
chooseEnter Horizontal Direction.FromLeft{}  r0 _  = r0
chooseEnter Vertical   Direction.FromAbove{} r0 _  = r0
chooseEnter Horizontal Direction.FromRight{} _  r1 = r1
chooseEnter Vertical   Direction.FromBelow{} _  r1 = r1
chooseEnter Horizontal (Direction.FromAbove r) r0 r1 =
    closer Rect.horizontalRange r r0 r1
chooseEnter Horizontal (Direction.FromBelow r) r0 r1 =
    closer Rect.horizontalRange r r0 r1
chooseEnter Vertical (Direction.FromLeft r) r0 r1 =
    closer Rect.verticalRange r r0 r1
chooseEnter Vertical (Direction.FromRight r) r0 r1 =
    closer Rect.verticalRange r r0 r1

stateLayers :: Lens.Setter' (State a) Element.Layers
stateLayers = stateLens uLayers (Lens.mapped . fLayers)

enterResult :: Lens.Setter' (Widget a) (EnterResult a)
enterResult = wState . stateEnterResult

eventMapMaker :: Lens.Setter' (Widget a) (EventContext -> EventMap a)
eventMapMaker = wState . _StateFocused . Lens.mapped . fEventMap

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
translate :: Functor f => Vector2 R -> Widget (f Update) -> State (f Update)
translate pos = translateGeneric (fmap (translateUpdate pos)) pos

translateUpdate :: Vector2 R -> Update -> Update
translateUpdate = (State.uVirtualCursor . Lens.mapped . State.vcRect . Rect.topLeft +~)

translateGeneric :: (a -> b) -> Vector2 R -> Widget a -> State b
translateGeneric f pos w =
    w ^. wState
    & onStatePure translateUnfocused (translateFocusedGeneric f pos)
    where
        onStatePure onU onF =
            Lens.runIdentity . onState (Lens.Identity . onU) (Lens.Identity . onF)
        translateUnfocused u =
            u
            & uMEnter . Lens._Just %~ translateEnter
            & uLayers %~ Element.translateLayers pos
            <&> f
        translateEnter enter =
            enter
            & Lens.argument %~ Direction.translate (negate pos)
            & Lens.mapped . enterResultRect . Rect.topLeft +~ pos

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
            & fLayers %~ Element.translateLayers pos
            <&> f

fromView :: View -> Widget a
fromView (View size mkLayers) =
    Widget
    { _wSize = size
    , _wState =
        StateUnfocused Unfocused
        { _uMEnter = Nothing
        , _uLayers = mkLayers
        }
    }
