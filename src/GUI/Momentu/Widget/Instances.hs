{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, MultiParamTypeClasses, TypeFamilies, FlexibleContexts, OverloadedStrings #-}
module GUI.Momentu.Widget.Instances
    ( sizedState, stateLayers, stateLens, mEnter
    , glueStates
    , translateFocusedGeneric, translateUpdate
    , translate, fromView
    ) where

import           Control.Lens (LensLike)
import qualified Control.Lens as Lens
import           Data.Vector.Vector2 (Vector2(..))
import qualified Data.Vector.Vector2 as Vector2
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
import           GUI.Momentu.State (Update, VirtualCursor)
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
        & mEnter . Lens._Just . Lens.mapped . enterResultLayer +~ 1
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
        & wState . _StateFocused . Lens.mapped . fEventMap . Lens.argument . State.virtualCursor . Rect.topLeftAndSize //~ mult
        & mEnter . Lens._Just . Lens.mapped . enterResultRect . Rect.topLeftAndSize *~ mult
        & mEnter . Lens._Just . Lens.argument %~ Direction.scale (1 / mult)
        & Lens.mapped . Lens.mapped . State.uVirtualCursor . Lens.mapped .
          State.virtualCursor . Rect.topLeftAndSize *~ mult

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
instance EventMap.HasEventMap Widget where eventMap = eventMapMaker . Lens.mapped

instance Functor f => Glue (Widget (f Update)) View where
    type Glued (Widget (f Update)) View = Widget (f Update)
    glue = Glue.glueH $ \w v -> w & Element.setLayers <>~ v ^. View.vAnimLayers

instance Functor f => Glue View (Widget (f Update)) where
    type Glued View (Widget (f Update)) = Widget (f Update)
    glue = Glue.glueH $ \v w -> w & Element.setLayers <>~ v ^. View.vAnimLayers

instance Functor f => Glue (Widget (f Update)) (Widget (f Update)) where
    type Glued (Widget (f Update)) (Widget (f Update)) = Widget (f Update)
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
    & wState .~ combineStates orientation dirPrev dirNext (w0 ^. wSize) (w0 ^. wState) (w1 ^. wState)
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
    Orientation -> NavDir -> NavDir -> Size ->
    State (f Update) -> State (f Update) -> State (f Update)
combineStates _ _ _ _ StateFocused{} StateFocused{} = error "joining two focused widgets!!"
combineStates o _ _ sz (StateUnfocused u0) (StateUnfocused u1) =
    Unfocused (combineMEnters o sz (u0 ^. uMEnter) (u1 ^. uMEnter)) (u0 ^. uLayers <> u1 ^. uLayers)
    & StateUnfocused
combineStates orientation _ nextDir sz (StateFocused f) (StateUnfocused u) =
    f
    <&> fMEnter %~ combineMEnters orientation sz (u ^. uMEnter)
    <&> fEventMap . Lens.imapped %@~ addEvents
    <&> fLayers <>~ u ^. uLayers
    & StateFocused
    where
        chooseRange =
            case orientation of
            Horizontal -> Rect.verticalRange
            Vertical   -> Rect.horizontalRange
        addEvents virtCursor =
            case u ^. uMEnter of
            Nothing -> mempty
            Just enter ->
                enter (dirCons nextDir (virtCursor ^. State.virtualCursor . chooseRange))
                ^. enterResultEvent
                & EventMap.keyPresses (dirKeys nextDir <&> ModKey mempty) (EventMap.Doc ["Navigation", "Move", dirName nextDir])
            & EventMap.weakerEvents
combineStates orientation dirPrev dirNext sz (StateUnfocused u) (StateFocused f) =
    combineStates orientation dirNext dirPrev sz (StateFocused f) (StateUnfocused u)

combineMEnters ::
    Orientation -> Size ->
    Maybe (Direction -> EnterResult a) ->
    Maybe (Direction -> EnterResult a) ->
    Maybe (Direction -> EnterResult a)
combineMEnters _ _ Nothing x = x
combineMEnters _ _ (Just x) Nothing = Just x
combineMEnters o sz (Just x) (Just y) = Just (combineEnters o sz x y)

combineEnters ::
    Orientation -> Size ->
    (Direction -> EnterResult a) -> (Direction -> EnterResult a) ->
    Direction -> EnterResult a
combineEnters o sz e0 e1 dir = chooseEnter o sz dir (e0 dir) (e1 dir)

closer ::
    (Vector2 R -> R) -> Rect -> EnterResult a -> EnterResult a -> EnterResult a
closer axis r r0 r1
    | axis (Rect.distances r (r0 ^. enterResultRect)) <=
      axis (Rect.distances r (r1 ^. enterResultRect)) = r0
    | otherwise = r1

chooseEnter :: Orientation -> Size -> Direction -> EnterResult a -> EnterResult a -> EnterResult a
chooseEnter _          _ Direction.Outside   r0 _  = r0 -- left-biased
chooseEnter _          _ (Direction.Point p) r0 r1 =
    closer Vector2.sqrNorm (Rect p 0) r0 r1
chooseEnter Horizontal _ Direction.FromLeft{}  r0 _  = r0
chooseEnter Vertical   _ Direction.FromAbove{} r0 _  = r0
chooseEnter Horizontal _ Direction.FromRight{} _  r1 = r1
chooseEnter Vertical   _ Direction.FromBelow{} _  r1 = r1
chooseEnter Horizontal _ (Direction.FromAbove r) r0 r1 =
    closer (^. _1) topBarrier r0 r1
    where
        topBarrier = Rect 0 0 & Rect.horizontalRange .~ r
chooseEnter Horizontal sz (Direction.FromBelow r) r0 r1 =
    closer (^. _1) bottomBarrier r0 r1
    where
        bottomBarrier =
            Rect 0 0 & Rect.top .~ sz ^. _2 & Rect.horizontalRange .~ r
chooseEnter Vertical _ (Direction.FromLeft r) r0 r1 =
    closer (^. _2) leftBarrier r0 r1
    where
        leftBarrier = Rect 0 0 & Rect.verticalRange .~ r
chooseEnter Vertical sz (Direction.FromRight r) r0 r1 =
    closer (^. _2) rightBarrier r0 r1
    where
        rightBarrier =
            Rect 0 0 & Rect.left .~ sz ^. _1 & Rect.verticalRange .~ r

stateLayers :: Lens.Setter' (State a) Element.Layers
stateLayers = stateLens uLayers (Lens.mapped . fLayers)

mEnter :: Lens.Setter' (Widget a) (Maybe (Direction -> EnterResult a))
mEnter = wState . stateMEnter

eventMapMaker :: Lens.Setter' (Widget a) (VirtualCursor -> EventMap a)
eventMapMaker = wState . _StateFocused . Lens.mapped . fEventMap

stateLens ::
    Functor f =>
    LensLike f (Unfocused s) (Unfocused t) a b ->
    LensLike f (Surrounding -> Focused s) (Surrounding -> Focused t) a b ->
    LensLike f (State s) (State t) a b
stateLens uLens fLens f = onState (uLens f) (fLens f)

stateMEnter :: Lens.Setter' (State a) (Maybe (Direction -> EnterResult a))
stateMEnter = stateLens uMEnter (Lens.mapped . fMEnter)

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
translateUpdate pos =
    State.uVirtualCursor . Lens.mapped . State.virtualCursor . Rect.topLeft +~ pos

translateGeneric :: (a -> b) -> Vector2 R -> Widget a -> State b
translateGeneric f pos w =
    w ^. wState & onStatePure translateUnfocused (translateFocusedGeneric f pos)
    where
        onStatePure onU onF =
            Lens.runIdentity . onState (Lens.Identity . onU) (Lens.Identity . onF)
        translateUnfocused u =
            u
            & uMEnter %~ translateMEnter pos
            & uLayers %~ Element.translateLayers pos
            <&> f

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
            & fMEnter %~ translateMEnter pos
            & fFocalAreas . traverse . Rect.topLeft +~ pos
            & fEventMap . Lens.argument . State.virtualCursor . Rect.topLeft -~ pos
            & fLayers %~ Element.translateLayers pos
            <&> f

translateMEnter ::
    Vector2 R ->
    Maybe (Direction -> EnterResult a) ->
    Maybe (Direction -> EnterResult a)
translateMEnter pos =
    Lens._Just %~ translateEnter
    where
        translateEnter enter =
            enter
            & Lens.argument %~ Direction.translate (negate pos)
            & Lens.mapped . enterResultRect . Rect.topLeft +~ pos

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
