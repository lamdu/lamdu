{-# LANGUAGE NoImplicitPrelude, DeriveFunctor, TemplateHaskell, GeneralizedNewtypeDeriving, DeriveGeneric, OverloadedStrings, NamedFieldPuns, LambdaCase, FlexibleInstances, MultiParamTypeClasses, TypeFamilies, FlexibleContexts #-}
module GUI.Momentu.Widget
    ( Id(..), subId, Id.joinId, isSubCursor
    , HasCursor(..)

    -- Types:
    , R, Size

    , EnterResult(..), enterResultEvent, enterResultRect, enterResultLayer

    -- Event Result:
    , EventResult(..), eCursor, eVirtualCursor, eAnimIdMapping
    , eventResultFromCursor
    , applyIdMapping

    -- Events:
    , EventMap
    , keysEventMap
    , keysEventMapMovesCursor

    -- Widget type and lenses:
    , State(..), _StateFocused, _StateUnfocused
        , stateLayers
    , Widget(..), wState
        , mEnter, eventMapMaker, events
    , VirtualCursor(..), virtualCursor
    , Unfocused(..), uMEnter, uLayers
    , Focused(..), fFocalAreas, fEventMap, fMEnter, fLayers
    , Surrounding(..), sLeft, sTop, sRight, sBottom

    , HasWidget(..)

    , isFocused

    , CursorConfig(..)
    , renderWithCursor, cursorAnimId

    -- Construct widgets:
    , fromView

    -- Focus handlers:
    , takesFocus
    , enterFuncAddVirtualCursor

    -- Operations:
    , translate, translateFocused
    , padToSizeAlign

    , makeFocusableView

    , respondToCursorPrefix
    , respondToCursorBy
    , setFocused, setFocusedWith

    , assignCursor
    , assignCursorPrefix

    , glueStates
    ) where

import           Control.Lens (LensLike)
import qualified Control.Lens as Lens
import qualified Control.Monad.Reader as Reader
import qualified Data.Map as Map
import qualified Data.Monoid as Monoid
import           Data.Monoid.Generic (def_mempty, def_mappend)
import           Data.Vector.Vector2 (Vector2(..))
import qualified Data.Vector.Vector2 as Vector2
import           GHC.Generics (Generic)
import           GUI.Momentu.Animation (AnimId, R, Size)
import qualified GUI.Momentu.Animation as Anim
import           GUI.Momentu.Direction (Direction)
import qualified GUI.Momentu.Direction as Direction
import           GUI.Momentu.Element (Element, SizedElement)
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.EventMap as EventMap
import           GUI.Momentu.Glue (Glue(..), Orientation(..))
import qualified GUI.Momentu.Glue as Glue
import           GUI.Momentu.MetaKey (MetaKey, toModKey)
import           GUI.Momentu.ModKey (ModKey(..))
import           GUI.Momentu.Rect (Rect(..))
import qualified GUI.Momentu.Rect as Rect
import           GUI.Momentu.View (View(..))
import qualified GUI.Momentu.View as View
import           GUI.Momentu.Widget.Id (Id(..))
import qualified GUI.Momentu.Widget.Id as Id
import           GUI.Momentu.Widgets.StdKeys (DirKeys(..), stdDirKeys)
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.GLFW as GLFW

import           Lamdu.Prelude

-- The virtual cursor is the focal area that would ideally match the
-- direction of user movements
newtype VirtualCursor = VirtualCursor { _virtualCursor :: Rect }
Lens.makeLenses ''VirtualCursor

data EventResult = EventResult
    { _eCursor :: Monoid.Last Id
    , _eVirtualCursor :: Monoid.Last VirtualCursor
    , _eAnimIdMapping :: Monoid.Endo AnimId
    } deriving (Generic)
instance Monoid EventResult where
    mempty = def_mempty
    mappend = def_mappend

data EnterResult a = EnterResult
    { -- The new focal area upon this entrace.
      -- Used in Grid to decide which cell's EnterResult to use.
      _enterResultRect :: Rect
    , -- Used to allow grid to choose hovering results over the results below them.
      _enterResultLayer :: Int
    , _enterResultEvent :: a
    } deriving Functor

-- When focused, mEnter may still be relevant, e.g: Mouse click in an
-- active textedit, to move to a different text-edit position.

data Focused a = Focused
    { -- When browsing sub-menus each selected menu is considered focal.
      -- The last focal area is where the cursor is,
      -- however Zoom should care about the first focal area
      _fFocalAreas :: [Rect]
    , _fEventMap :: VirtualCursor -> EventMap a
    , -- TODO: Replace with fMEnterPoint that is for Point direction only
      _fMEnter :: Maybe (Direction -> EnterResult a)
    , _fLayers :: Element.Layers
    } deriving Functor

data Unfocused a = Unfocused
    { _uMEnter :: Maybe (Direction -> EnterResult a)
    , _uLayers :: Element.Layers
    } deriving Functor

data Widget a = Widget
    { _wSize :: Size
    , _wState :: State a
    } deriving Functor

-- Area on screen around a focused widget. Used for positioning of hovers.
data Surrounding = Surrounding
    { _sLeft :: !R
    , _sTop :: !R
    , _sRight :: !R
    , _sBottom :: !R
    } deriving (Eq, Ord, Show)

data State a
    = StateUnfocused (Unfocused a)
    | StateFocused (Surrounding -> Focused a)
    deriving Functor

class HasWidget w where widget :: Lens.Setter (w a) (w b) (Widget a) (Widget b)
instance HasWidget Widget where widget = id

Lens.makeLenses ''EnterResult
Lens.makeLenses ''EventResult
Lens.makeLenses ''Focused
Lens.makeLenses ''Surrounding
Lens.makeLenses ''Unfocused
Lens.makeLenses ''Widget
Lens.makePrisms ''State

sizedState :: Lens.IndexedLens' Size (Widget a) (State a)
sizedState f (Widget sz state) = Lens.indexed f sz state <&> Widget sz

instance Functor f => Element (Widget (f EventResult)) where
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
        & wState . _StateFocused . Lens.mapped . fEventMap . Lens.argument . virtualCursor . Rect.topLeftAndSize //~ mult
        & mEnter . Lens._Just . Lens.mapped . enterResultRect . Rect.topLeftAndSize *~ mult
        & mEnter . Lens._Just . Lens.argument %~ Direction.scale (1 / mult)
        & Lens.mapped . Lens.mapped . eVirtualCursor . Lens.mapped .
          virtualCursor . Rect.topLeftAndSize *~ mult

instance Functor f => SizedElement (Widget (f EventResult)) where
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

instance Functor f => Glue (Widget (f EventResult)) View where
    type Glued (Widget (f EventResult)) View = Widget (f EventResult)
    glue = Glue.glueH $ \w v -> w & Element.setLayers <>~ v ^. View.vAnimLayers

instance Functor f => Glue View (Widget (f EventResult)) where
    type Glued View (Widget (f EventResult)) = Widget (f EventResult)
    glue = Glue.glueH $ \v w -> w & Element.setLayers <>~ v ^. View.vAnimLayers

instance Functor f => Glue (Widget (f EventResult)) (Widget (f EventResult)) where
    type Glued (Widget (f EventResult)) (Widget (f EventResult)) = Widget (f EventResult)
    glue orientation = Glue.glueH (glueStates orientation) orientation

data NavDir = NavDir
    { dirCons :: Rect.Range R -> Direction
    , dirName :: Text
    , dirKeys :: [GLFW.Key]
    }

glueStates ::
    Functor f =>
    Orientation -> Widget (f EventResult) -> Widget (f EventResult) -> Widget (f EventResult)
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
    State (f EventResult) -> State (f EventResult) -> State (f EventResult)
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
                enter (dirCons nextDir (virtCursor ^. virtualCursor . chooseRange))
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

isFocused :: Widget a -> Bool
isFocused = Lens.has (wState . _StateFocused)

eventMapMaker :: Lens.Setter' (Widget a) (VirtualCursor -> EventMap a)
eventMapMaker = wState . _StateFocused . Lens.mapped . fEventMap

eventResultFromCursor :: Id -> EventResult
eventResultFromCursor c = EventResult
    { _eCursor = Just c & Monoid.Last
    , _eVirtualCursor = mempty
    , _eAnimIdMapping = mempty
    }

onState ::
    Functor f =>
    (Unfocused a -> f (Unfocused b)) ->
    ((Surrounding -> Focused a) -> f (Surrounding -> Focused b)) ->
    State a -> f (State b)
onState onUnfocused _ (StateUnfocused x) = onUnfocused x <&> StateUnfocused
onState _   onFocused (StateFocused   x) = onFocused   x <&> StateFocused

stateLens ::
    Functor f =>
    LensLike f (Unfocused s) (Unfocused t) a b ->
    LensLike f (Surrounding -> Focused s) (Surrounding -> Focused t) a b ->
    LensLike f (State s) (State t) a b
stateLens uLens fLens f = onState (uLens f) (fLens f)

{-# ANN module ("HLint: ignore Eta reduce"::String) #-}

events :: HasWidget w => Lens.Setter (w a) (w b) a b
events =
    widget . wState . stateLens (uMEnter . atMEnter) (Lens.mapped . Lens.sets atFocus)
    where
        atMEnter f = (Lens._Just . Lens.mapped . enterResultEvent) f
        atFocus f focused =
            focused
            { _fMEnter = focused ^. fMEnter & atMEnter %~ f
            , _fEventMap = focused ^. fEventMap <&> Lens.mapped %~ f
            }

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

stateMEnter :: Lens.Setter' (State a) (Maybe (Direction -> EnterResult a))
stateMEnter = stateLens uMEnter (Lens.mapped . fMEnter)

mEnter :: Lens.Setter' (Widget a) (Maybe (Direction -> EnterResult a))
mEnter = wState . stateMEnter

stateLayers :: Lens.Setter' (State a) Element.Layers
stateLayers = stateLens uLayers (Lens.mapped . fLayers)

takesFocus ::
    (HasWidget w, Functor f) =>
    (Direction -> f Id) -> w (f EventResult) -> w (f EventResult)
takesFocus enterFunc =
    widget %~
    \w ->
        let rect = Rect 0 (w ^. Element.size)
        in  w & mEnter ?~
            ( enterFunc
                <&> Lens.mapped %~ eventResultFromCursor
                <&> EnterResult rect 0
                & enterFuncAddVirtualCursor rect
            )

enterFuncAddVirtualCursor ::
    Functor f =>
    Rect -> (Direction -> EnterResult (f EventResult)) -> (Direction -> EnterResult (f EventResult))
enterFuncAddVirtualCursor destRect =
    Lens.imapped <. (enterResultEvent . Lens.mapped . eVirtualCursor . Lens._Wrapped) .@~ mkVirtCursor
    where
        mkVirtCursor dir =
            case dir of
            Direction.FromRight r -> destRect & Rect.verticalRange   .~ r & Just
            Direction.FromLeft  r -> destRect & Rect.verticalRange   .~ r & Just
            Direction.FromAbove r -> destRect & Rect.horizontalRange .~ r & Just
            Direction.FromBelow r -> destRect & Rect.horizontalRange .~ r & Just
            Direction.Outside     -> Nothing
            Direction.Point p     -> Rect p 0 & Just
            <&> VirtualCursor

applyIdMapping :: Map Id Id -> EventResult -> EventResult
applyIdMapping widgetIdMap eventResult =
    eventResult
    & eAnimIdMapping <>~ Monoid.Endo (Anim.mappingFromPrefixMap animIdMap)
    & eCursor . Lens._Wrapped' . Lens._Just %~ mapCursor
    where
        animIdMap =
            widgetIdMap
            & Map.mapKeys toAnimId & Map.map toAnimId
        mapCursor (Id oldCursor) =
            Id $ Anim.mappingFromPrefixMap animIdMap oldCursor

keysEventMap ::
    Functor f => [MetaKey] -> EventMap.Doc ->
    f () -> EventMap (f EventResult)
keysEventMap keys doc act =
    (fmap . const) mempty <$>
    EventMap.keyPresses (keys <&> toModKey) doc act

keysEventMapMovesCursor ::
    Functor f => [MetaKey] -> EventMap.Doc ->
    f Id -> EventMap (f EventResult)
keysEventMapMovesCursor keys doc act =
    fmap eventResultFromCursor <$>
    EventMap.keyPresses (keys <&> toModKey) doc act

-- TODO: This actually makes an incorrect widget because its size
-- remains same, but it is now translated away from 0..size
-- Should expose higher-level combinators instead?
translate :: Functor f => Vector2 R -> Widget (f EventResult) -> State (f EventResult)
translate pos = translateGeneric (fmap (translateEventResult pos)) pos

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

translateEventResult :: Vector2 R -> EventResult -> EventResult
translateEventResult pos =
    eVirtualCursor . Lens.mapped . virtualCursor . Rect.topLeft +~ pos

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
            & fEventMap . Lens.argument . virtualCursor . Rect.topLeft -~ pos
            & fLayers %~ Element.translateLayers pos
            <&> f

translateFocused ::
    Functor f =>
    Vector2 R -> (Surrounding -> Focused (f EventResult)) ->
    Surrounding -> Focused (f EventResult)
translateFocused pos = translateFocusedGeneric (fmap (translateEventResult pos)) pos

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

padToSizeAlign ::
    Functor f => Size -> Vector2 R -> Widget (f EventResult) -> Widget (f EventResult)
padToSizeAlign newSize alignment w =
    w
    & wState .~ translate (sizeDiff * alignment) w
    & Element.size %~ (max <$> newSize <*>)
    where
        sizeDiff = max <$> 0 <*> newSize - w ^. Element.size

class HasCursor env where cursor :: Lens' env Id

subId :: (MonadReader env m, HasCursor env) => m (Id -> Maybe AnimId)
subId = Lens.view cursor <&> flip Id.subId

isSubCursor :: (MonadReader env m, HasCursor env) => m (Id -> Bool)
isSubCursor = subId <&> \sub prefix -> sub prefix & Lens.has Lens._Just

setFocused :: HasWidget w => w a -> w a
setFocused = widget %~ \w -> setFocusedWith (Rect 0 (w ^. wSize)) mempty w

setFocusedWith :: Rect -> (VirtualCursor -> EventMap a) -> Widget a -> Widget a
setFocusedWith rect eventMap =
    wState %~
    \s ->
    case s of
    StateUnfocused u ->
        const Focused
        { _fFocalAreas = [rect]
        , _fEventMap = eventMap
        , _fMEnter = u ^. uMEnter
        , _fLayers = u ^. uLayers
        }
    StateFocused makeFocus ->
        -- TODO: does this case make sense or is this an error?
        makeFocus
        <&> fFocalAreas .~ [rect]
        <&> fEventMap .~ eventMap
    & StateFocused

respondToCursorBy ::
    (MonadReader env m, HasCursor env, HasWidget w) =>
    m ((Id -> Bool) -> w a -> w a)
respondToCursorBy =
    Lens.view cursor
    <&> \c f -> if f c then setFocused else id

respondToCursorPrefix ::
    (MonadReader env m, HasCursor env, HasWidget w) =>
    m (Id -> w a -> w a)
respondToCursorPrefix =
    respondToCursorBy
    <&> \respond myIdPrefix -> respond (Lens.has Lens._Just . Id.subId myIdPrefix)

assignCursor ::
    (HasCursor env, MonadReader env m) =>
    Id -> Id -> m a -> m a
assignCursor src dest =
    Reader.local (cursor %~ replace)
    where
        replace c
            | c == src = dest
            | otherwise = c

assignCursorPrefix ::
    (HasCursor env, MonadReader env m) =>
    Id -> (AnimId -> Id) -> m a -> m a
assignCursorPrefix srcFolder dest =
    Reader.local (cursor %~ replace)
    where
        replace c =
            case Id.subId srcFolder c of
            Nothing -> c
            Just suffix -> dest suffix

makeFocusableView ::
    (MonadReader env m, HasCursor env, Applicative f, HasWidget w) =>
    m (Id -> w (f EventResult) -> w (f EventResult))
makeFocusableView =
    respondToCursorPrefix
    <&> \respond myIdPrefix ->
    respond myIdPrefix
    <&> takesFocus (const (pure myIdPrefix))

cursorAnimId :: AnimId
cursorAnimId = ["background"]

newtype CursorConfig = CursorConfig
    { cursorColor :: Draw.Color
    }

renderWithCursor ::
    CursorConfig -> Widget a ->
    (Anim.Frame, Maybe (Direction -> EnterResult a), Maybe (Rect, VirtualCursor -> EventMap a))
renderWithCursor CursorConfig{cursorColor} w =
    case w ^. wState of
    StateUnfocused u ->
        -- Unfocused top level widget! TODO: Is this some sort of error?
        (Element.render (u ^. uLayers), u ^. uMEnter, Nothing)
    StateFocused f ->
        (cursorFrame <> Element.render (r ^. fLayers), r ^. fMEnter, Just (area, r ^. fEventMap))
        where
            r = f (Surrounding 0 0 0 0)
            area = last (r ^. fFocalAreas)
            cursorFrame =
                Anim.backgroundColor cursorAnimId cursorColor (area ^. Rect.size)
                & Anim.translate (area ^. Rect.topLeft)
