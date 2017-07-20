{-# LANGUAGE NoImplicitPrelude, DeriveFunctor, TemplateHaskell, GeneralizedNewtypeDeriving, DeriveGeneric, OverloadedStrings, NamedFieldPuns, LambdaCase, FlexibleInstances, MultiParamTypeClasses, TypeFamilies, FlexibleContexts #-}
module Graphics.UI.Bottle.Widget
    ( Id(..), subId, Id.joinId, isSubCursor
    , HasCursor(..)

    -- Types:
    , R, Size

    , EnterResult(..), enterResultEvent, enterResultRect, enterResultLayer

    -- Event Result:
    , EventResult(..), eCursor, eVirtualCursor, eAnimIdMapping
    , VirtualCursorUpdate(..), _NewVirtualCursor, _ResetVirtualCursor
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
    , Focused(..), fFocalArea, fEventMap, fMEnter, fLayers
    , Surrounding(..), sLeft, sTop, sRight, sBottom

    , HasWidget(..)

    , isFocused

    , CursorConfig(..)
    , renderWithCursor, cursorAnimId

    -- Construct widgets:
    , fromView

    -- Focus handlers:
    , takesFocus

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
import           GHC.Generics (Generic)
import qualified Graphics.DrawingCombinators as Draw
import           Graphics.UI.Bottle.Animation (AnimId, R, Size)
import qualified Graphics.UI.Bottle.Animation as Anim
import           Graphics.UI.Bottle.Direction (Direction)
import qualified Graphics.UI.Bottle.Direction as Direction
import           Graphics.UI.Bottle.EventMap (EventMap)
import qualified Graphics.UI.Bottle.EventMap as EventMap
import           Graphics.UI.Bottle.MetaKey (MetaKey, toModKey)
import           Graphics.UI.Bottle.ModKey (ModKey(..))
import           Graphics.UI.Bottle.Rect (Rect(..))
import qualified Graphics.UI.Bottle.Rect as Rect
import           Graphics.UI.Bottle.View (View(..))
import qualified Graphics.UI.Bottle.View as View
import           Graphics.UI.Bottle.Widget.Id (Id(..))
import qualified Graphics.UI.Bottle.Widget.Id as Id
import           Graphics.UI.Bottle.Widgets.StdKeys as StdKeys
import qualified Graphics.UI.GLFW as GLFW

import           Lamdu.Prelude

-- The virtual cursor is the focal area that would ideally match the
-- direction of user movements
newtype VirtualCursor = VirtualCursor { _virtualCursor :: Rect }
Lens.makeLenses ''VirtualCursor

data VirtualCursorUpdate
    = NewVirtualCursor VirtualCursor
    | -- Set the virtual cursor to the new focal area
      ResetVirtualCursor
Lens.makePrisms ''VirtualCursorUpdate

data EventResult = EventResult
    { _eCursor :: Monoid.Last Id
    , _eVirtualCursor :: Monoid.Last VirtualCursorUpdate
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
    { _fFocalArea :: Rect
    , _fEventMap :: VirtualCursor -> EventMap a
    , -- TODO: Replace with fMEnterPoint that is for Point direction only
      _fMEnter :: Maybe (Direction -> EnterResult a)
    , _fLayers :: View.Layers
    } deriving Functor

data Unfocused a = Unfocused
    { _uMEnter :: Maybe (Direction -> EnterResult a)
    , _uLayers :: View.Layers
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

instance View.SetLayers (Widget a) where
    setLayers = sizedState <. stateLayers
    hoverLayers w =
        w
        & View.setLayers . View.layers %~ (mempty :)
        & mEnter . Lens._Just . Lens.mapped . enterResultLayer +~ 1

instance Functor f => View.Resizable (Widget (f EventResult)) where
    empty = fromView View.empty
    assymetricPad leftAndTop rightAndBottom w =
        w
        & wState .~ translate leftAndTop w
        & View.size +~ leftAndTop + rightAndBottom
    scale mult w =
        w
        & View.setLayers . View.layers . Lens.mapped %~ Anim.scale mult
        & View.size *~ mult
        & wState . _StateFocused . Lens.mapped . fFocalArea . Rect.topLeftAndSize *~ mult
        & wState . _StateFocused . Lens.mapped . fEventMap . Lens.argument . virtualCursor . Rect.topLeftAndSize //~ mult
        & mEnter . Lens._Just . Lens.mapped . enterResultRect . Rect.topLeftAndSize *~ mult
        & mEnter . Lens._Just . Lens.argument . Direction.coordinates . Rect.topLeftAndSize //~ mult
        & Lens.mapped . Lens.mapped . eVirtualCursor . Lens.mapped .
          _NewVirtualCursor . virtualCursor . Rect.topLeftAndSize *~ mult

instance View.HasSize (Widget a) where
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

instance View.Glue (Widget a) View where
    type Glued (Widget a) View = Widget a
    glue = View.glueH $ \w v -> w & View.setLayers <>~ v ^. View.vAnimLayers

instance Functor f => View.Glue View (Widget (f EventResult)) where
    type Glued View (Widget (f EventResult)) = Widget (f EventResult)
    glue = View.glueH $ \v w -> w & View.setLayers <>~ v ^. View.vAnimLayers

instance Functor f => View.Glue (Widget (f EventResult)) (Widget (f EventResult)) where
    type Glued (Widget (f EventResult)) (Widget (f EventResult)) = Widget (f EventResult)
    glue orientation = View.glueH (glueStates orientation) orientation

glueStates ::
    Functor f =>
    View.Orientation -> Widget (f EventResult) -> Widget (f EventResult) -> Widget (f EventResult)
glueStates orientation w0 w1 =
    w0
    & wState .~ combineStates orientation dirPrev dirNext (w0 ^. wSize) (w0 ^. wState) (w1 ^. wState)
    where
        (dirPrev, dirNext) =
            case orientation of
            View.Horizontal ->
                ( ("left", StdKeys.keysLeft StdKeys.stdDirKeys)
                , ("right", StdKeys.keysRight StdKeys.stdDirKeys)
                )
            View.Vertical ->
                ( ("up", StdKeys.keysUp StdKeys.stdDirKeys)
                , ("down", StdKeys.keysDown StdKeys.stdDirKeys)
                )

combineStates ::
    Functor f =>
    View.Orientation -> (Text, [GLFW.Key]) -> (Text, [GLFW.Key]) -> Size ->
    State (f EventResult) -> State (f EventResult) -> State (f EventResult)
combineStates _ _ _ _ StateFocused{} StateFocused{} = error "joining two focused widgets!!"
combineStates o _ _ sz (StateUnfocused u0) (StateUnfocused u1) =
    Unfocused (combineMEnters o sz (u0 ^. uMEnter) (u1 ^. uMEnter)) (u0 ^. uLayers <> u1 ^. uLayers)
    & StateUnfocused
combineStates orientation _ (nameNext, keysNext) sz (StateFocused f) (StateUnfocused u) =
    f
    <&> fMEnter %~ combineMEnters orientation sz (u ^. uMEnter)
    <&> fEventMap . Lens.imapped %@~ addEvents
    <&> fLayers <>~ u ^. uLayers
    & StateFocused
    where
        addEvents virtCursor =
            case u ^. uMEnter of
            Nothing -> mempty
            Just enter ->
                setVirt orientation virtCursor
                (enter (Direction.PrevFocalArea (virtCursor ^. virtualCursor)))
                ^. enterResultEvent
                & EventMap.keyPresses (keysNext <&> ModKey mempty) (EventMap.Doc ["Navigation", "Move", nameNext])
            & EventMap.weakerEvents
combineStates orientation dirPrev dirNext sz (StateUnfocused u) (StateFocused f) =
    combineStates orientation dirNext dirPrev sz (StateFocused f) (StateUnfocused u)

setVirt :: Functor f => View.Orientation -> VirtualCursor -> EnterResult (f EventResult) -> EnterResult (f EventResult)
setVirt orientation virtCursor enterResult =
    enterResult
    & enterResultEvent . Lens.mapped . eVirtualCursor . Lens._Wrapped ?~
    NewVirtualCursor
    ( enterResult ^. enterResultRect
        & Lens.cloneLens axis .~ virtCursor ^. virtualCursor . Lens.cloneLens axis
        & VirtualCursor
    )
    where
        axis =
            case orientation of
            View.Horizontal -> Rect.verticalRange
            View.Vertical -> Rect.horizontalRange

combineMEnters ::
    View.Orientation -> Size ->
    Maybe (Direction -> EnterResult a) ->
    Maybe (Direction -> EnterResult a) ->
    Maybe (Direction -> EnterResult a)
combineMEnters _ _ Nothing x = x
combineMEnters _ _ (Just x) Nothing = Just x
combineMEnters o sz (Just x) (Just y) = Just (combineEnters o sz x y)

combineEnters ::
    View.Orientation -> Size ->
    (Direction -> EnterResult a) -> (Direction -> EnterResult a) ->
    Direction -> EnterResult a
combineEnters o sz e0 e1 dir = chooseEnter o sz dir (e0 dir) (e1 dir)

chooseEnter :: View.Orientation -> Size -> Direction -> EnterResult a -> EnterResult a -> EnterResult a
chooseEnter orientation sz dir r0 r1
    -- coming in from the left/up, always choose the left/up one:
    | rect ^. Rect.bottomRight . l < 0 = r0
    -- coming in from the right/bottom, always choose the right/bottom one:
    | rect ^. Rect.topLeft . l > sz ^. l = r1
    | Rect.distance rect (r0 ^. enterResultRect) <
      Rect.distance rect (r1 ^. enterResultRect) = r0
    | otherwise = r1
    where
        l :: Lens' (Vector2 a) a
        l = View.axis orientation
        rect =
            case dir of
            Direction.Outside -> Rect 0 0
            Direction.Point x -> Rect x 0
            Direction.PrevFocalArea x -> x

isFocused :: Widget a -> Bool
isFocused = Lens.has (wState . _StateFocused)

eventMapMaker :: Lens.Setter' (Widget a) (VirtualCursor -> EventMap a)
eventMapMaker = wState . _StateFocused . Lens.mapped . fEventMap

eventResultFromCursor :: Id -> EventResult
eventResultFromCursor c = EventResult
    { _eCursor = Just c & Monoid.Last
    , _eVirtualCursor = Just ResetVirtualCursor & Monoid.Last
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

stateLayers :: Lens.Setter' (State a) View.Layers
stateLayers = stateLens uLayers (Lens.mapped . fLayers)

takesFocus ::
    (HasWidget w, Functor f) =>
    (Direction -> f Id) -> w (f EventResult) -> w (f EventResult)
takesFocus enterFunc =
    widget %~
    \w ->
    w & mEnter .~
    Just (
        enterFunc
        <&> Lens.mapped %~ eventResultFromCursor
        <&> EnterResult (Rect 0 (w ^. View.size)) 0
        )

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
            & uLayers %~ View.translateLayers pos
            <&> f

translateEventResult :: Vector2 R -> EventResult -> EventResult
translateEventResult pos =
    eVirtualCursor . Lens.mapped . _NewVirtualCursor . virtualCursor . Rect.topLeft +~ pos

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
            & fFocalArea . Rect.topLeft +~ pos
            & fEventMap . Lens.argument . virtualCursor . Rect.topLeft -~ pos
            & fLayers %~ View.translateLayers pos
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
            & Lens.argument . Direction.coordinates . Rect.topLeft -~ pos
            & Lens.mapped . enterResultRect . Rect.topLeft +~ pos

padToSizeAlign ::
    Functor f => Size -> Vector2 R -> Widget (f EventResult) -> Widget (f EventResult)
padToSizeAlign newSize alignment w =
    w
    & wState .~ translate (sizeDiff * alignment) w
    & View.size %~ (max <$> newSize <*>)
    where
        sizeDiff = max <$> 0 <*> newSize - w ^. View.size

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
        { _fFocalArea = rect
        , _fEventMap = eventMap
        , _fMEnter = u ^. uMEnter
        , _fLayers = u ^. uLayers
        }
    StateFocused makeFocus ->
        -- TODO: does this case make sense or is this an error?
        makeFocus
        <&> fFocalArea .~ rect
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
        (View.render (u ^. uLayers), u ^. uMEnter, Nothing)
    StateFocused f ->
        (cursorFrame <> View.render (r ^. fLayers), r ^. fMEnter, Just (r ^. fFocalArea, r ^. fEventMap))
        where
            r = f (Surrounding 0 0 0 0)
            area = r ^. fFocalArea
            cursorFrame =
                Anim.backgroundColor cursorAnimId cursorColor (area ^. Rect.size)
                & Anim.translate (area ^. Rect.topLeft)
