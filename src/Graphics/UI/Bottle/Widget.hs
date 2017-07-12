{-# LANGUAGE NoImplicitPrelude, DeriveFunctor, TemplateHaskell, GeneralizedNewtypeDeriving, DeriveGeneric, OverloadedStrings, NamedFieldPuns, LambdaCase, FlexibleInstances #-}
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
    , Widget(..), wSize, wState
      , wView, mEnter, eventMapMaker, events
    , VirtualCursor(..), virtualCursor
    , Unfocused(..), uMEnter, uMakeLayers
    , Focused(..), fFocalArea, fEventMap, fMEnter, fMakeLayers

    , HasWidget(..)

    , isFocused

    , CursorConfig(..)
    , renderWithCursor, cursorAnimId

    -- Construct widgets:
    , empty
    , fromView

    -- Focus handlers:
    , takesFocus

    -- Operations:
    , translate, scale
    , assymetricPad, padToSizeAlign

    , makeFocusableView

    , respondToCursorPrefix
    , respondToCursorBy
    , setFocused, setFocusedWith

    , assignCursor
    , assignCursorPrefix
    ) where

import           Control.Applicative (liftA2)
import           Control.Lens (Lens')
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
import           Graphics.UI.Bottle.Rect (Rect(..))
import qualified Graphics.UI.Bottle.Rect as Rect
import           Graphics.UI.Bottle.View (View(..))
import qualified Graphics.UI.Bottle.View as View
import           Graphics.UI.Bottle.Widget.Id (Id(..))
import qualified Graphics.UI.Bottle.Widget.Id as Id

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
    , _fMakeLayers :: View.Surrounding -> View.Layers
    } deriving Functor

data Unfocused a = Unfocused
    { _uMEnter :: Maybe (Direction -> EnterResult a)
    , _uMakeLayers :: View.Surrounding -> View.Layers
    } deriving Functor

data Widget a = Widget
    { _wSize :: Size
    , _wState :: State a
    } deriving Functor

data State a
    = StateUnfocused (Unfocused a)
    | StateFocused (Focused a)
    deriving Functor

class HasWidget w where widget :: Lens.Setter (w a) (w b) (Widget a) (Widget b)
instance HasWidget Widget where widget = id

Lens.makeLenses ''EnterResult
Lens.makeLenses ''EventResult
Lens.makeLenses ''Focused
Lens.makeLenses ''Unfocused
Lens.makeLenses ''Widget
Lens.makePrisms ''State

instance View.MkView (Widget a) where setView = wView

wView :: Lens' (Widget a) View
wView f (Widget size state) =
    case state of
    StateUnfocused x -> g StateUnfocused uMakeLayers x
    StateFocused   x -> g StateFocused   fMakeLayers x
    where
        g cons lens x =
            f (View size (x ^. Lens.cloneLens lens))
            <&>
            \(View newSize newMakeLayers) ->
            x & Lens.cloneLens lens .~ newMakeLayers & cons & Widget newSize

instance Functor f => View.Pad (Widget (f EventResult)) where pad p = assymetricPad p p

instance View.HasSize (Widget a) where size = wSize
instance EventMap.HasEventMap Widget where eventMap = eventMapMaker . Lens.mapped

isFocused :: Widget a -> Bool
isFocused = Lens.has (wState . _StateFocused)

empty :: Widget f
empty = fromView View.empty

eventMapMaker :: Lens.Traversal' (Widget a) (VirtualCursor -> EventMap a)
eventMapMaker = wState . _StateFocused . fEventMap

eventResultFromCursor :: Id -> EventResult
eventResultFromCursor c = EventResult
    { _eCursor = Just c & Monoid.Last
    , _eVirtualCursor = Just ResetVirtualCursor & Monoid.Last
    , _eAnimIdMapping = mempty
    }

events :: HasWidget w => Lens.Setter (w a) (w b) a b
events =
    widget . wState . atEvents
    where
        atEvents :: Lens.Settable f => (a -> f b) -> State a -> f (State b)
        atEvents f (StateUnfocused x) = (uMEnter . atMEnter) f x <&> StateUnfocused
        atEvents f (StateFocused x) = Lens.sets atFocus f x <&> StateFocused
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
        , _uMakeLayers = mkLayers
        }
    }

stateMEnter :: Lens' (State a) (Maybe (Direction -> EnterResult a))
stateMEnter f (StateUnfocused x) = uMEnter f x <&> StateUnfocused
stateMEnter f (StateFocused   x) = fMEnter f x <&> StateFocused

mEnter :: Lens' (Widget a) (Maybe (Direction -> EnterResult a))
mEnter = wState . stateMEnter

stateMakeLayers :: Lens' (State a) (View.Surrounding -> View.Layers)
stateMakeLayers f (StateUnfocused x) = uMakeLayers f x <&> StateUnfocused
stateMakeLayers f (StateFocused   x) = fMakeLayers f x <&> StateFocused

makeLayers :: Lens' (Widget a) (View.Surrounding -> View.Layers)
makeLayers = wState . stateMakeLayers

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
translate :: Functor f => Vector2 R -> Widget (f EventResult) -> Widget (f EventResult)
translate pos w =
    w
    & mEnter . Lens._Just . Lens.argument .
        Direction.coordinates . Rect.topLeft -~ pos
    & mEnter . Lens._Just . Lens.mapped .
        enterResultRect . Rect.topLeft +~ pos
    & wState . _StateFocused . fFocalArea . Rect.topLeft +~ pos
    & wState . _StateFocused . fEventMap . Lens.argument . virtualCursor . Rect.topLeft -~ pos
    & Lens.mapped . Lens.mapped . eVirtualCursor . Lens.mapped .
      _NewVirtualCursor . virtualCursor . Rect.topLeft +~ pos
    & View.setView . View.vMakeLayers %~ View.translateMakeLayers pos

scale :: Functor f => Vector2 R -> Widget (f EventResult) -> Widget (f EventResult)
scale mult w =
    w
    & View.setView %~ View.scale mult
    & wState . _StateFocused . fFocalArea . Rect.topLeftAndSize *~ mult
    & wState . _StateFocused . fEventMap . Lens.argument . virtualCursor . Rect.topLeftAndSize //~ mult
    & mEnter . Lens._Just . Lens.mapped . enterResultRect . Rect.topLeftAndSize *~ mult
    & mEnter . Lens._Just . Lens.argument . Direction.coordinates . Rect.topLeftAndSize //~ mult
    & Lens.mapped . Lens.mapped . eVirtualCursor . Lens.mapped .
      _NewVirtualCursor . virtualCursor . Rect.topLeftAndSize *~ mult

assymetricPad :: Functor f => Vector2 R -> Vector2 R -> Widget (f EventResult) -> Widget (f EventResult)
assymetricPad leftAndTop rightAndBottom w =
    w
    & View.size +~ leftAndTop + rightAndBottom
    & translate leftAndTop

padToSizeAlign ::
    Functor f => Size -> Vector2 R -> Widget (f EventResult) -> Widget (f EventResult)
padToSizeAlign newSize alignment w =
    w
    & translate (sizeDiff * alignment)
    & View.size %~ liftA2 max newSize
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
setFocusedWith rect eventMap w =
    w & wState .~
    StateFocused Focused
    { _fFocalArea = rect
    , _fEventMap = eventMap
    , _fMEnter = w ^. mEnter
    , _fMakeLayers = w ^. makeLayers
    }

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

renderWithCursor :: CursorConfig -> Widget a -> Anim.Frame
renderWithCursor CursorConfig{cursorColor} w =
    maybe mempty renderCursor (w ^? wState . _StateFocused . fFocalArea)
    & (`mappend` View.render (w ^. wView))
    where
        renderCursor area =
            Anim.backgroundColor cursorAnimId cursorColor
            (area ^. Rect.size)
            & Anim.translate (area ^. Rect.topLeft)
