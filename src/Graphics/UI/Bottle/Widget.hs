{-# LANGUAGE NoImplicitPrelude, DeriveFunctor, TemplateHaskell, GeneralizedNewtypeDeriving, DeriveGeneric, OverloadedStrings, NamedFieldPuns, LambdaCase, FlexibleInstances #-}
module Graphics.UI.Bottle.Widget
    ( Id(..), subId, Id.joinId, isSubCursor
    , HasCursor(..)

    -- Types:
    , R, Size

    , EnterResult(..), enterResultEvent, enterResultRect

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
    , Widget(..), wView, mEnter, mFocus, eventMapMaker
    , MEnter
    , VirtualCursor(..), virtualCursor
    , Focus(..), fEventMap, focalArea
    , events

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
    , setFocused

    , assignCursor
    , assignCursorPrefix
    ) where

import           Control.Applicative (liftA2)
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
    , _enterResultEvent :: a
    } deriving Functor

data Focus a = Focus
    { _focalArea :: Rect
    , _fEventMap :: VirtualCursor -> EventMap a
    } deriving Functor

-- When focused, mEnter may still be relevant, e.g: Mouse click in an
-- active textedit, to move to a different text-edit position.
type MEnter a = Maybe (Direction -> EnterResult a)

data Widget a = Widget
    { _wView :: View
    , _mEnter :: MEnter a -- Nothing if we're not enterable
    , _mFocus :: Maybe (Focus a)
    } deriving Functor

class HasWidget w where widget :: Lens.Setter (w a) (w b) (Widget a) (Widget b)
instance HasWidget Widget where widget = id

Lens.makeLenses ''EnterResult
Lens.makeLenses ''EventResult
Lens.makeLenses ''Focus
Lens.makeLenses ''Widget

instance View.MkView (Widget a) where setView = wView
instance Functor f => View.Pad (Widget (f EventResult)) where pad p = assymetricPad p p

instance View.HasSize (Widget a) where size = wView . View.size
instance EventMap.HasEventMap Widget where eventMap = eventMapMaker . Lens.mapped

isFocused :: Widget a -> Bool
isFocused = Lens.has (mFocus . Lens._Just)

empty :: Widget f
empty = fromView View.empty

eventMapMaker :: Lens.Traversal' (Widget a) (VirtualCursor -> EventMap a)
eventMapMaker = mFocus . Lens._Just . fEventMap

eventResultFromCursor :: Id -> EventResult
eventResultFromCursor c = EventResult
    { _eCursor = Just c & Monoid.Last
    , _eVirtualCursor = Just ResetVirtualCursor & Monoid.Last
    , _eAnimIdMapping = mempty
    }

events :: HasWidget w => Lens.Setter (w a) (w b) a b
events =
    widget . Lens.sets atEvents
    where
        atEvents :: (a -> b) -> Widget a -> Widget b
        atEvents f w =
            w
            { _mEnter =
                  (Lens.mapped . Lens.mapped . enterResultEvent %~ f) $
                  _mEnter w
            , _mFocus =
                w ^. mFocus & Lens._Just . fEventMap . Lens.mapped . Lens.mapped %~ f
            }

fromView :: View -> Widget a
fromView v =
    Widget
    { _mFocus = Nothing
    , _wView = v
    , _mEnter = Nothing
    }

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
        <&> EnterResult (Rect 0 (w ^. View.size))
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
    & mFocus . Lens._Just . focalArea . Rect.topLeft +~ pos
    & mFocus . Lens._Just . fEventMap . Lens.argument . virtualCursor . Rect.topLeft -~ pos
    & Lens.mapped . Lens.mapped . eVirtualCursor . Lens.mapped .
      _NewVirtualCursor . virtualCursor . Rect.topLeft +~ pos
    & View.setView %~ View.translate pos

scale :: Functor f => Vector2 R -> Widget (f EventResult) -> Widget (f EventResult)
scale mult w =
    w
    & View.setView %~ View.scale mult
    & mFocus . Lens._Just . focalArea . Rect.topLeftAndSize *~ mult
    & mFocus . Lens._Just . fEventMap . Lens.argument . virtualCursor . Rect.topLeftAndSize //~ mult
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
setFocused =
    widget %~
    \w ->
    w & mFocus .~
    Just Focus
    { _focalArea = Rect 0 (w ^. View.size)
    , _fEventMap = mempty
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
    maybe mempty renderCursor (w ^? mFocus . Lens._Just . focalArea)
    & (`mappend` View.render (w ^. wView))
    where
        renderCursor area =
            Anim.backgroundColor cursorAnimId cursorColor
            (area ^. Rect.size)
            & Anim.translate (area ^. Rect.topLeft)
