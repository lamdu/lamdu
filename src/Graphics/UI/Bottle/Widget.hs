{-# LANGUAGE NoImplicitPrelude, DeriveFunctor, TemplateHaskell, GeneralizedNewtypeDeriving, DeriveGeneric, OverloadedStrings, NamedFieldPuns, LambdaCase #-}
module Graphics.UI.Bottle.Widget
    ( Id(..), subId, Id.joinId, isSubCursor
    , HasCursor(..)

    -- Types:
    , R, Size

    , EnterResult(..), enterResultEvent, enterResultRect

    -- Event Result:
    , EventResult(..), eCursor, eAnimIdMapping
    , eventResultFromCursor
    , applyIdMapping
    , animIdMappingFromPrefixMap

    -- Events:
    , EventMap
    , keysEventMap
    , keysEventMapMovesCursor

    -- Widget type and lenses:
    , Widget(..), view, mEnter, mFocus, eventMap
    , MEnter
    , Focus(..), fEventMap, focalArea
    , size, width, height, events
    , bottomFrame

    , isFocused

    , CursorConfig(..)
    , renderWithCursor, cursorAnimId

    -- Construct widgets:
    , empty
    , fromView

    -- Focus handlers:
    , takesFocus, doesntTakeFocus

    -- Operations:
    , strongerEvents, weakerEvents
    , translate, scale
    , pad, assymetricPad, padToSizeAlign
    , backgroundColor

    , makeFocusableView

    , respondToCursorPrefix
    , respondToCursorBy
    , respondToCursor

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

data EventResult = EventResult
    { _eCursor :: Monoid.Last Id
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
    }

data Focus a = Focus
    { _focalArea :: Rect
    , _fEventMap :: EventMap a
    }

-- When focused, mEnter may still be relevant, e.g: Mouse click in an
-- active textedit, to move to a different text-edit position.
type MEnter a = Maybe (Direction -> EnterResult a)

data Widget a = Widget
    { _view :: View
    , _mEnter :: MEnter a -- Nothing if we're not enterable
    , _mFocus :: Maybe (Focus a)
    }

Lens.makeLenses ''EnterResult
Lens.makeLenses ''EventResult
Lens.makeLenses ''Focus
Lens.makeLenses ''Widget

isFocused :: Widget a -> Bool
isFocused = Lens.has (mFocus . Lens._Just)

empty :: Widget f
empty = fromView View.empty

bottomFrame :: Lens.Traversal' (Widget a) Anim.Frame
bottomFrame = view . View.bottomFrame

{-# INLINE size #-}
size :: Lens' (Widget a) Size
size = view . View.size

{-# INLINE width #-}
width :: Lens' (Widget a) R
width = view . View.width

{-# INLINE height #-}
height :: Lens' (Widget a) R
height = view . View.height

{-# INLINE eventMap #-}
eventMap :: Lens.Traversal' (Widget a) (EventMap a)
eventMap = mFocus . Lens._Just . fEventMap

eventResultFromCursor :: Id -> EventResult
eventResultFromCursor c = EventResult
    { _eCursor = Just c & Monoid.Last
    , _eAnimIdMapping = mempty
    }

events :: Lens.Setter (Widget a) (Widget b) a b
events =
    Lens.sets atEvents
    where
        atEvents :: (a -> b) -> Widget a -> Widget b
        atEvents f widget =
            widget
            { _mEnter =
                  (Lens.mapped . Lens.mapped . enterResultEvent %~ f) $
                  _mEnter widget
            , _mFocus =
                widget ^. mFocus & Lens._Just . fEventMap . Lens.mapped %~ f
            }

fromView :: View -> Widget a
fromView v =
    Widget
    { _mFocus = Nothing
    , _view = v
    , _mEnter = Nothing
    }

takesFocus ::
    Functor f =>
    (Direction -> f Id) -> Widget (f EventResult) -> Widget (f EventResult)
takesFocus enterFunc widget =
    widget & mEnter .~ Just enter
    where
        enter =
            enterFunc
            <&> Lens.mapped %~ eventResultFromCursor
            <&> EnterResult (Rect 0 (widget ^. size))

doesntTakeFocus :: Widget a -> Widget a
doesntTakeFocus = mEnter .~ Nothing

-- ^ If doesn't take focus, does nothing
strongerEvents :: EventMap a -> Widget a -> Widget a
strongerEvents eMap = eventMap %~ (eMap `mappend`)

-- ^ If doesn't take focus, does nothing
weakerEvents :: EventMap a -> Widget a -> Widget a
weakerEvents eMap = eventMap %~ (`mappend` eMap)

backgroundColor ::
    (MonadReader env m, View.HasAnimIdPrefix env) =>
    m (Draw.Color -> Widget a -> Widget a)
backgroundColor = View.backgroundColor <&> Lens.mapped %~ (view %~)

animIdMappingFromPrefixMap :: Map AnimId AnimId -> Monoid.Endo AnimId
animIdMappingFromPrefixMap = Monoid.Endo . Anim.mappingFromPrefixMap

applyIdMapping :: Map Id Id -> EventResult -> EventResult
applyIdMapping widgetIdMap eventResult =
    eventResult
    & eAnimIdMapping <>~ animIdMappingFromPrefixMap animIdMap
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
translate :: Vector2 R -> Widget f -> Widget f
translate pos widget =
    widget
    & mEnter . Lens._Just . Lens.argument .
        Direction.coordinates . Rect.topLeft -~ pos
    & mEnter . Lens._Just . Lens.mapped .
        enterResultRect . Rect.topLeft +~ pos
    & mFocus . Lens._Just . focalArea . Rect.topLeft +~ pos
    & view %~ View.translate pos

scale :: Vector2 R -> Widget a -> Widget a
scale mult widget =
    widget
    & view %~ View.scale mult
    & mFocus . Lens._Just . focalArea . Rect.topLeftAndSize *~ mult
    & mEnter . Lens._Just . Lens.mapped . enterResultRect . Rect.topLeftAndSize *~ mult
    & mEnter . Lens._Just . Lens.argument . Direction.coordinates . Rect.topLeftAndSize //~ mult

-- Surround a widget with padding
pad :: Vector2 R -> Widget a -> Widget a
pad p = assymetricPad p p

assymetricPad :: Vector2 R -> Vector2 R -> Widget a -> Widget a
assymetricPad leftAndTop rightAndBottom widget =
    widget
    & size +~ leftAndTop + rightAndBottom
    & translate leftAndTop

padToSizeAlign :: Size -> Vector2 R -> Widget a -> Widget a
padToSizeAlign newSize alignment widget =
    widget
    & translate (sizeDiff * alignment)
    & size %~ liftA2 max newSize
    where
        sizeDiff = max <$> 0 <*> newSize - widget ^. size

class HasCursor env where cursor :: Lens' env Id

subId :: (MonadReader env m, HasCursor env) => m (Id -> Maybe AnimId)
subId = Lens.view cursor <&> flip Id.subId

isSubCursor :: (MonadReader env m, HasCursor env) => m (Id -> Bool)
isSubCursor = subId <&> \sub prefix -> sub prefix & Lens.has Lens._Just

respondToCursor :: Widget a -> Widget a
respondToCursor widget =
    widget & mFocus .~ Just
        Focus
        { _focalArea = Rect 0 (widget ^. size)
        , _fEventMap = mempty
        }

respondToCursorBy ::
    (MonadReader env m, HasCursor env) =>
    m ((Id -> Bool) -> Widget a -> Widget a)
respondToCursorBy =
    Lens.view cursor
    <&> \c f -> if f c then respondToCursor else id

respondToCursorPrefix ::
    (MonadReader env m, HasCursor env) => m (Id -> Widget a -> Widget a)
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
    (MonadReader env m, HasCursor env, Applicative f) =>
    m (Id -> Widget (f EventResult) -> Widget (f EventResult))
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
renderWithCursor CursorConfig{cursorColor} widget =
    maybe mempty renderCursor (widget ^? mFocus . Lens._Just . focalArea)
    & (`mappend` View.render (widget ^. view))
    where
        renderCursor area =
            Anim.backgroundColor cursorAnimId cursorColor
            (area ^. Rect.size)
            & Anim.translate (area ^. Rect.topLeft)
