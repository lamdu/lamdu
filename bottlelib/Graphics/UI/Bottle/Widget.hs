{-# LANGUAGE DeriveFunctor, FlexibleInstances, MultiParamTypeClasses, TemplateHaskell, GeneralizedNewtypeDeriving, DeriveGeneric #-}
module Graphics.UI.Bottle.Widget
    ( module Graphics.UI.Bottle.WidgetId

    -- Types:
    , R, Size

    , EnterResult(..), enterResultEvent, enterResultRect

    -- Event Result:
    , EventResult(..), eCursor, eAnimIdMapping
    , eventResultFromCursor
    , applyIdMapping
    , animIdMappingFromPrefixMap

    -- Events:
    , EventHandlers
    , keysEventMap
    , keysEventMapMovesCursor

    -- Widget type and lenses:
    , Widget(..), isFocused, view, mEnter, eventMap, focalArea
    , animLayers, animFrame, size, width, height, events

    -- Construct widgets:
    , empty
    , fromView

    -- Focus handlers:
    , takesFocus, doesntTakeFocus

    -- Operations:
    , strongerEvents, weakerEvents
    , translate, scale
    , pad, assymetricPad, padToSizeAlign
    , addInnerFrame
    , backgroundColor
    , tint

    -- Env:
    , Env(..), envCursor, envCursorAnimId

    , respondToCursorAt
    , respondToCursorPrefix
    , respondToCursorBy
    , respondToCursor
    ) where

import           Control.Applicative ((<$>), (<*>), liftA2)
import           Control.Lens (Lens')
import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Monoid (Monoid(..))
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
import           Graphics.UI.Bottle.ModKey (ModKey)
import           Graphics.UI.Bottle.Rect (Rect(..))
import qualified Graphics.UI.Bottle.Rect as Rect
import           Graphics.UI.Bottle.View (View(..))
import qualified Graphics.UI.Bottle.View as View
import           Graphics.UI.Bottle.WidgetId

data EventResult = EventResult
    { _eCursor :: Monoid.Last Id
    , _eAnimIdMapping :: Monoid.Endo AnimId
    } deriving (Generic)
instance Monoid EventResult where
    mempty = def_mempty
    mappend = def_mappend

data EnterResult f = EnterResult
    { _enterResultRect :: Rect
    , _enterResultEvent :: f EventResult
    }

type EventHandlers f = EventMap (f EventResult)

data Widget f = Widget
    { _isFocused :: Bool
    , _view :: View
    , _mEnter :: Maybe (Direction -> EnterResult f) -- Nothing if we're not enterable
    , _eventMap :: EventHandlers f
    , _focalArea :: Rect
    }

-- When focused, mEnter may still be relevant, e.g: FlyNav in an
-- active textedit, to move to a different text-edit position.

-- When unfocused, event map is usually irrelevant, but can too be
-- used

Lens.makeLenses ''EnterResult
Lens.makeLenses ''EventResult
Lens.makeLenses ''Widget

empty :: Widget f
empty = Widget False View.empty Nothing mempty (Rect 0 0)

{-# INLINE animFrame #-}
animFrame :: Lens' (Widget f) Anim.Frame
animFrame = view . View.animFrame

{-# INLINE animLayers #-}
animLayers :: Lens.Traversal' (Widget f) Anim.Layer
animLayers = animFrame . Anim.layers

{-# INLINE size #-}
size :: Lens' (Widget f) Size
size = view . View.size

{-# INLINE width #-}
width :: Lens' (Widget f) R
width = view . View.width

{-# INLINE height #-}
height :: Lens' (Widget f) R
height = view . View.height

eventResultFromCursor :: Id -> EventResult
eventResultFromCursor cursor = EventResult
    { _eCursor = Monoid.Last $ Just cursor
    , _eAnimIdMapping = mempty
    }

events :: Lens.Setter (Widget f) (Widget g) (f EventResult) (g EventResult)
events =
    Lens.sets atEvents
    where
        atEvents f widget = widget
            { _mEnter =
                      (Lens.mapped . Lens.mapped . enterResultEvent %~ f) $
                      _mEnter widget
            , _eventMap = f <$> _eventMap widget
            }

fromView :: View -> Widget f
fromView v =
    Widget
        { _isFocused = False
        , _focalArea = Rect 0 (v ^. View.size)
        , _view = v
        , _eventMap = mempty
        , _mEnter = Nothing
        }

takesFocus :: Functor f => (Direction -> f Id) -> Widget f -> Widget f
takesFocus enterFunc widget =
    widget & mEnter .~ Just enter
    where
        enter =
            enterFunc
            <&> Lens.mapped %~ eventResultFromCursor
            <&> EnterResult (widget ^. focalArea)

doesntTakeFocus :: Widget f -> Widget f
doesntTakeFocus = mEnter .~ Nothing

-- ^ If doesn't take focus, event map is ignored
strongerEvents :: EventHandlers f -> Widget f -> Widget f
strongerEvents eMap = eventMap %~ (eMap `mappend`)

-- ^ If doesn't take focus, event map is ignored
weakerEvents :: EventHandlers f -> Widget f -> Widget f
weakerEvents eMap = eventMap %~ (`mappend` eMap)

backgroundColor :: Int -> AnimId -> Draw.Color -> Widget f -> Widget f
backgroundColor layer animId color =
    view %~ View.backgroundColor animId layer color

addInnerFrame :: Int -> AnimId -> Draw.Color -> Vector2 R -> Widget f -> Widget f
addInnerFrame layer animId color frameWidth widget =
    widget & animFrame %~ mappend emptyRectangle
    where
        emptyRectangle =
            Anim.emptyRectangle frameWidth (widget ^. size) animId
            & Anim.unitImages %~ Draw.tint color
            & Anim.layers +~ layer

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

tint :: Draw.Color -> Widget f -> Widget f
tint color = animFrame . Anim.unitImages %~ Draw.tint color

keysEventMap ::
    Functor f => [ModKey] -> EventMap.Doc ->
    f () -> EventHandlers f
keysEventMap keys doc act =
    (fmap . const) mempty <$>
    EventMap.keyPresses keys doc act

keysEventMapMovesCursor ::
    Functor f => [ModKey] -> EventMap.Doc ->
    f Id -> EventHandlers f
keysEventMapMovesCursor keys doc act =
    fmap eventResultFromCursor <$>
    EventMap.keyPresses keys doc act

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
    & focalArea . Rect.topLeft +~ pos
    & animFrame %~ Anim.translate pos

scale :: Vector2 R -> Widget f -> Widget f
scale mult widget =
    widget
    & view %~ View.scale mult
    & focalArea . Rect.topLeftAndSize *~ mult
    & mEnter . Lens._Just . Lens.mapped . enterResultRect . Rect.topLeftAndSize *~ mult
    & mEnter . Lens._Just . Lens.argument . Direction.coordinates . Rect.topLeftAndSize //~ mult

-- Surround a widget with padding
pad :: Vector2 R -> Widget f -> Widget f
pad p = assymetricPad p p

assymetricPad :: Vector2 R -> Vector2 R -> Widget f -> Widget f
assymetricPad leftAndTop rightAndBottom widget =
    widget
    & size +~ leftAndTop + rightAndBottom
    & translate leftAndTop

padToSizeAlign :: Size -> Vector2 R -> Widget f -> Widget f
padToSizeAlign newSize alignment widget =
    widget
    & translate (sizeDiff * alignment)
    & size %~ liftA2 max newSize
    where
        sizeDiff = max <$> 0 <*> newSize - widget ^. size

data Env = Env
    { -- | Where the cursor is pointing:
        _envCursor :: Id
    , -- | What animId to use when drawing a cursor anim frame:
        _envCursorAnimId :: AnimId
    } deriving (Show, Eq, Ord)
Lens.makeLenses ''Env

respondToCursorPrefix ::
    Id -> Draw.Color -> Anim.Layer -> Env ->
    Widget f -> Widget f
respondToCursorPrefix myIdPrefix =
    respondToCursorBy (Lens.has Lens._Just . subId myIdPrefix)

respondToCursorAt ::
    Id -> Draw.Color -> Anim.Layer -> Env ->
    Widget f -> Widget f
respondToCursorAt wId = respondToCursorBy (== wId)

respondToCursorBy ::
    (Id -> Bool) -> Draw.Color -> Anim.Layer -> Env ->
    Widget f -> Widget f
respondToCursorBy f color layer env widget
    | f (env ^. envCursor) =
        widget & respondToCursor color layer (env ^. envCursorAnimId)
    | otherwise = widget

respondToCursor ::
    Draw.Color -> Anim.Layer -> AnimId -> Widget f -> Widget f
respondToCursor color layer animId widget =
    widget
    & backgroundColor layer animId color
    & isFocused .~ True
