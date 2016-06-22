{-# LANGUAGE NoImplicitPrelude, DeriveFunctor, TemplateHaskell, GeneralizedNewtypeDeriving, DeriveGeneric, OverloadedStrings, RecordWildCards #-}
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
    , EventMap
    , keysEventMap
    , keysEventMapMovesCursor

    -- Widget type and lenses:
    , Widget(..), _WidgetFocused, _WidgetNotFocused
    , common, view, mEnter, eventMap
    , WidgetCommon(..), cView, cMEnter
    , FocusedWidget(..), fEventMap, focalArea
    , animLayers, animFrame, size, width, height, events

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
    , addInnerFrame
    , backgroundColor
    , tint

    -- Env:
    , Env(..), envCursor

    , respondToCursorPrefix
    , respondToCursorBy
    , respondToCursor
    ) where

import           Prelude.Compat

import           Control.Applicative (liftA2)
import           Control.Lens (Lens')
import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
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

data EnterResult a = EnterResult
    { -- The new focal area upon this entrace.
      -- Used in Grid to decide which cell's EnterResult to use and in FlyNav.
      _enterResultRect :: Rect
    , _enterResultEvent :: a
    }

-- TODO: Better name for this?
data WidgetCommon a = WidgetCommon
    { _cView :: View
    , _cMEnter :: Maybe (Direction -> EnterResult a) -- Nothing if we're not enterable
    }

data FocusedWidget a = FocusedWidget
    { _focalArea :: Rect
    , _fEventMap :: EventMap a
    , _fCommon :: WidgetCommon a
    }

data Widget a
    = WidgetNotFocused (WidgetCommon a)
    | WidgetFocused (FocusedWidget a)

-- When focused, mEnter may still be relevant, e.g: FlyNav in an
-- active textedit, to move to a different text-edit position.

Lens.makeLenses ''EnterResult
Lens.makeLenses ''EventResult
Lens.makeLenses ''FocusedWidget
Lens.makeLenses ''WidgetCommon
Lens.makePrisms ''Widget

{-# INLINE common #-}
common :: Lens' (Widget a) (WidgetCommon a)
common f (WidgetNotFocused x) = f x <&> WidgetNotFocused
common f (WidgetFocused x) = fCommon f x <&> WidgetFocused

{-# INLINE mEnter #-}
mEnter :: Lens' (Widget a) (Maybe (Direction -> EnterResult a))
mEnter = common . cMEnter

{-# INLINE view #-}
view :: Lens' (Widget a) View
view = common . cView

isFocused :: Widget a -> Bool
isFocused = Lens.has _WidgetFocused

empty :: Widget a
empty =
    WidgetNotFocused WidgetCommon
    { _cView = View.empty
    , _cMEnter = Nothing
    }

{-# INLINE animFrame #-}
animFrame :: Lens' (Widget a) Anim.Frame
animFrame = view . View.animFrame

{-# INLINE animLayers #-}
animLayers :: Lens.Traversal' (Widget a) Anim.Layer
animLayers = animFrame . Anim.layers

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
eventMap = _WidgetFocused . fEventMap

eventResultFromCursor :: Id -> EventResult
eventResultFromCursor cursor = EventResult
    { _eCursor = Monoid.Last $ Just cursor
    , _eAnimIdMapping = mempty
    }

events :: Lens.Setter (Widget a) (Widget b) a b
events =
    Lens.sets atEvents
    where
        commonEvents = cMEnter . Lens.mapped . Lens.mapped . enterResultEvent
        atEvents :: (a -> b) -> Widget a -> Widget b
        atEvents f (WidgetNotFocused x) =
            x
            & commonEvents %~ f
            & WidgetNotFocused
        atEvents f (WidgetFocused x) =
            WidgetFocused x
            { _fEventMap = x ^. fEventMap <&> f
            , _fCommon = x ^. fCommon & commonEvents %~ f
            }

fromView :: View -> Widget a
fromView v =
    WidgetNotFocused WidgetCommon
    { _cView = v
    , _cMEnter = Nothing
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

backgroundColor :: Int -> AnimId -> Draw.Color -> Widget a -> Widget a
backgroundColor layer animId color =
    view %~ View.backgroundColor animId layer color

addInnerFrame :: Int -> AnimId -> Draw.Color -> Vector2 R -> Widget a -> Widget a
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

tint :: Draw.Color -> Widget a -> Widget a
tint color = view %~ View.tint color

keysEventMap ::
    Functor f => [ModKey] -> EventMap.Doc ->
    f () -> EventMap (f EventResult)
keysEventMap keys doc act =
    (fmap . const) mempty <$>
    EventMap.keyPresses keys doc act

keysEventMapMovesCursor ::
    Functor f => [ModKey] -> EventMap.Doc ->
    f Id -> EventMap (f EventResult)
keysEventMapMovesCursor keys doc act =
    fmap eventResultFromCursor <$>
    EventMap.keyPresses keys doc act

-- TODO: This actually makes an incorrect widget because its size
-- remains same, but it is now translated away from 0..size
-- Should expose higher-level combinators instead?
translate :: Vector2 R -> Widget a -> Widget a
translate pos widget =
    widget
    & mEnter . Lens._Just %~ onEnter
    & _WidgetFocused . focalArea . Rect.topLeft +~ pos
    & view %~ View.translate pos
    where
        onEnter x =
            x
            & Lens.argument . Direction.coordinates . Rect.topLeft -~ pos
            <&> enterResultRect . Rect.topLeft +~ pos

scale :: Vector2 R -> Widget a -> Widget a
scale mult widget =
    widget
    & view %~ View.scale mult
    & _WidgetFocused . focalArea . Rect.topLeftAndSize *~ mult
    & mEnter . Lens._Just %~ onEnter
    where
        onEnter x =
            x
            <&> enterResultRect . Rect.topLeftAndSize *~ mult
            & Lens.argument . Direction.coordinates . Rect.topLeftAndSize //~ mult

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

data Env = Env
    { -- | Where the cursor is pointing:
        _envCursor :: Id
    } deriving (Show, Eq, Ord)
Lens.makeLenses ''Env

respondToCursorPrefix :: Id -> Env -> Widget a -> Widget a
respondToCursorPrefix myIdPrefix =
    respondToCursorBy (Lens.has Lens._Just . subId myIdPrefix)

respondToCursorBy :: (Id -> Bool) -> Env -> Widget a -> Widget a
respondToCursorBy f env
    | f (env ^. envCursor) = respondToCursor
    | otherwise = id

respondToCursor :: Widget a -> Widget a
respondToCursor (WidgetNotFocused x) =
    WidgetFocused FocusedWidget
    { _focalArea = Rect 0 (x ^. cView . View.size)
    , _fEventMap = mempty
    , _fCommon = x
    }
respondToCursor (WidgetFocused x) =
    x
    & focalArea .~ Rect 0 (x ^. fCommon . cView . View.size)
    & WidgetFocused

cursorAnimId :: AnimId
cursorAnimId = ["background"]

newtype CursorConfig = CursorConfig
    { cursorColor :: Draw.Color
    }

renderWithCursor :: CursorConfig -> Widget a -> Anim.Frame
renderWithCursor CursorConfig{..} widget =
    maybe mempty renderCursor (widget ^? _WidgetFocused . focalArea)
    & mappend (widget ^. animFrame)
    where
        minLayer = fromMaybe 0 (Lens.minimumOf animLayers widget)
        cursorLayer = minLayer - 1
        renderCursor area =
            Anim.backgroundColor cursorAnimId cursorLayer cursorColor
            (area ^. Rect.size)
            & Anim.translate (area ^. Rect.topLeft)
