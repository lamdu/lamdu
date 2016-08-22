{-# LANGUAGE NoImplicitPrelude, DeriveFunctor, TemplateHaskell, FlexibleContexts, GeneralizedNewtypeDeriving, DeriveGeneric, OverloadedStrings, RecordWildCards, RankNTypes, EmptyDataDecls, GADTs, ConstraintKinds #-}
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
    , Widget, WidgetF(..), _WidgetFocused, _WidgetNotFocused
    , WidgetFConstraints
    , view, mEnter, eventMap, widgetFocus
    , WidgetData(..), wView, wMEnter, wFocus
    , Focus(..), TagFocused, TagNotFocused, focusData
    , FocusData(..), fEventMap, focalArea
    , animLayers, animFrame, size, width, height

    , widgetF, widgetData, onWidgetData
    , hoist, sequenced
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

import           Control.Applicative (liftA2)
import           Control.Lens (LensLike, LensLike')
import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Data.Functor.Identity (Identity(..))
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import qualified Data.Monoid as Monoid
import           Data.Monoid.Generic (def_mempty, def_mappend)
import           Data.Traversable.Generalized (GTraversable(..))
import qualified Data.Traversable.Generalized as GTraversable
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

import           Prelude.Compat

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
    } deriving Functor

data FocusData a = FocusData
    { _focalArea :: Rect
    , _fEventMap :: EventMap a
    } deriving Functor

data TagFocused
data TagNotFocused

data Focus tag a where
    NoFocusData :: Focus TagNotFocused a
    HasFocusData :: FocusData a -> Focus TagFocused a

instance Functor (Focus tag) where
    fmap _ NoFocusData = NoFocusData
    fmap f (HasFocusData x) = x <&> f & HasFocusData

focusData :: Lens.Traversal (Focus tag a) (Focus tag b) (FocusData a) (FocusData b)
focusData _ NoFocusData = pure NoFocusData
focusData f (HasFocusData d) = f d <&> HasFocusData

-- TODO: Better name for this?
data WidgetData focusTag a = WidgetData
    { _wView :: View
    -- When focused, mEnter may still be relevant, e.g: FlyNav in an
    -- active textedit, to move to a different text-edit position:
    , _wMEnter :: Maybe (Direction -> EnterResult a) -- Nothing if we're not enterable
    , _wFocus :: Focus focusTag a
    } deriving Functor

data WidgetF t a
    = WidgetNotFocused (t (WidgetData TagNotFocused a))
    | WidgetFocused (t (WidgetData TagFocused a))
    deriving Functor

type Widget = WidgetF Identity

type WidgetFConstraints t a =
    ( GTraversable.Constraints t (Lens.Const Anim.Frame)
    , GTraversable.Constraints t (Lens.Const (Maybe (Direction -> EnterResult a)))
    , GTraversable.Constraints t (Lens.Const (Monoid.First (FocusData a)))
    )

Lens.makeLenses ''EnterResult
Lens.makeLenses ''EventResult
Lens.makePrisms ''Focus
Lens.makeLenses ''FocusData
Lens.makeLenses ''WidgetData
Lens.makePrisms ''WidgetF

{-# INLINE widgetF #-}
widgetF ::
    Functor f =>
    (forall tag. t (WidgetData tag a) -> f (s (WidgetData tag b))) ->
    WidgetF t a -> f (WidgetF s b)
widgetF f (WidgetFocused v) = f v <&> WidgetFocused
widgetF f (WidgetNotFocused v) = f v <&> WidgetNotFocused

widgetData ::
    GTraversable.Constraints t f =>
    (forall tag. WidgetData tag a -> f (WidgetData tag b)) ->
    WidgetF t a -> f (WidgetF t b)
widgetData f = widgetF (gTraverse f)

onWidgetData ::
    GTraversable t =>
    (forall tag. WidgetData tag a -> WidgetData tag b) ->
    WidgetF t a -> WidgetF t b
onWidgetData f = runIdentity . widgetData (Identity . f)

-- TODO: better name for this?
{-# INLINE sequenced #-}
sequenced ::
    (Functor f, Functor t, GTraversable.Constraints s (Lens.Const (Widget b))) =>
    LensLike f (WidgetF t a) (WidgetF s b) (t (Widget a)) (s (Widget b))
sequenced f w =
    f (toFWidget w) <&> toWidgetF
    where
        toFWidget (WidgetNotFocused x) = x <&> WidgetNotFocused . Identity
        toFWidget (WidgetFocused x) = x <&> WidgetFocused . Identity
        toWidgetF x =
            case x ^. gTraverse of
            WidgetFocused (Identity y) -> WidgetFocused (x & gTraverse .~ y)
            WidgetNotFocused (Identity y) -> WidgetNotFocused (x & gTraverse .~ y)

{-# INLINE hoist #-}
hoist :: (forall tag. t (WidgetData tag a) -> s (WidgetData tag b)) -> WidgetF t a -> WidgetF s b
hoist f = runIdentity . widgetF (Identity . f)

{-# INLINE mEnter #-}
mEnter ::
    GTraversable.Constraints t f =>
    LensLike' f (WidgetF t a) (Maybe (Direction -> EnterResult a))
mEnter f = widgetData (wMEnter f)

{-# INLINE view #-}
view :: GTraversable.Constraints t f => LensLike' f (WidgetF t a) View
view f = widgetData (wView f)

isFocused :: WidgetF t a -> Bool
isFocused = Lens.has _WidgetFocused

empty :: Applicative t => WidgetF t a
empty = fromView View.empty

{-# INLINE animFrame #-}
animFrame :: GTraversable.Constraints t f => LensLike' f (WidgetF t a) Anim.Frame
animFrame = view . View.animFrame

{-# INLINE animLayers #-}
animLayers ::
    (GTraversable.Constraints t f, Applicative f) =>
    Lens.LensLike' f (WidgetF t a) Anim.Layer
animLayers = animFrame . Anim.layers

{-# INLINE size #-}
size :: GTraversable.Constraints t f => LensLike' f (WidgetF t a) Size
size = view . View.size

{-# INLINE width #-}
width :: GTraversable.Constraints t f => LensLike' f (WidgetF t a) R
width = view . View.width

{-# INLINE height #-}
height :: GTraversable.Constraints t f => LensLike' f (WidgetF t a) R
height = view . View.height

{-# INLINE widgetFocus #-}
widgetFocus ::
    (GTraversable.Constraints t f, Applicative f) =>
    LensLike' f (WidgetF t a) (FocusData a)
widgetFocus = _WidgetFocused . gTraverse . wFocus . focusData

{-# INLINE eventMap #-}
eventMap ::
    (GTraversable.Constraints t f, Applicative f) =>
    Lens.LensLike' f (WidgetF t a) (EventMap a)
eventMap = widgetFocus . fEventMap

eventResultFromCursor :: Id -> EventResult
eventResultFromCursor cursor = EventResult
    { _eCursor = Monoid.Last $ Just cursor
    , _eAnimIdMapping = mempty
    }

fromView :: Applicative t => View -> WidgetF t a
fromView v =
    pure WidgetData
    { _wView = v
    , _wMEnter = Nothing
    , _wFocus = NoFocusData
    } & WidgetNotFocused

wTakesFocus ::
    Functor f =>
    (Direction -> f Id) ->
    WidgetData tag (f EventResult) ->
    WidgetData tag (f EventResult)
wTakesFocus enterFunc wd =
    wd & wMEnter .~ Just enter
    where
        enter =
            enterFunc
            <&> Lens.mapped %~ eventResultFromCursor
            <&> EnterResult (Rect 0 (wd ^. wView . View.size))

takesFocus ::
    (Functor f, GTraversable t) =>
    (Direction -> f Id) -> WidgetF t (f EventResult) -> WidgetF t (f EventResult)
takesFocus enterFunc = onWidgetData (wTakesFocus enterFunc)

doesntTakeFocus :: GTraversable t => WidgetF t a -> WidgetF t a
doesntTakeFocus = mEnter .~ Nothing

-- ^ If doesn't take focus, does nothing
strongerEvents :: GTraversable t => EventMap a -> WidgetF t a -> WidgetF t a
strongerEvents eMap = eventMap %~ (eMap `mappend`)

-- ^ If doesn't take focus, does nothing
weakerEvents :: GTraversable t => EventMap a -> WidgetF t a -> WidgetF t a
weakerEvents eMap = eventMap %~ (`mappend` eMap)

backgroundColor ::
    GTraversable t => Int -> AnimId -> Draw.Color -> WidgetF t a -> WidgetF t a
backgroundColor layer animId color =
    view %~ View.backgroundColor animId layer color

addInnerFrame ::
    (GTraversable.Constraints t (Lens.Const (Vector2 R))) =>
    Int -> AnimId -> Draw.Color -> Vector2 R -> WidgetF t a -> WidgetF t a
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

tint :: GTraversable t => Draw.Color -> WidgetF t a -> WidgetF t a
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

-- TODO: On its own this makes an incorrect widget because its size
-- remains same, but it is now translated away from 0..size
translate :: Vector2 R -> WidgetData tag a -> WidgetData tag a
translate pos w =
    w
    & wMEnter . Lens._Just %~ onEnter
    & wFocus . focusData . focalArea . Rect.topLeft +~ pos
    & wView %~ View.translate pos
    where
        onEnter x =
            x
            & Lens.argument . Direction.coordinates . Rect.topLeft -~ pos
            <&> enterResultRect . Rect.topLeft +~ pos

scale :: GTraversable t => Vector2 R -> WidgetF t a -> WidgetF t a
scale mult widget =
    widget
    & view %~ View.scale mult
    & widgetFocus . focalArea . Rect.topLeftAndSize *~ mult
    & mEnter . Lens._Just %~ onEnter
    where
        onEnter x =
            x
            <&> enterResultRect . Rect.topLeftAndSize *~ mult
            & Lens.argument . Direction.coordinates . Rect.topLeftAndSize //~ mult

-- Surround a widget with padding
pad :: Vector2 R -> WidgetData fo a -> WidgetData fo a
pad p = assymetricPad p p

assymetricPad :: Vector2 R -> Vector2 R -> WidgetData fo a -> WidgetData fo a
assymetricPad leftAndTop rightAndBottom wd =
    wd
    & wView . View.size +~ leftAndTop + rightAndBottom
    & translate leftAndTop

padToSizeAlign :: Size -> Vector2 R -> Widget a -> Widget a
padToSizeAlign newSize alignment widget =
    widget
    & onWidgetData (translate (sizeDiff * alignment))
    & size %~ liftA2 max newSize
    where
        sizeDiff = max <$> 0 <*> newSize - widget ^. size

data Env = Env
    { -- | Where the cursor is pointing:
        _envCursor :: Id
    } deriving (Show, Eq, Ord)
Lens.makeLenses ''Env

respondToCursorPrefix ::
    GTraversable t => Id -> Env -> WidgetF t a -> WidgetF t a
respondToCursorPrefix myIdPrefix =
    respondToCursorBy (Lens.has Lens._Just . subId myIdPrefix)

respondToCursorBy ::
    GTraversable t => (Id -> Bool) -> Env -> WidgetF t a -> WidgetF t a
respondToCursorBy f env
    | f (env ^. envCursor) = respondToCursor
    | otherwise = id

respondToCursor :: GTraversable t => WidgetF t a -> WidgetF t a
respondToCursor (WidgetNotFocused x) =
    x <&> mkFocused & WidgetFocused
    where
        mkFocused w =
            w & wFocus .~
            HasFocusData FocusData
            { _focalArea = Rect 0 (w ^. wView . View.size)
            , _fEventMap = mempty
            }
respondToCursor (WidgetFocused x) =
    x <&> onFocused & WidgetFocused
    where
        onFocused w =
            w & wFocus . focusData . focalArea .~ Rect 0 (w ^. wView . View.size)

cursorAnimId :: AnimId
cursorAnimId = ["background"]

newtype CursorConfig = CursorConfig
    { cursorColor :: Draw.Color
    }

renderWithCursor :: CursorConfig -> Widget a -> Anim.Frame
renderWithCursor CursorConfig{..} widget =
    maybe mempty renderCursor (widget ^? widgetFocus . focalArea)
    & mappend (widget ^. animFrame)
    where
        minLayer = fromMaybe 0 (Lens.minimumOf animLayers widget)
        cursorLayer = minLayer - 1
        renderCursor area =
            Anim.backgroundColor cursorAnimId cursorLayer cursorColor
            (area ^. Rect.size)
            & Anim.translate (area ^. Rect.topLeft)
