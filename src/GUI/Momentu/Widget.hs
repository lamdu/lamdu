{-# LANGUAGE NoImplicitPrelude #-}
module GUI.Momentu.Widget
    ( module Types
    , module State
    , subId, Id(..), Id.joinId, isSubCursor, makeSubId
    , HasCursor(..)

    -- Types:
    , R, Size

    -- Event Result:
    , applyIdMapping

    -- Events:
    , EventMap
    , keysEventMap
    , keysEventMapMovesCursor

    -- Widget lenses:
    , mEnter, events, enterResultCursor, sizedState

    , HasWidget(..)

    , isFocused

    -- Construct widgets:
    , fromView

    -- Focus handlers:
    , takesFocus
    , enterFuncAddVirtualCursor

    -- Operations:
    , translate
    , translateFocused
    , padToSizeAlign

    , makeFocusableView

    , respondToCursorPrefix
    , respondToCursorBy
    , setFocused, setFocusedWith

    , assignCursor
    , assignCursorPrefix

    , glueStates
    ) where

import qualified Control.Lens as Lens
import qualified Control.Monad.Reader as Reader
import qualified Data.Map as Map
import qualified Data.Monoid as Monoid
import           Data.Vector.Vector2 (Vector2(..))
import           GUI.Momentu.Animation (AnimId, R, Size)
import qualified GUI.Momentu.Animation as Anim
import           GUI.Momentu.Direction (Direction)
import qualified GUI.Momentu.Direction as Direction
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.EventMap as EventMap
import           GUI.Momentu.MetaKey (MetaKey, toModKey)
import           GUI.Momentu.State as State
import           GUI.Momentu.Rect (Rect(..))
import qualified GUI.Momentu.Rect as Rect
import           GUI.Momentu.View (View(..))
import           GUI.Momentu.Widget.Id (Id(..))
import qualified GUI.Momentu.Widget.Id as Id
import           GUI.Momentu.Widget.Instances as Instances
import           GUI.Momentu.Widget.Types as Types

import           Lamdu.Prelude

class HasWidget w where widget :: Lens.Setter (w a) (w b) (Widget a) (Widget b)
instance HasWidget Widget where widget = id

isFocused :: Widget a -> Bool
isFocused = Lens.has (wState . _StateFocused)

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

enterResultCursor :: (HasWidget w, Functor f) => Lens.Setter' (w (f EventResult)) Id
enterResultCursor =
    widget . mEnter . Lens._Just . Lens.mapped .
    enterResultEvent . Lens.mapped . eCursor . Lens.mapped

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

translateFocused ::
    Functor f =>
    Vector2 R -> (Surrounding -> Focused (f EventResult)) ->
    Surrounding -> Focused (f EventResult)
translateFocused pos = translateFocusedGeneric (fmap (translateEventResult pos)) pos

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

makeSubId :: (MonadReader env m, Element.HasAnimIdPrefix env) => AnimId -> m Id
makeSubId suffix = Lens.view Element.animIdPrefix <&> (++ suffix) <&> Id

makeFocusableView ::
    (MonadReader env m, HasCursor env, Applicative f) =>
    m (Id -> View -> Widget (f EventResult))
makeFocusableView =
    respondToCursorPrefix
    <&> \respond myIdPrefix view ->
    fromView view
    & respond myIdPrefix
    & takesFocus (const (pure myIdPrefix))
