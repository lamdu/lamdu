{-# LANGUAGE NoImplicitPrelude #-}
module GUI.Momentu.Widget
    ( module Types
    , Id(..), Id.joinId, makeSubId

    -- Types:
    , R, Size

    -- Event Result:
    , applyIdMapping

    -- Events:
    , EventMap
    , keysEventMap
    , keysEventMapMovesCursor

    -- Widget lenses:
    , enterResultCursor, sizedState

    , HasWidget(..)

    , isFocused

    -- Construct widgets:
    , fromView

    -- Focus handlers:
    , takesFocus
    , enterFuncAddVirtualCursor

    -- Operations:
    , translate
    , translateFocused, combineEnterPoints
    , padToSizeAlign

    , makeFocusableView

    , respondToCursorPrefix
    , respondToCursorBy
    , setFocused, setFocusedWith

    , glueStates
    ) where

import qualified Control.Lens as Lens
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
import           GUI.Momentu.State (VirtualCursor(..), Update, HasCursor(..))
import qualified GUI.Momentu.State as State
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

enterResultCursor :: (HasWidget w, Functor f) => Lens.Setter' (w (f Update)) Id
enterResultCursor =
    widget . enterResult . enterResultEvent . Lens.mapped . State.uCursor . Lens.mapped

takesFocus ::
    (HasWidget w, Functor f) =>
    (Direction -> f Id) -> w (f Update) -> w (f Update)
takesFocus enterFunc =
    widget %~
    \w ->
        let rect = Rect 0 (w ^. Element.size)
        in
        w
        & wState . _StateUnfocused . uMEnter ?~
        ( enterFunc
            <&> Lens.mapped %~ State.updateCursor
            <&> EnterResult rect 0
            & enterFuncAddVirtualCursor rect
        )

enterFuncAddVirtualCursor ::
    Functor f =>
    Rect -> (Direction -> EnterResult (f Update)) -> (Direction -> EnterResult (f Update))
enterFuncAddVirtualCursor destRect =
    Lens.imapped <. (enterResultEvent . Lens.mapped . State.uVirtualCursor . Lens._Wrapped) .@~ mkVirtCursor
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

applyIdMapping :: Map Id Id -> Update -> Update
applyIdMapping widgetIdMap eventResult =
    eventResult
    & State.uAnimIdMapping <>~ Monoid.Endo (Anim.mappingFromPrefixMap animIdMap)
    & State.uCursor . Lens._Wrapped' . Lens._Just %~ mapCursor
    where
        animIdMap =
            widgetIdMap
            & Map.mapKeys toAnimId & Map.map toAnimId
        mapCursor (Id oldCursor) =
            Id $ Anim.mappingFromPrefixMap animIdMap oldCursor

keysEventMap ::
    Functor f => [MetaKey] -> EventMap.Doc ->
    f () -> EventMap (f Update)
keysEventMap keys doc act =
    (fmap . const) mempty <$>
    EventMap.keyPresses (keys <&> toModKey) doc act

keysEventMapMovesCursor ::
    Functor f => [MetaKey] -> EventMap.Doc ->
    f Id -> EventMap (f Update)
keysEventMapMovesCursor keys doc act =
    fmap State.updateCursor <$>
    EventMap.keyPresses (keys <&> toModKey) doc act

translateFocused ::
    Functor f =>
    Vector2 R -> (Surrounding -> Focused (f Update)) ->
    Surrounding -> Focused (f Update)
translateFocused pos = translateFocusedGeneric (fmap (translateUpdate pos)) pos

padToSizeAlign ::
    Functor f => Size -> Vector2 R -> Widget (f Update) -> Widget (f Update)
padToSizeAlign newSize alignment w =
    w
    & wState .~ translate (sizeDiff * alignment) w
    & Element.size %~ (max <$> newSize <*>)
    where
        sizeDiff = max <$> 0 <*> newSize - w ^. Element.size

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
        , _fMEnterPoint = u ^. uMEnter <&> (. Direction.Point)
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

makeSubId :: (MonadReader env m, Element.HasAnimIdPrefix env) => AnimId -> m Id
makeSubId suffix = Lens.view Element.animIdPrefix <&> (++ suffix) <&> Id

makeFocusableView ::
    (MonadReader env m, HasCursor env, Applicative f) =>
    m (Id -> View -> Widget (f Update))
makeFocusableView =
    respondToCursorPrefix
    <&> \respond myIdPrefix view ->
    fromView view
    & respond myIdPrefix
    & takesFocus (const (pure myIdPrefix))
