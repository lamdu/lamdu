{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module GUI.Momentu.Widget
    ( module Types

    , Id(..), Id.joinId, makeSubId

    -- Types:
    , R, Size

    -- Widget lenses:
    , enterResultCursor, sizedState

    , HasWidget(..)

    , isFocused

    -- Construct widgets:
    , fromView

    -- Focus handlers:
    , takesFocus
    , enterFuncAddVirtualCursor

    -- Event maps:
    , strongerEventsWithoutPreevents
    , weakerEventsWithoutPreevents

    , strongerEvents
    , weakerEvents
    , weakerEventsWithContext
    , addPreEvent, addPreEventWith
    , eventMapMaker

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

import           Control.Applicative (liftA2)
import qualified Control.Lens as Lens
import qualified Data.Text as Text
import           Data.Vector.Vector2 (Vector2(..))
import           GUI.Momentu.Animation (AnimId, R, Size)
import           GUI.Momentu.Direction (Direction)
import qualified GUI.Momentu.Direction as Direction
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.Rect (Rect(..))
import qualified GUI.Momentu.Rect as Rect
import           GUI.Momentu.State (VirtualCursor(..), Update, HasCursor(..))
import qualified GUI.Momentu.State as State
import           GUI.Momentu.View (View(..))
import           GUI.Momentu.Widget.Id (Id(..))
import qualified GUI.Momentu.Widget.Id as Id
import           GUI.Momentu.Widget.Instances
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
            enter =
                enterFunc
                <&> Lens.mapped %~ State.updateCursor
                <&> EnterResult rect 0
                & enterFuncAddVirtualCursor rect
        in  w
            & wState . _StateFocused . Lens.mapped . fMEnterPoint %~
                Just . fromMaybe (enter . Direction.Point)
            & wState . _StateUnfocused . uMEnter ?~ enter

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

-- | Take a manual `mappend` function to avoid needing "Monoid (f a)"
-- constraint in callers, who can give the Applicative-Monoid instance
-- for a generic Applicative without requiring a cumbersome
-- "Applicative (f a)" constraint
addPreEventToEventMap :: (a -> a -> a) -> PreEvent a -> EventMap a -> EventMap a
addPreEventToEventMap append preEvent e =
    e
    & actionText %~ concatDescs (preEvent ^. pDesc)
    <&> append (preEvent ^. pAction)
    where
        actionText = E.emDocs . E.docStrs . Lens.reversed . Lens.element 0
        concatDescs x y = filter (not . Text.null) [x, y] & Text.intercalate ", "

-- | New pre-event is not added to the pre-events if pre-event is
-- BlockEvents (but still added to the event map)
addPreEventWith :: (a -> a -> a) -> PreEvent a -> Widget a -> Widget a
addPreEventWith append preEvent =
    wState . _StateFocused . Lens.mapped %~ onFocused
    where
        onFocused f =
            f
            & fPreEvents . _PreEvents %~ (preEvent :)
            & fEventMap %~ onMkEventMap
        onMkEventMap mk ctx =
            ctx
            & ePrevTextRemainder %~ (preEvent ^. pTextRemainder <>)
            & mk
            & addPreEventToEventMap append preEvent

addPreEvent :: Monoid a => PreEvent a -> Widget a -> Widget a
addPreEvent = addPreEventWith mappend

addEventsWithContext ::
    (Applicative f, Monoid a, HasWidget w) =>
    (EventMap (f a) -> EventMap (f a) -> EventMap (f a)) ->
    (EventContext -> EventMap (f a)) -> w (f a) -> w (f a)
addEventsWithContext append mkEvents =
    widget . wState . _StateFocused . Lens.mapped %~ onFocused
    where
        onFocused f =
            case f ^. fPreEvents of
            BlockEvents -> f
            PreEvents es ->
                f & fEventMap . Lens.imapped %@~ add
                where
                    add ctx =
                        ctx
                        & ePrevTextRemainder <>~ (es ^. traverse . pTextRemainder)
                        & mkEvents
                        & (foldr (addPreEventToEventMap (liftA2 mappend)) ?? es)
                        & append

addEvents ::
    (Applicative f, Monoid a, HasWidget w) =>
    (EventMap (f a) -> EventMap (f a) -> EventMap (f a)) ->
    EventMap (f a) -> w (f a) -> w (f a)
addEvents append = addEventsWithContext append . const

strongerEvents ::
    (Applicative f, Monoid a, HasWidget w) => EventMap (f a) -> w (f a) -> w (f a)
strongerEvents = addEvents mappend

weakerEvents ::
    (Applicative f, Monoid a, HasWidget w) => EventMap (f a) -> w (f a) -> w (f a)
weakerEvents = addEvents (flip mappend)

weakerEventsWithContext ::
    (Applicative f, Monoid a, HasWidget w) =>
    (EventContext -> EventMap (f a)) -> w (f a) -> w (f a)
weakerEventsWithContext = addEventsWithContext (flip mappend)

strongerEventsWithoutPreevents ::
    HasWidget w => EventMap a -> w a -> w a
strongerEventsWithoutPreevents eventMap =
    widget . eventMapMaker . Lens.mapped %~ (eventMap <>)

weakerEventsWithoutPreevents ::
    HasWidget w => EventMap a -> w a -> w a
weakerEventsWithoutPreevents eventMap =
    widget . eventMapMaker . Lens.mapped %~ (<> eventMap)

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

setFocusedWith ::
    Rect -> (EventContext -> EventMap a) -> Widget a -> Widget a
setFocusedWith rect eventMap =
    wState %~
    \s ->
    case s of
    StateUnfocused u ->
        const Focused
        { _fFocalAreas = [rect]
        , _fEventMap = eventMap
        , _fPreEvents = mempty
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
