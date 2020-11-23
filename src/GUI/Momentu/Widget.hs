module GUI.Momentu.Widget
    ( module Types

    , Id(..), Id.joinId

    -- Types:
    , R, Size

    -- Widget lenses:
    , enterResultCursor, sizedState

    , HasWidget(..)

    , updates
    , isFocused
    , wFocused

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
    , addPreEvent
    , eventMapMaker

    -- Operations:
    , translate
    , translateFocused, combineEnterPoints

    , makeFocusableView
    , makeFocusableWidget

    , respondToCursorPrefix
    , respondToCursorBy
    , setFocused, setFocusedWith

    , strollAheadKeys, strollBackKeys
    , takesStroll, disableStroll
    , glueStates
    ) where

import           Control.Applicative (liftA2)
import qualified Control.Lens as Lens
import qualified Data.Text as Text
import           Data.Vector.Vector2 (Vector2(..))
import           GUI.Momentu.Animation (R, Size)
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.FocusDirection (FocusDirection(..))
import           GUI.Momentu.Rect (Rect(..))
import qualified GUI.Momentu.Rect as Rect
import           GUI.Momentu.State (VirtualCursor(..), HasCursor(..))
import qualified GUI.Momentu.State as State
import           GUI.Momentu.View (View(..))
import           GUI.Momentu.Widget.Id (Id(..))
import qualified GUI.Momentu.Widget.Id as Id
import           GUI.Momentu.Widget.Instances
import           GUI.Momentu.Widget.Types as Types

import           GUI.Momentu.Prelude

class HasWidget w where widget :: Lens.Setter (w a) (w b) (Widget a) (Widget b)
instance HasWidget Widget where widget = id

updates :: HasWidget w => Lens.Setter (w f) (w g) (f State.Update) (g State.Update)
updates = widget . wState . Lens.mapped

isFocused :: Widget a -> Bool
isFocused = Lens.has (wState . _StateFocused)

enterResultCursor :: (HasWidget w, Functor f) => Lens.Setter' (w f) Id
enterResultCursor =
    widget . enterResult . enterResultEvent . Lens.mapped . State.uCursor . Lens.mapped

takesStroll :: HasWidget w => Id -> w a -> w a
takesStroll myId =
    widget . wState . _StateUnfocused . uMStroll ?~
    (myId ^. Lens._Unwrapped, myId ^. Lens._Unwrapped)

disableStroll :: HasWidget w => w a -> w a
disableStroll = widget . wState . _StateUnfocused . uMStroll .~ Nothing

takesFocus ::
    (HasWidget w, Functor f) =>
    (FocusDirection -> f Id) -> w f -> w f
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
            & wFocused . fMEnterPoint %~
                Just . fromMaybe (enter . Point)
            & wState . _StateUnfocused . uMEnter ?~ enter

enterFuncAddVirtualCursor ::
    Functor f =>
    Rect ->
    (FocusDirection -> EnterResult (f State.Update)) ->
    FocusDirection -> EnterResult (f State.Update)
enterFuncAddVirtualCursor destRect =
    Lens.imapped <. enterResultEvent . Lens.mapped . State.uVirtualCursor . Lens._Wrapped .@~ mkVirtCursor
    where
        mkVirtCursor dir =
            case dir of
            FromRight r -> destRect & Rect.verticalRange   .~ r & Just
            FromLeft  r -> destRect & Rect.verticalRange   .~ r & Just
            FromAbove r -> destRect & Rect.horizontalRange .~ r & Just
            FromBelow r -> destRect & Rect.horizontalRange .~ r & Just
            FromOutside -> Nothing
            Point p     -> Rect p 0 & Just
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
        actionText = E.emHandlerDocHandlers . E.dhDoc . E.docStrs . Lens.reversed . Lens.element 0
        concatDescs x y = filter (not . Text.null) [x, y] & Text.intercalate ", "

-- | New pre-event is not added to the pre-events if pre-event is
-- BlockEvents (but still added to the event map)
addPreEvent ::
    Applicative f =>
    PreEvent (f State.Update) -> Widget f -> Widget f
addPreEvent preEvent =
    wFocused %~ onFocused
    where
        onFocused f =
            f
            & fPreEvents . _PreEvents %~ (preEvent :)
            & fEventMap %~ onMkEventMap
        onMkEventMap mk ctx =
            ctx
            & ePrevTextRemainder %~ (preEvent ^. pTextRemainder <>)
            & mk
            & addPreEventToEventMap (liftA2 mappend) preEvent

addEventsWithContext ::
    (Applicative f, HasWidget w) =>
    (EventMap (f State.Update) -> EventMap (f State.Update) -> EventMap (f State.Update)) ->
    (EventContext -> EventMap (f State.Update)) -> w f -> w f
addEventsWithContext append mkEvents =
    widget . wFocused %~ onFocused
    where
        onFocused f =
            case f ^. fPreEvents of
            BlockEvents -> f
            PreEvents es ->
                f & fEventMap . Lens.imapped %@~ add
                where
                    add ctx =
                        ctx
                        & ePrevTextRemainder <>~ es ^. traverse . pTextRemainder
                        & mkEvents
                        & (foldr (addPreEventToEventMap (liftA2 mappend)) ?? es)
                        & append

addEvents ::
    (Applicative f, HasWidget w) =>
    (EventMap (f State.Update) -> EventMap (f State.Update) -> EventMap (f State.Update)) ->
    EventMap (f State.Update) -> w f -> w f
addEvents append = addEventsWithContext append . const

strongerEvents ::
    (Applicative f, HasWidget w) => EventMap (f State.Update) -> w f -> w f
strongerEvents = addEvents mappend

weakerEvents ::
    (Applicative f, HasWidget w) => EventMap (f State.Update) -> w f -> w f
weakerEvents = addEvents (flip mappend)

weakerEventsWithContext ::
    (Applicative f, HasWidget w) =>
    (EventContext -> EventMap (f State.Update)) -> w f -> w f
weakerEventsWithContext = addEventsWithContext (flip mappend)

strongerEventsWithoutPreevents ::
    HasWidget w => EventMap (f State.Update) -> w f -> w f
strongerEventsWithoutPreevents eventMap =
    widget . eventMapMaker . Lens.mapped %~ (eventMap <>)

weakerEventsWithoutPreevents ::
    HasWidget w => EventMap (f State.Update) -> w f -> w f
weakerEventsWithoutPreevents eventMap =
    widget . eventMapMaker . Lens.mapped %~ (<> eventMap)

translateFocused ::
    Functor f =>
    Vector2 R -> (Surrounding -> Focused (f State.Update)) ->
    Surrounding -> Focused (f State.Update)
translateFocused pos = translateFocusedGeneric (fmap (translateUpdate pos)) pos

setFocused :: HasWidget w => w a -> w a
setFocused = widget %~ \w -> setFocusedWith (Rect 0 (w ^. wSize)) mempty w

setFocusedWith ::
    Rect -> (EventContext -> EventMap (f State.Update)) -> Widget f -> Widget f
setFocusedWith rect eventMap =
    wState %~
    \s ->
    case s of
    StateUnfocused u ->
        const Focused
        { _fFocalAreas = [rect]
        , _fEventMap = eventMap
        , _fPreEvents = mempty
        , _fMEnterPoint = u ^. uMEnter <&> (. Point)
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

makeFocusableView ::
    (MonadReader env m, HasCursor env, Applicative f) =>
    m (Id -> View -> Widget f)
makeFocusableView = makeFocusableWidget <&> Lens.mapped . Lens.argument %~ fromView

-- TODO: Describe why makeFocusableView is to be usually preferred
makeFocusableWidget ::
    (MonadReader env m, HasCursor env, Applicative f) =>
    m (Id -> Widget f -> Widget f)
makeFocusableWidget =
    respondToCursorPrefix
    <&> \respond myIdPrefix w ->
    w
    & respond myIdPrefix
    & takesFocus (const (pure myIdPrefix))
