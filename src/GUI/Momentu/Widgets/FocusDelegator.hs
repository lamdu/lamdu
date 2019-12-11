module GUI.Momentu.Widgets.FocusDelegator
    ( FocusEntryTarget(..)
    , Config(..)
    , make
    ) where

import qualified Control.Lens as Lens
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.FocusDirection (FocusDirection)
import qualified GUI.Momentu.FocusDirection as Direction
import           GUI.Momentu.MetaKey (MetaKey, toModKey)
import           GUI.Momentu.Rect (Rect(..))
import qualified GUI.Momentu.State as State
import qualified GUI.Momentu.Widget as Widget

import           GUI.Momentu.Prelude

data FocusEntryTarget = FocusEntryChild | FocusEntryParent

data Config = Config
    { focusChildKeys :: [MetaKey]
    , focusChildDoc :: E.Doc
    , focusParentKeys :: [MetaKey]
    , focusParentDoc :: E.Doc
    }

focusChildEventMap :: Config -> Maybe (FocusDirection -> Widget.EnterResult a) -> EventMap a
focusChildEventMap config =
    -- We're not delegating, so replace the child eventmap with an
    -- event map to either delegate to it (if it is enterable) or to
    -- nothing (if it is not):
    foldMap f
    where
        f childEnter =
            childEnter Direction.FromOutside ^. Widget.enterResultEvent
            & E.keyPresses (focusChildKeys config <&> toModKey)
                (focusChildDoc config)

modifyEntry ::
    Applicative f =>
    Widget.Id -> Rect -> FocusEntryTarget ->
    Maybe (FocusDirection -> Widget.EnterResult (f State.Update)) ->
    Maybe (FocusDirection -> Widget.EnterResult (f State.Update))
modifyEntry myId fullChildRect target mChildEnter =
    case target of
    FocusEntryParent -> parentEnter
    FocusEntryChild -> maybe parentEnter wrapEnter mChildEnter
    & Just
    where
        wrapEnter _          Direction.FromOutside = parentEnterResult
        wrapEnter enterChild dir                   = enterChild dir

        parentEnter = Widget.enterFuncAddVirtualCursor fullChildRect (const parentEnterResult)
        parentEnterResult =
            Widget.EnterResult
            { Widget._enterResultRect = fullChildRect
            , Widget._enterResultLayer = 0
            , Widget._enterResultEvent = State.updateCursor myId & pure
            }

make ::
    (HasCallStack, MonadReader env m, State.HasCursor env, Applicative f, Widget.HasWidget w) =>
    m (Config -> FocusEntryTarget -> Widget.Id -> w f -> w f)
make =
    Lens.view State.cursor <&>
    \cursor config focusEntryTarget myId -> Widget.widget %~ \childWidget ->
    case () of
    ()
        | selfIsFocused && childIsFocused ->
            unlines
            [ "FocusDelegator both itself and child focused. parentDoc:"
            , show (focusParentDoc config)
            , "childDoc:"
            , show (focusChildDoc config)
            ] & error

        | selfIsFocused ->
            Widget.setFocused childWidget
            & Widget.wState . Widget._StateFocused . Lens.mapped . Widget.fEventMap . Lens.mapped
            .~ focusChildEventMap config
                (childWidget ^? Widget.wState . Widget._StateUnfocused . Widget.uMEnter . Lens._Just)

        | childIsFocused ->
            childWidget & Widget.eventMapMaker . Lens.mapped %~ (<> focusParentEventMap)

        | otherwise ->
            childWidget
            & Widget.wState . Widget._StateUnfocused . Widget.uMEnter %~
                modifyEntry myId fullChildRect focusEntryTarget
        where
            fullChildRect = Rect 0 (childWidget ^. Element.size)
            childIsFocused = Widget.isFocused childWidget
            selfIsFocused = myId == cursor
            focusParentEventMap =
                E.keysEventMapMovesCursor
                (focusParentKeys config)
                (focusParentDoc config)
                (pure myId)
