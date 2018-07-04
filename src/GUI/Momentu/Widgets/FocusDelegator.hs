module GUI.Momentu.Widgets.FocusDelegator
    ( FocusEntryTarget(..)
    , Config(..)
    , make
    ) where

import qualified Control.Lens as Lens
import           GUI.Momentu.Direction (Direction)
import qualified GUI.Momentu.Direction as Direction
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.MetaKey (MetaKey, toModKey)
import           GUI.Momentu.Rect (Rect(..))
import           GUI.Momentu.State (Gui)
import qualified GUI.Momentu.State as State
import qualified GUI.Momentu.Widget as Widget

import           Lamdu.Prelude

data FocusEntryTarget = FocusEntryChild | FocusEntryParent

data Config = Config
    { focusChildKeys :: [MetaKey]
    , focusChildDoc :: E.Doc
    , focusParentKeys :: [MetaKey]
    , focusParentDoc :: E.Doc
    }

focusChildEventMap :: Config -> Maybe (Direction -> Widget.EnterResult a) -> EventMap a
focusChildEventMap config mEnter =
    -- We're not delegating, so replace the child eventmap with an
    -- event map to either delegate to it (if it is enterable) or to
    -- nothing (if it is not):
    case mEnter of
    Nothing -> mempty
    Just childEnter ->
        E.keyPresses (focusChildKeys config <&> toModKey)
        (focusChildDoc config) $
        childEnter Direction.Outside ^. Widget.enterResultEvent

modifyEntry ::
    Applicative f =>
    Widget.Id -> Rect -> FocusEntryTarget ->
    Maybe (Direction -> Gui Widget.EnterResult f) ->
    Maybe (Direction -> Gui Widget.EnterResult f)
modifyEntry myId fullChildRect target mChildEnter =
    case target of
    FocusEntryParent -> parentEnter
    FocusEntryChild -> maybe parentEnter wrapEnter mChildEnter
    & Just
    where
        wrapEnter _          Direction.Outside = parentEnterResult
        wrapEnter enterChild dir               = enterChild dir

        parentEnter = Widget.enterFuncAddVirtualCursor fullChildRect (const parentEnterResult)
        parentEnterResult =
            Widget.EnterResult
            { Widget._enterResultRect = fullChildRect
            , Widget._enterResultLayer = 0
            , Widget._enterResultEvent = State.updateCursor myId & pure
            }

make ::
    (MonadReader env m, State.HasCursor env, Applicative f, Widget.HasWidget w) =>
    m (Config -> FocusEntryTarget -> Widget.Id -> Gui w f -> Gui w f)
make =
    Lens.view State.cursor <&>
    \cursor config focusEntryTarget myId -> Widget.widget %~ \childWidget ->
    case () of
    ()
        | selfIsFocused && childIsFocused ->
            error "FocusDelegator both itself and child focused"

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
