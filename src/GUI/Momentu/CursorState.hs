{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

-- | A mechanism to maintain transient state in Momentu's cursor
-- (useful for popups's whose state disappears with them)

module GUI.Momentu.CursorState
    ( cursorState
    ) where

import qualified Control.Lens as Lens
import           Data.Binary (Binary)
import           Data.Binary.Utils (decodeS, encodeS)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widget.Id as WidgetId

import           Lamdu.Prelude

cursorStatePrefix :: ByteString
cursorStatePrefix = "state"

cursorStateWidgetId :: Binary s => Widget.Id -> s -> Widget.Id
cursorStateWidgetId prefix state = prefix <> Widget.Id [cursorStatePrefix, encodeS state]

cursorState ::
    (MonadReader env m, Widget.HasCursor env, Binary s) =>
    Widget.Id ->
    s ->
    Lens.ASetter r0 r1 (s, Widget.EventResult) Widget.EventResult ->
    Lens.ASetter r1 r2 Widget.EventResult Widget.EventResult ->
    (s -> m r0) ->
    m r2
cursorState myId defaultState eventStates events make =
    do
        state <- Widget.subId ?? myId <&> (>>= decodeState) <&> fromMaybe defaultState
        let curId = cursorStateWidgetId myId state
        let processDest cursor =
                case WidgetId.subId myId cursor of
                Nothing -> cursor
                Just sub ->
                    case decodeState sub of
                    Nothing -> curId <> Widget.Id sub
                    Just _ -> cursor
        make state
            & Widget.assignCursorPrefix curId ((myId <>) . Widget.Id)
            <&> eventStates %~ updateState
            <&> events . Widget.eCursor . Lens.mapped %~ processDest
    & Widget.assignCursor myId (cursorStateWidgetId myId defaultState)
    where
        decodeState = decodeState' -- for monomorphism
        decodeState' subId
            | take 1 subId == [cursorStatePrefix] = decodeS (subId !! 1) & Just
            | otherwise = Nothing
        updateState (newState, eventResult) =
            eventResult & Widget.eCursor . Lens.mapped %~ cursorUpdateState newState
        cursorUpdateState newState cursor =
            case WidgetId.subId myId cursor of
            Nothing -> cursor
            Just sub -> cursorStateWidgetId myId newState <> Widget.Id sub
