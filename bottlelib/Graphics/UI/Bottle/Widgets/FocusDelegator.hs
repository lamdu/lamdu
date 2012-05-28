{-# LANGUAGE OverloadedStrings #-}
module Graphics.UI.Bottle.Widgets.FocusDelegator
  ( IsDelegating(..)
  , Keys(..)
  , make
  , defaultKeys
  , wrapKeys
  , delegatingId, notDelegatingId
  )
where

import Control.Applicative (Applicative(..))
import Data.ByteString.Char8() -- IsString instance
import Data.Maybe (isJust)
import Data.Monoid (mappend, mempty)
import Graphics.UI.Bottle.Animation (AnimId)
import Graphics.UI.Bottle.Rect(Rect(..))
import Graphics.UI.Bottle.Widget(Widget(..))
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.Direction as Direction
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget

data IsDelegating = Delegating | NotDelegating

data Keys = Keys {
  startDelegatingKey :: E.EventType,
  stopDelegatingKey :: E.EventType
  }

defaultKeys :: Keys
defaultKeys = Keys {
  startDelegatingKey = E.KeyEventType E.noMods E.KeyEnter,
  stopDelegatingKey = E.KeyEventType E.noMods E.KeyEsc
  }

blue :: Draw.Color
blue = Draw.Color 0 0 1 1

makeFocused :: Applicative f =>
  IsDelegating -> Widget.Id -> Keys -> AnimId ->
  Widget f -> Widget f
makeFocused delegating focusSelf keys backgroundCursorId =
  handleFocus delegating
  where
    handleFocus Delegating    = addStopDelegatingEventMap
    handleFocus NotDelegating = blueify . useStartDelegatingEventMap

    blueify = Widget.backgroundColor (mappend backgroundCursorId (Widget.toAnimId focusSelf)) blue

    useStartDelegatingEventMap = Widget.atSizeDependentWidgetData setStartDelegatingEventMap
    setStartDelegatingEventMap userIO =
      ($ userIO) .
      -- We're not delegating, so replace the child eventmap with an
      -- event map to either delegate to it (if it is enterable) or to
      -- nothing (if it is not):
      Widget.atSdwdEventMap . const .
      maybe mempty startDelegatingEventMap $
      Widget.sdwdMaybeEnter userIO

    startDelegatingEventMap childEnter =
      E.fromEventType (startDelegatingKey keys) "Enter child" .
      Widget.enterResultEvent $ childEnter Direction.Outside

    addStopDelegatingEventMap =
      Widget.weakerEvents .
      E.fromEventType (stopDelegatingKey keys) "Exit child" .
      pure $ Widget.eventResultFromCursor focusSelf

-- | Make a focus delegator
make
  :: Applicative f
  => IsDelegating -- ^ Start state, enter from direction state
  -> Maybe IsDelegating -- ^ Current state
  -> Widget.Id -- ^ Enter/Stop delegating value
  -> Keys -- ^ Keys configuration
  -> AnimId -- ^ Background Cursor Id
  -> Widget f -> Widget f
make isDelegating Nothing focusSelf =
  const . const $ Widget.atMkSizeDependentWidgetData f
  where
    f mkSizeDependentWidgetData size = Widget.atSdwdMaybeEnter (mEnter isDelegating size) $ mkSizeDependentWidgetData size

    mEnter NotDelegating wholeSize _ = Just . const $ takeFocus wholeSize
    mEnter _ _ Nothing = Nothing
    mEnter Delegating wholeSize (Just enterChild) = Just $ handleDir enterChild wholeSize

    handleDir enterChild wholeSize dir =
      Direction.fold (takeFocus wholeSize) (const (enterChild dir)) dir

    takeFocus wholeSize = Widget.EnterResult (Rect 0 wholeSize) . pure $ Widget.eventResultFromCursor focusSelf

make _ (Just cursor) focusSelf =
  makeFocused cursor focusSelf

delegatingId :: Widget.Id -> Widget.Id
delegatingId = flip Widget.joinId ["delegating"]

notDelegatingId :: Widget.Id -> Widget.Id
notDelegatingId = flip Widget.joinId ["not delegating"]

wrapKeys
  :: Applicative f
  => Keys
  -> IsDelegating
  -> ((Widget f -> Widget f) -> Widget.Id -> Widget.Id -> a)
  -> AnimId
  -> Widget.Id
  -> Widget.Id -> a
wrapKeys keys entryState mkResult backgroundCursorId myId cursor =
  mkResult atWidget innerId newCursor
  where
    atWidget innerWidget =
      (Widget.atIsFocused . const) (isJust mIsDelegating) $
      make entryState mIsDelegating delegatorId keys backgroundCursorId innerWidget
      where
        mIsDelegating =
          case Widget.subId delegatorId newCursor of
            Nothing
              | Widget.isFocused innerWidget -> Just Delegating
              | otherwise -> Nothing
            Just _ -> Just NotDelegating
    newCursor
      | cursor == myId = destId
      | otherwise = cursor
    destId =
      case entryState of
        NotDelegating -> delegatorId
        Delegating -> innerId
    innerId = delegatingId myId
    delegatorId = notDelegatingId myId
