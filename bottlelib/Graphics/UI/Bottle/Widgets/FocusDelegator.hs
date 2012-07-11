{-# LANGUAGE OverloadedStrings #-}
module Graphics.UI.Bottle.Widgets.FocusDelegator
  ( IsDelegating(..)
  , Config(..)
  , make
  , wrapConfig
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

data Config = Config {
  startDelegatingKey :: E.ModKey,
  startDelegatingDoc :: E.Doc,
  stopDelegatingKey :: E.ModKey,
  stopDelegatingDoc :: E.Doc
  }

blue :: Draw.Color
blue = Draw.Color 0 0 1 1

makeFocused
  :: Applicative f
  => IsDelegating -> Widget.Id -> Config -> AnimId
  -> Widget f -> Widget f
makeFocused delegating focusSelf config backgroundCursorId =
  handleFocus delegating
  where
    handleFocus Delegating    = addStopDelegatingEventMap
    handleFocus NotDelegating = blueify . useStartDelegatingEventMap

    blueify = Widget.backgroundColor 10 (mappend backgroundCursorId (Widget.toAnimId focusSelf)) blue

    useStartDelegatingEventMap w =
      ($ w) .
      -- We're not delegating, so replace the child eventmap with an
      -- event map to either delegate to it (if it is enterable) or to
      -- nothing (if it is not):
      Widget.atWEventMap . const .
      maybe mempty startDelegatingEventMap $
      Widget.wMaybeEnter w

    startDelegatingEventMap childEnter =
      E.keyPress (startDelegatingKey config) (startDelegatingDoc config) .
      Widget.enterResultEvent $ childEnter Direction.Outside

    addStopDelegatingEventMap =
      Widget.weakerEvents .
      E.keyPress (stopDelegatingKey config) (stopDelegatingDoc config) .
      pure $ Widget.eventResultFromCursor focusSelf

-- | Make a focus delegator
make
  :: Applicative f
  => IsDelegating -- ^ Start state, enter from direction state
  -> Maybe IsDelegating -- ^ Current state
  -> Widget.Id -- ^ Enter/Stop delegating value
  -> Config -- ^ FocusDelegator configuration
  -> AnimId -- ^ Background Cursor Id
  -> Widget f -> Widget f
make isDelegating Nothing focusSelf _ _ w =
  Widget.atWMaybeEnter (mEnter isDelegating (Widget.wSize w)) w
  where
    mEnter NotDelegating wholeSize _ = Just . const $ takeFocus wholeSize
    mEnter _ _ Nothing = Nothing
    mEnter Delegating wholeSize (Just enterChild) = Just $ handleDir enterChild wholeSize

    handleDir enterChild wholeSize dir =
      Direction.fold (takeFocus wholeSize) (const (enterChild dir)) dir

    takeFocus wholeSize = Widget.EnterResult (Rect 0 wholeSize) . pure $ Widget.eventResultFromCursor focusSelf

make _ (Just cursor) focusSelf config backgroundCursorId w =
  makeFocused cursor focusSelf config backgroundCursorId w

delegatingId :: Widget.Id -> Widget.Id
delegatingId = flip Widget.joinId ["delegating"]

notDelegatingId :: Widget.Id -> Widget.Id
notDelegatingId = flip Widget.joinId ["not delegating"]

wrapConfig
  :: Applicative f
  => Config
  -> IsDelegating
  -> ((Widget f -> Widget f) -> Widget.Id -> Widget.Id -> a)
  -> AnimId
  -> Widget.Id
  -> Widget.Id -> a
wrapConfig config entryState mkResult backgroundCursorId myId cursor =
  mkResult atWidget innerId newCursor
  where
    atWidget innerWidget =
      (Widget.atWIsFocused . const) (isJust mIsDelegating) $
      make entryState mIsDelegating delegatorId
      config backgroundCursorId innerWidget
      where
        mIsDelegating =
          case Widget.subId delegatorId newCursor of
            Nothing
              | Widget.wIsFocused innerWidget -> Just Delegating
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
