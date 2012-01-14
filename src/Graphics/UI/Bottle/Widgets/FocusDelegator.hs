{-# OPTIONS -Wall #-}
{-# LANGUAGE TypeOperators, OverloadedStrings #-}
module Graphics.UI.Bottle.Widgets.FocusDelegator(Cursor(..), make, makeWithKeys) where

import Data.Monoid (mappend)
import Graphics.UI.Bottle.Widget(Widget(..))
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget

data Cursor = Delegating | NotDelegating

defaultStartDelegatingKey :: E.EventType
defaultStartDelegatingKey = E.KeyEventType E.noMods E.KeyEnter

defaultStopDelegatingKey :: E.EventType
defaultStopDelegatingKey = E.KeyEventType E.noMods E.KeyEsc

blue :: Draw.Color
blue = Draw.Color 0 0 1 1

makeWithKeys ::
  E.EventType -> E.EventType ->
  (Cursor -> k) -> Cursor -> Anim.AnimId ->
  Widget k -> Bool -> Widget k
makeWithKeys
  startDelegatingKey stopDelegatingKey liftCursor delegating backgroundCursorId widget hasFocus =
  handleFocus delegating hasFocus widget
  where
    handleFocus Delegating    _     = addStopDelegatingEventMap
    handleFocus NotDelegating True  = blueify . useStartDelegatingEventMap
    handleFocus NotDelegating False = id

    blueify =
      Widget.atImageWithSize . Anim.backgroundColor backgroundCursorId 10 $ blue

    useStartDelegatingEventMap =
      Widget.atEventMap . const $ eventMap startDelegatingKey Delegating

    addStopDelegatingEventMap =
      Widget.atEventMap . flip mappend $ eventMap stopDelegatingKey NotDelegating

    eventMap key = E.fromEventType key . liftCursor

make :: (Cursor -> k) -> Cursor -> Anim.AnimId -> Widget k -> Bool -> Widget k
make = makeWithKeys defaultStartDelegatingKey defaultStopDelegatingKey
