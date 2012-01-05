{-# OPTIONS -Wall #-}
{-# LANGUAGE TypeOperators, OverloadedStrings #-}
module Graphics.UI.Bottle.Widgets.FocusDelegator(Cursor, make, makeWithKeys, makeWithLabel) where

import Data.Monoid (mappend)
import Data.Record.Label ((:->), setL, getL)
import Graphics.UI.Bottle.Widget(Widget(..))
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.AnimIds as AnimIds
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget

type Cursor = Bool

defaultStartDelegatingKey :: E.EventType
defaultStartDelegatingKey = E.KeyEventType E.noMods E.KeyEnter

defaultStopDelegatingKey :: E.EventType
defaultStopDelegatingKey = E.KeyEventType E.noMods E.KeyEsc

blue :: Draw.Color
blue = Draw.Color 0 0 1 1

makeWithKeys ::
  E.EventType -> E.EventType -> (Cursor -> k) -> Cursor -> Anim.AnimId ->
  (Bool -> Widget k) -> Bool -> Widget k
makeWithKeys
  startDelegatingKey stopDelegatingKey liftCursor delegating _animId widget hasFocus =
  handleFocus delegating hasFocus
  where
    handleFocus False True = blueify (widget False)
    handleFocus False False = widget False
    handleFocus True _ = addEscape (widget hasFocus)

    blueify =
      (Widget.atImageWithSize . Anim.backgroundColor AnimIds.backgroundCursorId 10) blue .
      Widget.atMaybeEventMap mkNonDelegatingEventMap

    mkNonDelegatingEventMap = (fmap . const) nonDelegatingEventMap
    nonDelegatingEventMap = eventMap startDelegatingKey True

    addEscape = Widget.atMaybeEventMap $ flip mappend (Just delegatingEventMap)
    delegatingEventMap = eventMap stopDelegatingKey False

    eventMap key = E.singleton key . const . liftCursor

make ::
  (Cursor -> k) -> Cursor -> Anim.AnimId ->
  (Bool -> Widget k) -> Bool -> Widget k
make = makeWithKeys defaultStartDelegatingKey defaultStopDelegatingKey

makeWithLabel ::
  (model :-> Cursor) -> model -> Anim.AnimId ->
  (Bool -> Widget model) -> Bool -> Widget model
makeWithLabel cursorL model = make (flip (setL cursorL) model) (getL cursorL model)
