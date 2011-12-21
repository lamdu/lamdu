{-# OPTIONS -Wall #-}
{-# LANGUAGE TypeOperators #-}
module Graphics.UI.Bottle.Widgets.FocusDelegator(Cursor, make, makeWithKeys, makeWithLabel) where

import Control.Newtype(unpack)
import Data.Monoid (mappend)
import Data.Record.Label ((:->), setL, getL)
import Graphics.DrawingCombinators.Utils (backgroundColor)
import Graphics.UI.Bottle.Widget(Widget(..))
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget

type Cursor = Bool

defaultStartDelegatingKey :: E.EventType
defaultStartDelegatingKey = E.KeyEventType E.noMods E.KeyEnter

defaultStopDelegatingKey :: E.EventType
defaultStopDelegatingKey = E.KeyEventType E.noMods E.KeyEsc

blue :: Draw.Color
blue = Draw.Color 0 0 1 1

makeWithKeys :: E.EventType -> E.EventType -> (Cursor -> k) -> Cursor -> Widget k -> Widget k
makeWithKeys
  startDelegatingKey stopDelegatingKey
  liftCursor delegating widget =
  Widget $ handleFocus delegating
  where
    handleFocus False True = unpack (blueify widget) False
    handleFocus False False = unpack widget False
    handleFocus True hasFocus = unpack (addEscape widget) hasFocus

    blueify =
      Widget.atImageWithSize (backgroundColor blue) .
      Widget.atMaybeEventMap mkNonDelegatingEventMap

    mkNonDelegatingEventMap = (fmap . const) nonDelegatingEventMap
    nonDelegatingEventMap = eventMap startDelegatingKey True

    addEscape = Widget.atMaybeEventMap $ flip mappend (Just delegatingEventMap)
    delegatingEventMap = eventMap stopDelegatingKey False

    eventMap key = E.singleton key . const . liftCursor

make :: (Cursor -> k) -> Cursor -> Widget k -> Widget k
make = makeWithKeys defaultStartDelegatingKey defaultStopDelegatingKey

makeWithLabel :: (model :-> Cursor) -> model -> Widget model -> Widget model
makeWithLabel cursorL model = make (flip (setL cursorL) model) (getL cursorL model)
