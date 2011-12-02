{-# OPTIONS -Wall #-}
module Graphics.UI.GLFWWidgets.FocusDelegator(Cursor, make) where

import Control.Newtype(unpack)
import Data.Monoid (mappend, mconcat)
import Data.Vector.Vector2 (Vector2(..))
import Graphics.DrawingCombinators((%%))
import Graphics.DrawingCombinators.Utils (square)
import Graphics.UI.GLFWWidgets.Widget(Widget(..))
import qualified Graphics.UI.GLFWWidgets.EventMap as EventMap
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.UI.GLFWWidgets.Widget as Widget

type Cursor = Bool

startDelegatingKey :: EventMap.EventType
startDelegatingKey = EventMap.KeyEventType EventMap.noMods GLFW.KeyEnter

stopDelegatingKey :: EventMap.EventType
stopDelegatingKey = EventMap.KeyEventType EventMap.noMods GLFW.KeyEsc

blue :: Draw.Color
blue = Draw.Color 0 0 1 1

make :: (Cursor -> k) -> Cursor -> Widget k -> Widget k
make liftCursor delegating widget = Widget $ handleFocus delegating
  where
    handleFocus False True = unpack (blueify widget) False
    handleFocus False False = unpack widget False
    handleFocus True hasFocus = unpack (addEscape widget) hasFocus

    blueify =
      Widget.atImageWithSize blueBackground .
      Widget.atMaybeEventMap mkNonDelegatingEventMap

    mkNonDelegatingEventMap = (fmap . const) nonDelegatingEventMap
    nonDelegatingEventMap = eventMap startDelegatingKey True

    addEscape = Widget.atMaybeEventMap $ flip mappend (Just delegatingEventMap)
    delegatingEventMap = eventMap stopDelegatingKey False

    eventMap key = EventMap.singleton key . const . liftCursor

    blueBackground (Vector2 width height) image =
      mconcat [
        image,
        Draw.tint blue $ Draw.scale width height %% square
      ]
