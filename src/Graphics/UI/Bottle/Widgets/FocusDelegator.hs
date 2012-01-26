{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Graphics.UI.Bottle.Widgets.FocusDelegator(IsDelegating(..), make, defaultKeys) where

import Data.Maybe(fromMaybe)
import Data.Monoid(mappend)
import Data.Vector.Vector2(Vector2(..))
import Graphics.UI.Bottle.Widget(Widget(..))
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.Animation as Anim
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

makeFocused :: Monad f =>
  IsDelegating -> Anim.AnimId -> Keys -> Anim.AnimId ->
  Widget f -> Widget f
makeFocused delegating focusSelf keys backgroundCursorId =
  Widget.align (Vector2 0.5 0.5) .
  handleFocus delegating
  where
    handleFocus Delegating    = addStopDelegatingEventMap
    handleFocus NotDelegating = blueify . useStartDelegatingEventMap

    blueify = Widget.backgroundColor backgroundCursorId blue

    dir = Nothing

    useStartDelegatingEventMap = Widget.atUserIO setStartDelegatingEventMap
    setStartDelegatingEventMap userIO =
      ($ userIO) .
      -- Don't touch event map if child has no enter (is not focusable)
      fromMaybe id .
      -- If child has enter, set event map to the result of startDelegatingEventMap
      fmap (Widget.atUioEventMap . const . startDelegatingEventMap) $
      Widget.uioMaybeEnter userIO

    startDelegatingEventMap childEnter =
      E.fromEventType (startDelegatingKey keys) "Enter child" $ childEnter dir

    addStopDelegatingEventMap =
      Widget.atEventMap .
      flip mappend .
      E.fromEventType (stopDelegatingKey keys) "Exit child" .
      return $ Widget.eventResultFromCursor focusSelf

make :: Monad f => -- actually "Pointed", as only using return.
  IsDelegating -> -- ^ Enter/start state
  Maybe IsDelegating -> -- ^ Current state
  Anim.AnimId -> -- ^ Enter/Stop delegating value
  Keys -> -- ^ Keys configuration
  Anim.AnimId -> -- ^ Background AnimId
  Widget f -> Widget f
make NotDelegating Nothing focusSelf _ _ =
  Widget.atMaybeEnter . const . Just . const . return $ Widget.eventResultFromCursor focusSelf
make Delegating Nothing  _     _ _ = id
make _ (Just cursor) focusSelf keys backgroundCursorId = makeFocused cursor focusSelf keys backgroundCursorId
