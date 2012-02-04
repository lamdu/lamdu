{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Graphics.UI.Bottle.Widgets.FocusDelegator(IsDelegating(..), Keys(..), make, defaultKeys) where

import Data.Maybe(fromMaybe)
import Data.Monoid(mappend)
import Graphics.UI.Bottle.Widget(Widget(..))
import qualified Graphics.DrawingCombinators as Draw
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
  IsDelegating -> Widget.Id -> Keys -> Widget.Id ->
  Widget f -> Widget f
makeFocused delegating focusSelf keys backgroundCursorId =
  handleFocus delegating
  where
    handleFocus Delegating    = addStopDelegatingEventMap
    handleFocus NotDelegating = blueify . useStartDelegatingEventMap

    blueify = Widget.backgroundColor backgroundCursorId blue

    useStartDelegatingEventMap = Widget.atUserIO setStartDelegatingEventMap
    setStartDelegatingEventMap userIO =
      ($ userIO) .
      -- Don't touch event map if child has no enter (is not focusable)
      fromMaybe id .
      -- If child has enter, set event map to the result of startDelegatingEventMap
      fmap (Widget.atUioEventMap . const . startDelegatingEventMap) $
      Widget.uioMaybeEnter userIO

    startDelegatingEventMap childEnter =
      E.fromEventType (startDelegatingKey keys) "Enter child" $ childEnter Widget.Outside

    addStopDelegatingEventMap =
      Widget.atEventMap .
      flip mappend .
      E.fromEventType (stopDelegatingKey keys) "Exit child" .
      return $ Widget.eventResultFromCursor focusSelf

make :: Monad f => -- actually "Pointed", as only using return.
  IsDelegating -> -- ^ Start state, enter from direction state
  Maybe IsDelegating -> -- ^ Current state
  Widget.Id -> -- ^ Enter/Stop delegating value
  Keys -> -- ^ Keys configuration
  Widget.Id -> -- ^ Background Cursor Id
  Widget f -> Widget f
make isDelegating Nothing focusSelf _ _ =
  Widget.atMaybeEnter $ mEnter isDelegating
  where
    takeFocus = return $ Widget.eventResultFromCursor focusSelf
    mEnter NotDelegating _ = Just (const takeFocus)
    mEnter _ Nothing = Nothing
    mEnter Delegating (Just enterChild) =
      Just . Widget.direction takeFocus $ enterChild . Widget.Dir
make _ (Just cursor) focusSelf keys backgroundCursorId = makeFocused cursor focusSelf keys backgroundCursorId
