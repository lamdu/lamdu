{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Graphics.UI.Bottle.Widgets.FocusDelegator(IsDelegating(..), Keys(..), make, defaultKeys) where

import Data.Maybe(fromMaybe)
import Data.Monoid(mappend)
import Graphics.UI.Bottle.Rect(Rect(..))
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
      E.fromEventType (startDelegatingKey keys) "Enter child" .
      Widget.enterResultEvent $ childEnter Widget.Outside

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
make isDelegating Nothing focusSelf =
  const . const $ Widget.atMkUserIO f
  where
    f mkUserIO size = Widget.atUioMaybeEnter (mEnter isDelegating size) $ mkUserIO size

    mEnter NotDelegating wholeSize _ = Just . const $ takeFocus wholeSize
    mEnter _ _ Nothing = Nothing
    mEnter Delegating wholeSize (Just enterChild) = Just $ handleDir enterChild wholeSize

    handleDir enterChild wholeSize dir =
      Widget.direction (takeFocus wholeSize) (const (enterChild dir)) dir

    takeFocus wholeSize = Widget.EnterResult (Rect 0 wholeSize) . return $ Widget.eventResultFromCursor focusSelf

make _ (Just cursor) focusSelf =
  makeFocused cursor focusSelf
