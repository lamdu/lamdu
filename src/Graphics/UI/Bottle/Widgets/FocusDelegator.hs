{-# OPTIONS -Wall #-}
{-# LANGUAGE TypeOperators, OverloadedStrings #-}
module Graphics.UI.Bottle.Widgets.FocusDelegator(Cursor(..), make, defaultKeys) where

import Data.Maybe(fromMaybe)
import Data.Monoid(mappend)
import Graphics.UI.Bottle.Widget(Widget(..))
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget

data Cursor = Delegating | NotDelegating

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

makeFocused ::
  Cursor -> k -> Keys -> Anim.AnimId ->
  Widget k -> Widget k
makeFocused delegating focusSelf keys backgroundCursorId =
  handleFocus delegating
  where
    handleFocus Delegating    = addStopDelegatingEventMap
    handleFocus NotDelegating = blueify . useStartDelegatingEventMap

    blueify =
      Widget.atImageWithSize . Anim.backgroundColor backgroundCursorId 10 $ blue

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
      E.fromEventType (startDelegatingKey keys) $ childEnter dir

    addStopDelegatingEventMap =
      Widget.atEventMap . flip mappend $ E.fromEventType (stopDelegatingKey keys) focusSelf

make ::
  Cursor -> -- ^ Enter/start state
  Maybe Cursor -> -- ^ Current state
  k -> -- ^ Enter/Stop delegating value
  Keys -> -- ^ Keys configuration
  Anim.AnimId -> -- ^ Background AnimId
  Widget k -> Widget k
make NotDelegating Nothing focusSelf _ _ = Widget.atMaybeEnter . const $ Just (const focusSelf)
make Delegating Nothing  _     _ _ = id
make _ (Just cursor) focusSelf keys backgroundCursorId = makeFocused cursor focusSelf keys backgroundCursorId
