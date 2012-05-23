{-# LANGUAGE OverloadedStrings #-}
module Graphics.UI.Bottle.Widgets.FocusDelegator(IsDelegating(..), Keys(..), make, defaultKeys) where

import Data.Monoid (mappend, mempty)
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

makeFocused :: Monad f =>
  IsDelegating -> Widget.Id -> Keys -> Widget.Id ->
  Widget f -> Widget f
makeFocused delegating focusSelf keys backgroundCursorId =
  handleFocus delegating
  where
    handleFocus Delegating    = addStopDelegatingEventMap
    handleFocus NotDelegating = blueify . useStartDelegatingEventMap

    blueify = Widget.backgroundColor (mappend backgroundCursorId focusSelf) blue

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
      return $ Widget.eventResultFromCursor focusSelf

-- | Make a focus delegator
make
  :: Monad f
  => IsDelegating -- ^ Start state, enter from direction state
  -> Maybe IsDelegating -- ^ Current state
  -> Widget.Id -- ^ Enter/Stop delegating value
  -> Keys -- ^ Keys configuration
  -> Widget.Id -- ^ Background Cursor Id
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

    takeFocus wholeSize = Widget.EnterResult (Rect 0 wholeSize) . return $ Widget.eventResultFromCursor focusSelf

make _ (Just cursor) focusSelf =
  makeFocused cursor focusSelf
