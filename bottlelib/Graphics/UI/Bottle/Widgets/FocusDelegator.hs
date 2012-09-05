{-# LANGUAGE OverloadedStrings #-}
module Graphics.UI.Bottle.Widgets.FocusDelegator
  ( IsDelegating(..)
  , Config(..), Style(..), Env(..)
  , make
  , wrapEnv
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

data Style = Style
  { color :: Draw.Color
  , layer :: Int
  , cursorBGAnimId :: AnimId
  }

data Config = Config
  { startDelegatingKey :: E.ModKey
  , startDelegatingDoc :: E.Doc
  , stopDelegatingKey :: E.ModKey
  , stopDelegatingDoc :: E.Doc
  }

data Env = Env
  { config :: Config
  , style :: Style
  }

makeFocused
  :: Applicative f
  => IsDelegating -> Widget.Id -> Env
  -> Widget f -> Widget f
makeFocused delegating focusSelf env =
  handleFocus delegating
  where
    handleFocus Delegating    = addStopDelegatingEventMap
    handleFocus NotDelegating = colorize . useStartDelegatingEventMap

    ourConfig = config env
    ourStyle = style env
    colorize = Widget.backgroundColor (layer ourStyle) (mappend (cursorBGAnimId ourStyle) (Widget.toAnimId focusSelf)) $ color ourStyle

    useStartDelegatingEventMap w =
      ($ w) .
      -- We're not delegating, so replace the child eventmap with an
      -- event map to either delegate to it (if it is enterable) or to
      -- nothing (if it is not):
      Widget.atWEventMap . const .
      maybe mempty startDelegatingEventMap $
      Widget.wMaybeEnter w

    startDelegatingEventMap childEnter =
      E.keyPress (startDelegatingKey ourConfig) (startDelegatingDoc ourConfig) .
      Widget.enterResultEvent $ childEnter Direction.Outside

    addStopDelegatingEventMap =
      Widget.weakerEvents .
      E.keyPress (stopDelegatingKey ourConfig) (stopDelegatingDoc ourConfig) .
      pure $ Widget.eventResultFromCursor focusSelf

-- | Make a focus delegator
make
  :: Applicative f
  => IsDelegating -- ^ Start state, enter from direction state
  -> Maybe IsDelegating -- ^ Current state
  -> Widget.Id -- ^ Enter/Stop delegating value
  -> Env -- ^ FocusDelegator configuration
  -> Widget f -> Widget f
make isDelegating Nothing focusSelf _ w =
  Widget.atWMaybeEnter (mEnter isDelegating (Widget.wSize w)) w
  where
    mEnter NotDelegating wholeSize _ = Just . const $ takeFocus wholeSize
    mEnter _ _ Nothing = Nothing
    mEnter Delegating wholeSize (Just enterChild) = Just $ handleDir enterChild wholeSize

    handleDir enterChild wholeSize dir =
      Direction.fold (takeFocus wholeSize) (const (enterChild dir)) dir

    takeFocus wholeSize = Widget.EnterResult (Rect 0 wholeSize) . pure $ Widget.eventResultFromCursor focusSelf

make _ (Just cursor) focusSelf env w =
  makeFocused cursor focusSelf env w

delegatingId :: Widget.Id -> Widget.Id
delegatingId = flip Widget.joinId ["delegating"]

notDelegatingId :: Widget.Id -> Widget.Id
notDelegatingId = flip Widget.joinId ["not delegating"]

wrapEnv
  :: Applicative f
  => Env
  -> IsDelegating
  -> ((Widget f -> Widget f) -> Widget.Id -> Widget.Id -> a)
  -> Widget.Id
  -> Widget.Id -> a
wrapEnv env entryState mkResult myId cursor =
  mkResult atWidget innerId newCursor
  where
    atWidget innerWidget =
      (Widget.atWIsFocused . const) (isJust mIsDelegating) $
      make entryState mIsDelegating delegatorId env innerWidget
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
