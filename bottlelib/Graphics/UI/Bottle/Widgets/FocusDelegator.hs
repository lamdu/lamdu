{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Graphics.UI.Bottle.Widgets.FocusDelegator
  ( IsDelegating(..)
  , Config(..), Style(..), Env(..)
  , make
  , wrapEnv
  , delegatingId, notDelegatingId
  ) where

import           Control.Applicative (Applicative(..))
import           Control.Lens.Operators
import           Data.Maybe (isJust)
import           Data.Monoid ((<>), mempty)
import qualified Graphics.DrawingCombinators as Draw
import           Graphics.UI.Bottle.Animation (AnimId)
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.Direction as Direction
import qualified Graphics.UI.Bottle.EventMap as E
import           Graphics.UI.Bottle.ModKey (ModKey(..))
import           Graphics.UI.Bottle.Rect (Rect(..))
import           Graphics.UI.Bottle.Widget (Widget(..))
import qualified Graphics.UI.Bottle.Widget as Widget

data IsDelegating = Delegating | NotDelegating

data Style = Style
  { color :: Draw.Color
  , layer :: Anim.Layer
  , cursorBGAnimId :: AnimId
  }

data Config = Config
  { focusChildKeys :: [ModKey]
  , focusChildDoc :: E.Doc
  , focusParentKeys :: [ModKey]
  , focusParentDoc :: E.Doc
  }

data Env = Env
  { config :: Config
  , style :: Style
  }

addBackground :: Style -> AnimId -> Widget f -> Widget f
addBackground Style{..} animId =
  Widget.backgroundColor layer (cursorBGAnimId <> animId) color

makeFocused ::
  Applicative f =>
  IsDelegating -> Widget.Id -> Env ->
  Widget f -> Widget f
makeFocused delegating myId Env{..} widget =
  widget
  & case delegating of
    Delegating -> Widget.weakerEvents stopDelegatingEventMap
    NotDelegating ->
      addBackground style (Widget.toAnimId myId) .
      setStartDelegatingEventMap config
  where
    Config{..} = config
    stopDelegatingEventMap =
      pure (Widget.eventResultFromCursor myId)
      & E.keyPresses focusParentKeys focusParentDoc

setStartDelegatingEventMap :: Config -> Widget f -> Widget f
setStartDelegatingEventMap Config{..} widget =
  widget
  -- We're not delegating, so replace the child eventmap with an
  -- event map to either delegate to it (if it is enterable) or to
  -- nothing (if it is not):
  & Widget.wEventMap .~ newEventMap
  where
    newEventMap =
      case widget ^. Widget.wMaybeEnter of
      Nothing -> mempty
      Just childEnter ->
        E.keyPresses focusChildKeys focusChildDoc $
        childEnter Direction.Outside ^. Widget.enterResultEvent

-- | Make a focus delegator
make
  :: Applicative f
  => IsDelegating -- ^ Start state, enter from direction state
  -> Maybe IsDelegating -- ^ Current state
  -> Widget.Id -- ^ Enter/Stop delegating value
  -> Env -- ^ FocusDelegator configuration
  -> Widget f -> Widget f
make _ (Just cursor) focusSelf env widget = makeFocused cursor focusSelf env widget
make isDelegating Nothing focusSelf env widget =
  widget
  & Widget.wMaybeEnter %~ mEnter isDelegating
  & updateEventMap isDelegating
  where
    wholeSize = widget ^. Widget.wSize

    updateEventMap NotDelegating = setStartDelegatingEventMap (config env)
    updateEventMap Delegating = id

    mEnter NotDelegating _ = Just $ const takeFocus
    mEnter Delegating    Nothing   = Nothing
    mEnter Delegating    (Just enterChild) = Just $ handleDir enterChild

    handleDir _ Direction.Outside = takeFocus
    handleDir enterChild dir = enterChild dir

    takeFocus =
      Widget.EnterResult
      { Widget._enterResultRect = Rect 0 wholeSize
      , Widget._enterResultEvent = pure $ Widget.eventResultFromCursor focusSelf
      }

-- TODO: Don't export these?
delegatingId :: Widget.Id -> Widget.Id
delegatingId = flip Widget.joinId ["delegating"]

notDelegatingId :: Widget.Id -> Widget.Id
notDelegatingId = flip Widget.joinId ["not delegating"]

-- TODO: Make this nicer?!
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
      make entryState mIsDelegating delegatorId env innerWidget
      & Widget.wIsFocused .~ isJust mIsDelegating
      where
        mIsDelegating =
          case Widget.subId delegatorId newCursor of
            Nothing
              | innerWidget ^. Widget.wIsFocused -> Just Delegating
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
