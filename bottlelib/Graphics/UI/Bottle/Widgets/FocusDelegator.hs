{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Graphics.UI.Bottle.Widgets.FocusDelegator
  ( FocusEntryTarget(..)
  , Config(..), Style(..), Env(..)
  , make
  ) where

import           Control.Applicative (Applicative(..))
import           Control.Lens.Operators
import           Data.Monoid (mempty)
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.Animation as Anim
import           Graphics.UI.Bottle.Direction (Direction)
import qualified Graphics.UI.Bottle.Direction as Direction
import qualified Graphics.UI.Bottle.EventMap as E
import           Graphics.UI.Bottle.ModKey (ModKey(..))
import           Graphics.UI.Bottle.Rect (Rect(..))
import           Graphics.UI.Bottle.Widget (Widget)
import qualified Graphics.UI.Bottle.Widget as Widget

data FocusEntryTarget = FocusEntryChild | FocusEntryParent

data Style = Style
  { color :: Draw.Color
  , layer :: Anim.Layer
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

setFocusChildEventMap :: Config -> Widget f -> Widget f
setFocusChildEventMap Config{..} widgetRecord =
  widgetRecord
  -- We're not delegating, so replace the child eventmap with an
  -- event map to either delegate to it (if it is enterable) or to
  -- nothing (if it is not):
  & Widget.eventMap .~ neeventMap
  where
    neeventMap =
      case widgetRecord ^. Widget.mEnter of
      Nothing -> mempty
      Just childEnter ->
        E.keyPresses focusChildKeys focusChildDoc $
        childEnter Direction.Outside ^. Widget.enterResultEvent

modifyEntry ::
  Applicative f =>
  Widget.Id -> Rect -> FocusEntryTarget ->
  Maybe (Direction -> Widget.EnterResult f) ->
  Maybe (Direction -> Widget.EnterResult f)
modifyEntry myId fullChildRect = f
  where
    f FocusEntryParent _ = Just $ const focusParent
    f FocusEntryChild Nothing = Just $ const focusParent
    f FocusEntryChild (Just childEnter) = Just $ wrapEnter childEnter
    wrapEnter _          Direction.Outside = focusParent
    wrapEnter enterChild dir               = enterChild dir

    focusParent =
      Widget.EnterResult
      { Widget._enterResultRect = fullChildRect
      , Widget._enterResultEvent = pure $ Widget.eventResultFromCursor myId
      }

make ::
  Applicative f =>
  Env -> FocusEntryTarget -> Widget.Id ->
  Widget.Env -> Widget f -> Widget f
make Env{..} focusEntryTarget myId env childWidget
  | selfIsFocused =
    childWidget
    & Widget.respondToCursor color layer (env ^. Widget.envCursorAnimId)
    & setFocusChildEventMap config
    -- NOTE: Intentionally not checking whether child is also
    -- focused. That's a bug, which will usefully show up as two
    -- cursors displaying rather than a crash.

  | childIsFocused =
    childWidget
    & Widget.weakerEvents focusParentEventMap

  | otherwise =
    childWidget
    & Widget.focalArea .~ fullChildRect
    & Widget.mEnter %~ modifyEntry myId fullChildRect focusEntryTarget
  where
    fullChildRect = Rect 0 (childWidget ^. Widget.size)
    childIsFocused = childWidget ^. Widget.isFocused
    selfIsFocused = myId == env ^. Widget.envCursor
    Config{..} = config
    Style{..} = style
    focusParentEventMap =
      Widget.keysEventMapMovesCursor focusParentKeys focusParentDoc
      (pure myId)
