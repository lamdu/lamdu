{-# LANGUAGE TemplateHaskell, RecordWildCards #-}
-- | A vertical-expand (combo-like) choice widget

module Graphics.UI.Bottle.Widgets.Choice
    ( make
    , IsSelected(..)
    , Config(..)
    , ExpandMode(..)
    ) where

import           Control.Applicative (Applicative(..), (*>))
import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Lens.Tuple
import           Data.List (findIndex)
import qualified Graphics.DrawingCombinators as Draw
import           Graphics.UI.Bottle.Widget (Widget)
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Box as Box
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator

data ExpandMode
  -- Cursor is on expanded widget, need to show selected choice with a
  -- color:
  = AutoExpand Draw.Color
  | ExplicitEntry
Lens.makePrisms ''ExpandMode

data Config = Config
  { cwcFDConfig :: FocusDelegator.Config
  , cwcOrientation :: Box.Orientation
  , cwcExpandMode :: ExpandMode
  , cwcBgLayer :: Int
  }

data IsSelected = Selected | NotSelected
  deriving Eq

toBox ::
  Applicative f => Config -> Bool ->
  Widget.Id -> [(IsSelected, f (), Widget f)] ->
  Widget f
toBox Config{..} selfFocused myId childrenRecords =
  childrenRecords
  <&> applyAction
  & filterVisible
  <&> colorize
  & Box.makeAlign 0 cwcOrientation
  & maybe Box.toWidget Box.toWidgetBiased mCurChildIndex
  where
    mCurChildIndex = childrenRecords & findIndex ((== Selected) . (^. _1))
    filterVisible
      | anyChildFocused || (autoExpand && selfFocused) = id
      | otherwise = filter ((== Selected) . fst)
    autoExpand = Lens.has _AutoExpand cwcExpandMode
    colorize (isSelected, widget)
      | anyChildFocused = widget -- focus shows selection already
      | otherwise = -- need to show selection even as focus is elsewhere
        widget
        & case cwcExpandMode of
          AutoExpand color
            | isSelected == Selected ->
              Widget.backgroundColor cwcBgLayer (Widget.toAnimId myId) color
          _ -> id
    applyAction (isSelected, action, widget) =
      ( isSelected
      , widget
        & Widget.mEnter . Lens._Just . Lens.mapped .
          Widget.enterResultEvent %~ (action *>)
      )
    anyChildFocused =
      childrenRecords
      & Lens.orOf (Lens.traversed . _3 . Widget.isFocused)

make ::
  Applicative f =>
  FocusDelegator.Style -> Config ->
  [(IsSelected, f (), Widget f)] ->
  Widget.Id -> Widget.Env -> Widget f
make fdStyle Config{..} children myId widgetEnv =
  FocusDelegator.make FocusDelegator.Env
  { FocusDelegator.config = cwcFDConfig
  , FocusDelegator.style = fdStyle
  } FocusDelegator.FocusEntryParent myId widgetEnv childrenBox
  where
    childrenBox = toBox Config{..} selfFocused myId children
    selfFocused =
      widgetEnv ^. Widget.envCursor
      & Widget.subId myId
      & Lens.has Lens._Just
