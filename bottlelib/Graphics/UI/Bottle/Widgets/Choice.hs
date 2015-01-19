{-# LANGUAGE TemplateHaskell #-}
-- | A vertical-expand (combo-like) choice widget

module Graphics.UI.Bottle.Widgets.Choice
    ( make
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

make ::
  (Eq a, Applicative f) =>
  (a -> f ()) -> [(a, Widget f)] -> a ->
  FocusDelegator.Style ->
  Config -> Widget.Id -> Widget.Id -> Widget f
make choose children curChild fdStyle config myId cursor = do
  FocusDelegator.wrapEnv (FocusDelegator.Env (cwcFDConfig config) fdStyle)
    FocusDelegator.NotDelegating mkResult myId cursor
  where
    visiblePairs
      | childFocused || (autoExpand && isFocused) = pairs
      | otherwise = filter itemIsCurChild pairs
    mCurChildIndex = findIndex itemIsCurChild visiblePairs
    colorizedPairs
      -- focus shows selection already
      | childFocused = map snd visiblePairs
      -- need to show selection even as focus is elsewhere
      | otherwise = map colorize visiblePairs
    box = Box.makeAlign 0 (cwcOrientation config) colorizedPairs
    boxWidget =
      maybe Box.toWidget Box.toWidgetBiased mCurChildIndex box
    mkResult f _innerId _newCursor = f boxWidget
    isFocused = Lens.has Lens._Just (Widget.subId myId cursor)
    autoExpand = Lens.has _AutoExpand $ cwcExpandMode config
    itemIsCurChild = (curChild ==) . fst
    colorize (item, w) =
      case cwcExpandMode config ^? _AutoExpand of
      Just color
        | item == curChild ->
          Widget.backgroundColor (cwcBgLayer config)
          (Widget.toAnimId myId) color w
      _ -> w
    pairs = map mkPair children
    childFocused = any (^. _2 . Widget.wIsFocused) children
    mkPair (item, widget) =
      ( item
      , widget
        & Widget.wMaybeEnter . Lens.traversed . Lens.mapped .
          Widget.enterResultEvent %~ (choose item *>)
      )
