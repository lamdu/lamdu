{-# LANGUAGE TemplateHaskell #-}
module Lamdu.GUI.BottleWidgets
  ( makeTextView, makeTextViewWidget, makeLabel
  , makeFocusableView
  , makeFocusableTextView, makeFocusableLabel
  , wrapDelegatedWith, wrapDelegatedOT
  , makeTextEdit
  , makeTextEditor, makeLineEdit, makeWordEdit
  , stdSpaceWidget
  , hboxSpaced, hboxCenteredSpaced
  , gridHSpaced, gridHSpacedCentered
  , makeChoiceWidget, ChoiceWidgetConfig(..), ChoiceWidgetExpandMode(..)
  ) where

import Control.Applicative (Applicative(..), (*>), (<$>))
import Control.Lens.Operators
import Control.Monad (when)
import Control.MonadA (MonadA)
import Data.ByteString.Char8 (pack)
import Data.List (findIndex, intersperse)
import Data.Maybe (isJust)
import Data.Monoid (mappend)
import Data.Store.Property (Property)
import Graphics.UI.Bottle.Animation (AnimId)
import Graphics.UI.Bottle.View (View)
import Graphics.UI.Bottle.Widget (Widget)
import Lamdu.Config (Config)
import Lamdu.GUI.WidgetEnvT (WidgetEnvT)
import qualified Control.Lens as Lens
import qualified Data.Store.Property as Property
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.EventMap as EventMap
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Box as Box
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Graphics.UI.Bottle.Widgets.Grid as Grid
import qualified Graphics.UI.Bottle.Widgets.Spacer as Spacer
import qualified Graphics.UI.Bottle.Widgets.TextEdit as TextEdit
import qualified Graphics.UI.Bottle.Widgets.TextView as TextView
import qualified Lamdu.Config as Config
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.GUI.WidgetEnvT as WE

makeTextView ::
  MonadA m => String -> AnimId -> WidgetEnvT m View
makeTextView text myId = do
  style <- WE.readTextStyle
  return $
    TextView.make (style ^. TextEdit.sTextViewStyle) text myId

makeTextViewWidget :: MonadA m => String -> AnimId -> WidgetEnvT m (Widget f)
makeTextViewWidget text myId =
  uncurry Widget.liftView <$> makeTextView text myId

makeLabel :: MonadA m => String -> AnimId -> WidgetEnvT m (Widget f)
makeLabel text prefix =
  makeTextViewWidget text $ mappend prefix [pack text]

makeFocusableView
  :: (Applicative f, MonadA m)
  => Widget.Id -> Widget f
  -> WidgetEnvT m (Widget f)
makeFocusableView myId widget = do
  hasFocus <- fmap isJust $ WE.subCursor myId
  config <- WE.readConfig
  let
    setBackground
      | hasFocus =
        Widget.backgroundColor
        (Config.layerCursorBG (Config.layers config))
        WidgetIds.backgroundCursorId $ Config.cursorBGColor config
      | otherwise = id
  return .
    (Widget.wIsFocused .~ hasFocus) . setBackground $
    Widget.takesFocus (const (pure myId)) widget

makeFocusableTextView
  :: (Applicative f, MonadA m)
  => String -> Widget.Id
  -> WidgetEnvT m (Widget f)
makeFocusableTextView text myId = do
  textView <- makeTextViewWidget text $ Widget.toAnimId myId
  makeFocusableView myId textView

makeFocusableLabel
  :: (Applicative f, MonadA m)
  => String -> Widget.Id
  -> WidgetEnvT m (Widget f)
makeFocusableLabel text myIdPrefix = do
  textView <- makeTextViewWidget text $ Widget.toAnimId myId
  makeFocusableView myId textView
  where
    myId = Widget.joinId myIdPrefix [pack text]

fdStyle :: Config -> FocusDelegator.Style
fdStyle config = FocusDelegator.Style
  { FocusDelegator.color = Config.cursorBGColor config
  , FocusDelegator.layer = Config.layerCursorBG $ Config.layers config
  , FocusDelegator.cursorBGAnimId = WidgetIds.backgroundCursorId
  }

-- TODO: Clean this hell up:
wrapDelegatedWith ::
  (Applicative f, MonadA m) =>
  m Widget.Id -> m Config ->
  ((Widget.Id -> Widget.Id) -> m a -> m a) ->
  FocusDelegator.Config ->
  FocusDelegator.IsDelegating ->
  ((Widget f -> Widget f) -> a -> b) ->
  (Widget.Id -> m a) ->
  Widget.Id -> m b
wrapDelegatedWith readCursor readConfig atCursor fdConfig entryState aToB mkA myId = do
  cursor <- readCursor
  config <- readConfig
  FocusDelegator.wrapEnv (FocusDelegator.Env fdConfig (fdStyle config)) entryState mk myId cursor
  where
    mk f innerId newCursor =
      fmap (aToB f) . (atCursor . const) newCursor $ mkA innerId

-- TODO: This logic belongs in the FocusDelegator itself
wrapDelegatedOT
  :: (Applicative f, MonadA m)
  => FocusDelegator.Config
  -> FocusDelegator.IsDelegating
  -> ((Widget f -> Widget f) -> a -> b)
  -> (Widget.Id -> WidgetEnvT m a)
  -> Widget.Id -> WidgetEnvT m b
wrapDelegatedOT = wrapDelegatedWith WE.readCursor WE.readConfig (WE.localEnv . (WE.envCursor %~))

makeTextEdit ::
  MonadA m =>
  String -> Widget.Id ->
  WidgetEnvT m (Widget ((,) String))
makeTextEdit text myId =
  TextEdit.make <$> WE.readTextStyle <*> WE.readCursor <*> pure text <*> pure myId

makeTextEditor
  :: (MonadA m, MonadA f)
  => Property f String
  -> Widget.Id
  -> WidgetEnvT m (Widget f)
makeTextEditor textRef myId =
  Widget.atEvents setter <$>
  makeTextEdit (Property.value textRef) myId
  where
    setter (newText, eventRes) = do
      when (newText /= Property.value textRef) $ Property.set textRef newText
      return eventRes

deleteKeyEventHandler :: EventMap.ModKey -> Widget f -> Widget f
deleteKeyEventHandler key = Widget.wEventMap %~ EventMap.deleteKey (EventMap.KeyEvent EventMap.Press key)

noMods :: EventMap.Key -> EventMap.ModKey
noMods = EventMap.ModKey EventMap.noMods

makeLineEdit ::
  (MonadA m, MonadA f) =>
  Property f String ->
  Widget.Id ->
  WidgetEnvT m (Widget f)
makeLineEdit textRef myId =
  makeTextEditor textRef myId <&> deleteKeyEventHandler (noMods EventMap.KeyEnter)

makeWordEdit ::
  (MonadA m, MonadA f) =>
  Property f String ->
  Widget.Id ->
  WidgetEnvT m (Widget f)
makeWordEdit textRef myId =
  makeLineEdit textRef myId <&> deleteKeyEventHandler (noMods EventMap.KeySpace)

stdSpaceWidget :: Widget f
stdSpaceWidget = uncurry Widget.liftView $ Spacer.makeHorizontal 20

hboxSpaced :: [(Box.Alignment, Widget f)] -> Widget f
hboxSpaced = Box.hbox . intersperse (0.5, stdSpaceWidget)

hboxCenteredSpaced :: [Widget f] -> Widget f
hboxCenteredSpaced = Box.hboxAlign 0.5 . intersperse stdSpaceWidget

gridHSpaced :: [[(Grid.Alignment, Widget f)]] -> Widget f
gridHSpaced = Grid.toWidget . Grid.make . map (intersperse (0, stdSpaceWidget))

gridHSpacedCentered :: [[Widget f]] -> Widget f
gridHSpacedCentered = gridHSpaced . (map . map) ((,) 0.5)

data ChoiceWidgetExpandMode
  -- Cursor is on expanded widget, need to show selected choice with a
  -- color:
  = AutoExpand Draw.Color
  | ExplicitEntry
Lens.makePrisms ''ChoiceWidgetExpandMode

data ChoiceWidgetConfig = ChoiceWidgetConfig
  { cwcFDConfig :: FocusDelegator.Config
  , cwcOrientation :: Box.Orientation
  , cwcExpandMode :: ChoiceWidgetExpandMode
  , cwcBgLayer :: Int
  }

makeChoiceWidget ::
  (Eq a, MonadA m, Applicative f) =>
  (a -> f ()) -> [(a, Widget f)] -> a ->
  ChoiceWidgetConfig -> Widget.Id -> WidgetEnvT m (Widget f)
makeChoiceWidget choose children curChild config myId = do
  isFocused <- WE.isSubCursor myId
  let
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
  wrapDelegatedOT (cwcFDConfig config) FocusDelegator.NotDelegating id
    ((const . return) boxWidget) myId
  where
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
    childFocused = any (^. Lens._2 . Widget.wIsFocused) children
    mkPair (item, widget) =
      ( item
      , widget
        & Widget.wMaybeEnter . Lens.traversed . Lens.mapped .
          Widget.enterResultEvent %~ (choose item *>)
      )
