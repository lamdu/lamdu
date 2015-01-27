{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Lamdu.GUI.BottleWidgets
  ( makeTextView, makeTextViewWidget, makeLabel
  , makeFocusableView
  , makeFocusableTextView, makeFocusableLabel
  , makeTextEdit
  , makeTextEditor, makeLineEdit, makeWordEdit
  , makeFocusDelegator
  , makeChoiceWidget
  , stdSpaceWidget, hspaceWidget, vspaceWidget
  , hboxSpaced, hboxCenteredSpaced
  , gridHSpaced, gridHSpacedCentered
  , verticalSpace
  , liftLayerInterval
  , respondToCursorPrefix
  ) where

import           Control.Applicative (Applicative(..), (<$>))
import           Control.Lens.Operators
import           Control.Monad (when)
import           Control.MonadA (MonadA)
import           Data.ByteString.Char8 (pack)
import           Data.List (intersperse)
import           Data.Monoid (Monoid(..))
import           Data.Store.Property (Property)
import qualified Data.Store.Property as Property
import           Graphics.UI.Bottle.Animation (AnimId)
import qualified Graphics.UI.Bottle.EventMap as EventMap
import           Graphics.UI.Bottle.ModKey (ModKey(..))
import           Graphics.UI.Bottle.View (View)
import           Graphics.UI.Bottle.Widget (Widget)
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Box as Box
import qualified Graphics.UI.Bottle.Widgets.Choice as Choice
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Graphics.UI.Bottle.Widgets.Grid as Grid
import qualified Graphics.UI.Bottle.Widgets.Spacer as Spacer
import qualified Graphics.UI.Bottle.Widgets.TextEdit as TextEdit
import qualified Graphics.UI.Bottle.Widgets.TextView as TextView
import qualified Graphics.UI.GLFW as GLFW
import           Lamdu.Config (Config)
import qualified Lamdu.Config as Config
import           Lamdu.GUI.WidgetEnvT (WidgetEnvT)
import qualified Lamdu.GUI.WidgetEnvT as WE
import qualified Lamdu.GUI.WidgetIds as WidgetIds

makeTextView ::
  MonadA m => String -> AnimId -> WidgetEnvT m View
makeTextView text myId = do
  style <- WE.readTextStyle
  return $
    TextView.make (style ^. TextEdit.sTextViewStyle) text myId

makeTextViewWidget :: MonadA m => String -> AnimId -> WidgetEnvT m (Widget f)
makeTextViewWidget text myId =
  Widget.fromView <$> makeTextView text myId

makeLabel :: MonadA m => String -> AnimId -> WidgetEnvT m (Widget f)
makeLabel text prefix =
  makeTextViewWidget text $ mappend prefix [pack text]

verticalSpace :: MonadA m => WidgetEnvT m (Widget f)
verticalSpace = do
  config <- WE.readConfig
  return $ vspaceWidget $ realToFrac $ Config.verticalSpacing config

liftLayerInterval :: Config -> Widget f -> Widget f
liftLayerInterval config =
  Widget.wAnimLayers -~ layerDiff
  where
    layerDiff = Config.layerInterval (Config.layers config)

readEnv :: MonadA m => WidgetEnvT m Widget.Env
readEnv =
  do
    cursor <- WE.readCursor
    Widget.Env cursor WidgetIds.backgroundCursorId & return

respondToCursorPrefix ::
  MonadA m => Widget.Id -> Widget f -> WidgetEnvT m (Widget f)
respondToCursorPrefix myIdPrefix widget = do
  config <- WE.readConfig
  widgetEnv <- readEnv
  widget
    & Widget.respondToCursorPrefix myIdPrefix
      (Config.cursorBGColor config)
      (Config.layerCursor (Config.layers config))
      widgetEnv
    & return

makeFocusableView ::
  (Applicative f, MonadA m) => Widget.Id ->
  Widget f -> WidgetEnvT m (Widget f)
makeFocusableView myIdPrefix widget =
  widget
  -- TODO: make it non-prefix-related?
  & Widget.takesFocus (const (pure myIdPrefix))
  & respondToCursorPrefix myIdPrefix

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
  , FocusDelegator.layer = Config.layerCursor $ Config.layers config
  }

makeFocusDelegator ::
  (Applicative f, MonadA m) =>
  FocusDelegator.Config ->
  FocusDelegator.FocusEntryTarget ->
  Widget.Id ->
  Widget f -> WidgetEnvT m (Widget f)
makeFocusDelegator fdConfig focusEntryTarget myId childWidget =
  do
    env <- readEnv
    fdEnv <- WE.readConfig <&> fdStyle <&> FocusDelegator.Env fdConfig
    FocusDelegator.make fdEnv focusEntryTarget myId env childWidget & return

makeTextEdit ::
  MonadA m =>
  String -> Widget.Id ->
  WidgetEnvT m (Widget ((,) String))
makeTextEdit text myId =
  do
    style <- WE.readTextStyle
    env <- readEnv
    TextEdit.make style text myId env & return

makeTextEditor
  :: (MonadA m, MonadA f)
  => Property f String
  -> Widget.Id
  -> WidgetEnvT m (Widget f)
makeTextEditor textRef myId =
  makeTextEdit (Property.value textRef) myId
  <&> Widget.wEvents %~ setter
  where
    setter (newText, eventRes) = do
      when (newText /= Property.value textRef) $ Property.set textRef newText
      return eventRes

deleteKeyEventHandler :: ModKey -> Widget f -> Widget f
deleteKeyEventHandler key =
  Widget.wEventMap %~
  EventMap.deleteKey (EventMap.KeyEvent GLFW.KeyState'Pressed key)

-- TODO: Editor, not Edit (consistent with makeTextEditor vs. makeTextEdit)
makeLineEdit ::
  (MonadA m, MonadA f) =>
  Property f String ->
  Widget.Id ->
  WidgetEnvT m (Widget f)
makeLineEdit textRef myId =
  makeTextEditor textRef myId <&> deleteKeyEventHandler (ModKey mempty GLFW.Key'Enter)

makeWordEdit ::
  (MonadA m, MonadA f) =>
  Property f String ->
  Widget.Id ->
  WidgetEnvT m (Widget f)
makeWordEdit textRef myId =
  makeLineEdit textRef myId <&> deleteKeyEventHandler (ModKey mempty GLFW.Key'Space)

hspaceWidget :: Widget.R -> Widget f
hspaceWidget = Widget.fromView . Spacer.makeHorizontal

vspaceWidget :: Widget.R -> Widget f
vspaceWidget = Widget.fromView . Spacer.makeVertical

stdSpaceWidget :: MonadA m => WidgetEnvT m (Widget f)
stdSpaceWidget =
  WE.readConfig
  <&> Widget.fromView . Spacer.make . realToFrac . Config.spaceWidth

hboxSpaced :: MonadA m => [(Box.Alignment, Widget f)] -> WidgetEnvT m (Widget f)
hboxSpaced widgets =
  stdSpaceWidget
  <&> Box.hbox . (`intersperse` widgets) . (,) 0.5

hboxCenteredSpaced :: MonadA m => [Widget f] -> WidgetEnvT m (Widget f)
hboxCenteredSpaced widgets =
  stdSpaceWidget
  <&> Box.hboxAlign 0.5 . (`intersperse` widgets)

gridHSpaced :: MonadA m => [[(Grid.Alignment, Widget f)]] -> WidgetEnvT m (Widget f)
gridHSpaced xs =
  stdSpaceWidget
  <&> Grid.toWidget . Grid.make . (`map` xs) . intersperse . (,) 0

gridHSpacedCentered :: MonadA m => [[Widget f]] -> WidgetEnvT m (Widget f)
gridHSpacedCentered = gridHSpaced . (map . map) ((,) 0.5)

makeChoiceWidget ::
  (Eq a, MonadA m, Applicative f) =>
  (a -> f ()) -> [(a, Widget f)] -> a ->
  Choice.Config -> Widget.Id -> WidgetEnvT m (Widget f)
makeChoiceWidget choose children curChild choiceConfig myId = do
  config <- WE.readConfig
  env <- readEnv
  Choice.make (fdStyle config) choiceConfig (children <&> annotate) myId env
    & return
  where
    annotate (item, widget) =
      ( if item == curChild then Choice.Selected else Choice.NotSelected
      , choose item
      , widget
      )
