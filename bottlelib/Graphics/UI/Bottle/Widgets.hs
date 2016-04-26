{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Graphics.UI.Bottle.Widgets
    ( makeTextView, makeTextViewWidget, makeLabel
    , makeFocusableView
    , makeFocusableTextView, makeFocusableLabel
    , makeTextEdit
    , makeTextEditor, makeLineEdit, makeWordEdit
    , makeFocusDelegator
    , makeChoiceWidget
    , stdSpaceWidth, stdSpaceView
    , stdSpaceHeight, stdVSpaceView
    , hspaceWidget, vspaceWidget
    , hboxSpaced, hboxCenteredSpaced
    , gridHSpaced, gridHSpacedCentered
    , liftLayerInterval
    , respondToCursorPrefix
    ) where

import           Control.Lens.Operators
import           Control.Monad (when)
import           Data.ByteString.Char8 (pack)
import           Data.List (intersperse)
import           Data.Store.Property (Property)
import qualified Data.Store.Property as Property
import qualified Graphics.DrawingCombinators as Draw
import           Graphics.UI.Bottle.Animation (AnimId)
import qualified Graphics.UI.Bottle.EventMap as EventMap
import           Graphics.UI.Bottle.ModKey (ModKey(..))
import           Graphics.UI.Bottle.Rect (Rect(..))
import           Graphics.UI.Bottle.View (View)
import qualified Graphics.UI.Bottle.View as View
import           Graphics.UI.Bottle.Widget (Widget)
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Box as Box
import qualified Graphics.UI.Bottle.Widgets.Choice as Choice
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Graphics.UI.Bottle.Widgets.Grid as Grid
import qualified Graphics.UI.Bottle.Widgets.Spacer as Spacer
import qualified Graphics.UI.Bottle.Widgets.TextEdit as TextEdit
import qualified Graphics.UI.Bottle.Widgets.TextView as TextView
import           Graphics.UI.Bottle.WidgetsEnvT (WidgetEnvT)
import qualified Graphics.UI.Bottle.WidgetsEnvT as WE
import qualified Graphics.UI.GLFW as GLFW

import           Prelude.Compat

makeTextView ::
    Monad m => String -> AnimId -> WidgetEnvT m View
makeTextView text myId = do
    style <- WE.readTextStyle
    return $
        TextView.make (style ^. TextEdit.sTextViewStyle) text myId

makeTextViewWidget :: Monad m => String -> AnimId -> WidgetEnvT m (Widget f)
makeTextViewWidget text myId =
    Widget.fromView <$> makeTextView text myId

makeLabel :: Monad m => String -> AnimId -> WidgetEnvT m (Widget f)
makeLabel text prefix =
    makeTextViewWidget text $ mappend prefix [pack text]

liftLayerInterval :: Monad m => Widget f -> WidgetEnvT m (Widget f)
liftLayerInterval widget =
    do
        env <- WE.readEnv
        let layerDiff = WE.layerInterval env
        widget & Widget.animLayers -~ layerDiff & return

readEnv :: Monad m => WidgetEnvT m Widget.Env
readEnv =
    do
        env <- WE.readEnv
        Widget.Env (env ^. WE.envCursor) (WE.backgroundCursorId env) & return

respondToCursorPrefix ::
    Monad m => Widget.Id -> Widget f -> WidgetEnvT m (Widget f)
respondToCursorPrefix myIdPrefix widget = do
    env <- WE.readEnv
    widgetEnv <- readEnv
    widget
        & Widget.respondToCursorPrefix myIdPrefix
            (WE.cursorBGColor env)
            (WE.layerCursor env)
            widgetEnv
        & return

makeFocusableView ::
    (Applicative f, Monad m) => Widget.Id ->
    Widget f -> WidgetEnvT m (Widget f)
makeFocusableView myIdPrefix widget =
    widget
    & Widget.focalArea .~ Rect 0 (widget ^. Widget.view . View.size)
    -- TODO: make it non-prefix-related?
    & Widget.takesFocus (const (pure myIdPrefix))
    & respondToCursorPrefix myIdPrefix

makeFocusableTextView
    :: (Applicative f, Monad m)
    => String -> Widget.Id
    -> WidgetEnvT m (Widget f)
makeFocusableTextView text myId = do
    textView <- makeTextViewWidget text $ Widget.toAnimId myId
    makeFocusableView myId textView

makeFocusableLabel
    :: (Applicative f, Monad m)
    => String -> Widget.Id
    -> WidgetEnvT m (Widget f)
makeFocusableLabel text myIdPrefix =
    makeFocusableTextView text (Widget.joinId myIdPrefix [pack text])

fdStyle :: WE.Env -> FocusDelegator.Style
fdStyle env = FocusDelegator.Style
    { FocusDelegator.color = WE.cursorBGColor env
    , FocusDelegator.layer = WE.layerCursor env
    }

makeFocusDelegator ::
    (Applicative f, Monad m) =>
    FocusDelegator.Config ->
    FocusDelegator.FocusEntryTarget ->
    Widget.Id ->
    Widget f -> WidgetEnvT m (Widget f)
makeFocusDelegator fdConfig focusEntryTarget myId childWidget =
    do
        env <- readEnv
        fdEnv <- WE.readEnv <&> fdStyle <&> FocusDelegator.Env fdConfig
        FocusDelegator.make fdEnv focusEntryTarget myId env childWidget & return

makeTextEdit ::
    Monad m =>
    String -> Widget.Id ->
    WidgetEnvT m (Widget ((,) String))
makeTextEdit text myId =
    do
        style <- WE.readTextStyle
        env <- readEnv
        TextEdit.make style text myId env & return

makeTextEditor
    :: (Monad m, Applicative f)
    => Property f String
    -> Widget.Id
    -> WidgetEnvT m (Widget f)
makeTextEditor textRef myId =
    makeTextEdit (Property.value textRef) myId
    <&> Widget.events %~ setter
    where
        setter (newText, eventRes) =
            eventRes <$
            when (newText /= Property.value textRef) (Property.set textRef newText)

deleteKeyEventHandler :: ModKey -> Widget f -> Widget f
deleteKeyEventHandler key =
    Widget.eventMap %~
    EventMap.deleteKey (EventMap.KeyEvent GLFW.KeyState'Pressed key)

-- TODO: Editor, not Edit (consistent with makeTextEditor vs. makeTextEdit)
makeLineEdit ::
    (Monad m, Applicative f) =>
    Property f String ->
    Widget.Id ->
    WidgetEnvT m (Widget f)
makeLineEdit textRef myId =
    makeTextEditor textRef myId <&> deleteKeyEventHandler (ModKey mempty GLFW.Key'Enter)

makeWordEdit ::
    (Monad m, Applicative f) =>
    Property f String ->
    Widget.Id ->
    WidgetEnvT m (Widget f)
makeWordEdit textRef myId =
    makeLineEdit textRef myId <&> deleteKeyEventHandler (ModKey mempty GLFW.Key'Space)

hspaceWidget :: Widget.R -> Widget f
hspaceWidget = Widget.fromView . Spacer.makeHorizontal

vspaceWidget :: Widget.R -> Widget f
vspaceWidget = Widget.fromView . Spacer.makeVertical

stdFont :: Monad m => WidgetEnvT m Draw.Font
stdFont = WE.readEnv <&> (^. WE.envTextStyle . TextEdit.sTextViewStyle . TextView.styleFont)

stdSpaceHeight :: Monad m => WidgetEnvT m Double
stdSpaceHeight =
    (*) <$> (stdFont <&> Draw.fontHeight) <*> (WE.readEnv <&> WE.verticalSpacing)

stdSpaceWidth :: Monad m => WidgetEnvT m Double
stdSpaceWidth = stdFont <&> (`Draw.textAdvance` " ")

stdSpaceView :: Monad m => WidgetEnvT m View
stdSpaceView = stdSpaceWidth <&> realToFrac <&> Spacer.make

stdVSpaceView :: Monad m => WidgetEnvT m View
stdVSpaceView = stdSpaceHeight <&> realToFrac <&> Spacer.make

hboxSpaced :: Monad m => [(Box.Alignment, Widget f)] -> WidgetEnvT m (Widget f)
hboxSpaced widgets =
    stdSpaceView
    <&> Widget.fromView
    <&> Box.hbox . (`intersperse` widgets) . (,) 0.5

hboxCenteredSpaced :: Monad m => [Widget f] -> WidgetEnvT m (Widget f)
hboxCenteredSpaced widgets =
    stdSpaceView
    <&> Widget.fromView
    <&> Box.hboxAlign 0.5 . (`intersperse` widgets)

gridHSpaced :: Monad m => [[(Grid.Alignment, Widget f)]] -> WidgetEnvT m (Widget f)
gridHSpaced xs =
    stdSpaceView
    <&> Widget.fromView
    <&> Grid.toWidget . Grid.make . (`map` xs) . intersperse . (,) 0

gridHSpacedCentered :: Monad m => [[Widget f]] -> WidgetEnvT m (Widget f)
gridHSpacedCentered = gridHSpaced . (map . map) ((,) 0.5)

makeChoiceWidget ::
    (Eq a, Monad m, Applicative f) =>
    (a -> f ()) -> [(a, Widget f)] -> a ->
    Choice.Config -> Widget.Id -> WidgetEnvT m (Widget f)
makeChoiceWidget choose children curChild choiceConfig myId = do
    env <- WE.readEnv
    widgetEnv <- readEnv
    Choice.make (fdStyle env) choiceConfig (children <&> annotate) myId widgetEnv
        & return
    where
        annotate (item, widget) =
            ( if item == curChild then Choice.Selected else Choice.NotSelected
            , choose item
            , widget
            )
