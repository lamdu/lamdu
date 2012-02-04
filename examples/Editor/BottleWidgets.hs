{-# OPTIONS -Wall #-}
module Editor.BottleWidgets(
  makeTextView, makeChoice,
  makeFocusableView, makeFocusableTextView,
  wrapDelegatedWithKeys, wrapDelegated,
  makeTextEdit, makeWordEdit, makeNameEdit,
  hbox, hboxAlign,
  vbox, vboxAlign
) where

import Control.Monad (when, liftM)
import Data.List.Utils (enumerate, nth)
import Data.Maybe (isJust)
import Data.Store.IRef (IRef)
import Data.Store.Transaction (Transaction)
import Data.Vector.Vector2 (Vector2(..))
import Editor.CTransaction (TWidget, CTransaction, readTextStyle, readCursor, getP, assignCursor, atTextStyle)
import Editor.MonadF (MonadF)
import Graphics.UI.Bottle.Widget (Widget)
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Editor.Anchors as Anchors
import qualified Editor.WidgetIds as WidgetIds
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.EventMap as EventMap
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Box as Box
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Graphics.UI.Bottle.Widgets.TextEdit as TextEdit
import qualified Graphics.UI.Bottle.Widgets.TextView as TextView

makeTextView :: MonadF m => String -> Widget.Id -> TWidget t m
makeTextView text myId = do
  style <- readTextStyle
  return $
    TextView.makeWidget (TextEdit.sTextViewStyle style) text $ Widget.cursorId myId

makeFocusableView :: MonadF m => Widget (Transaction t m) -> Widget.Id -> TWidget t m
makeFocusableView widget animId = do
  hasFocus <- liftM (animId ==) readCursor
  let
    setBackground
      | hasFocus = Widget.backgroundColor WidgetIds.backgroundCursorId blue
      | otherwise = id
  return .
    (Widget.atIsFocused . const) hasFocus . setBackground $
    Widget.takesFocus (const (return animId)) widget
  where
    blue = Draw.Color 0 0 1 0.8

makeFocusableTextView :: MonadF m => String -> Widget.Id -> TWidget t m
makeFocusableTextView text myId = do
  textView <- makeTextView text myId
  makeFocusableView textView myId

makeChoice ::
  (Monad m) =>
  Widget.Id -> Transaction.Property t m Int -> Box.Orientation ->
  [TWidget t m] -> TWidget t m
makeChoice selectionAnimId curChoiceRef orientation children = do
  curChoice <- getP curChoiceRef
  focusables <- sequence children
  let
    widget =
      Box.makeBiased orientation curChoice .
      nth curChoice (Widget.backgroundColor selectionAnimId selectedColor) .
      map updateCurChoice $
      enumerate focusables
  return widget
  where
    updateCurChoice (i, focusable) =
      Widget.atEvents (Property.set curChoiceRef i >>) focusable
    selectedColor = Draw.Color 0 0.5 0 1

-- TODO: This logic belongs in the FocusDelegator itself
wrapDelegatedWithKeys ::
  Monad m => FocusDelegator.Keys ->
  FocusDelegator.IsDelegating ->
  ((Widget (Transaction t m) ->
    Widget (Transaction t m)) -> a -> b) ->
  (Widget.Id -> CTransaction t m a) ->
  Widget.Id -> CTransaction t m b
wrapDelegatedWithKeys keys entryState atWidget mkResult myId = do
  let
    innerId = WidgetIds.delegating myId
    delegatorId = WidgetIds.notDelegating myId
    destId =
      case entryState of
        FocusDelegator.NotDelegating -> delegatorId
        FocusDelegator.Delegating -> innerId
  assignCursor myId destId $ do
    innerResult <- mkResult innerId
    cursor <- readCursor
    let
      cursorSelf = Just FocusDelegator.NotDelegating
      cursorNotSelf innerWidget
        | Widget.isFocused innerWidget = Just FocusDelegator.Delegating
        | otherwise = Nothing
      makeDelegator delegateState =
        (Widget.atIsFocused . const) (isJust delegateState) .
        FocusDelegator.make entryState delegateState delegatorId keys WidgetIds.backgroundCursorId
      onWidget innerWidget =
        (`makeDelegator` innerWidget) .
        maybe (cursorNotSelf innerWidget) (const cursorSelf) .
        Widget.subId delegatorId $
        cursor
    return $ atWidget onWidget innerResult

wrapDelegated ::
  Monad m => FocusDelegator.IsDelegating ->
  (Widget.Id -> TWidget t m) ->
  Widget.Id -> TWidget t m
wrapDelegated entryState =
  wrapDelegatedWithKeys FocusDelegator.defaultKeys entryState id

makeTextEdit ::
  Monad m =>
  Transaction.Property t m String ->
  Widget.Id -> TWidget t m
makeTextEdit textRef myId = do
  text <- getP textRef
  let
    lifter (newText, eventRes) = do
      when (newText /= text) $ Property.set textRef newText
      return eventRes
  cursor <- readCursor
  style <- readTextStyle
  return .
    Widget.atEvents lifter $
    TextEdit.make style cursor text myId

makeWordEdit ::
  Monad m =>
  Transaction.Property t m String ->
  Widget.Id -> TWidget t m
makeWordEdit = (fmap . fmap . liftM . Widget.atEventMap) removeWordSeparators makeTextEdit
  where
    compose = foldr (.) id
    removeWordSeparators = compose $ map EventMap.delete [newlineKey, newwordKey]
    newlineKey = EventMap.KeyEventType EventMap.noMods EventMap.KeyEnter
    newwordKey = EventMap.SpaceKeyEventType EventMap.noMods

makeNameEdit :: Monad m => String -> IRef a -> Widget.Id -> TWidget t m
makeNameEdit emptyStr iref =
  (atTextStyle . TextEdit.atSEmptyString . const) emptyStr . makeWordEdit (Anchors.aNameRef iref)

hboxAlign :: Widget.R -> [Widget f] -> Widget f
hboxAlign align = Box.make Box.horizontal . map (Widget.align (Vector2 0 align))

hbox :: [Widget f] -> Widget f
hbox = hboxAlign 0.5

vboxAlign :: Widget.R -> [Widget f] -> Widget f
vboxAlign align = Box.make Box.vertical . map (Widget.align (Vector2 align 0))

vbox :: [Widget f] -> Widget f
vbox = vboxAlign 0.5
