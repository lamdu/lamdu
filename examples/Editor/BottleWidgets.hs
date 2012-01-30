{-# OPTIONS -Wall #-}
module Editor.BottleWidgets(
  makeTextView, makeChoice,
  makeFocusableTextView,
  wrapDelegatedWithKeys, wrapDelegated,
  makeTextEdit, makeWordEdit, makeNameEdit,
  hbox, vbox
) where

import Control.Monad (when, liftM)
import Data.List.Utils (enumerate, nth)
import Data.Maybe (isJust)
import Data.Store.IRef (IRef)
import Data.Store.Transaction (Transaction)
import Data.Vector.Vector2 (Vector2(..))
import Editor.CTransaction (TWidget, CTransaction, readTextStyle, readCursor, getP, assignCursor, atEmptyStr)
import Editor.MonadF (MonadF)
import Graphics.UI.Bottle.Widget (Widget)
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Editor.Anchors as Anchors
import qualified Editor.AnimIds as AnimIds
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.EventMap as EventMap
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Box as Box
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Graphics.UI.Bottle.Widgets.TextEdit as TextEdit
import qualified Graphics.UI.Bottle.Widgets.TextView as TextView

makeTextView :: MonadF m => String -> Anim.AnimId -> TWidget t m
makeTextView text animId = do
  style <- readTextStyle
  return $
    TextView.makeWidget (TextEdit.sTextViewStyle style) text animId

makeFocusableTextView :: MonadF m => String -> Anim.AnimId -> TWidget t m
makeFocusableTextView text animId = do
  hasFocus <- liftM (animId ==) readCursor
  textView <- makeTextView text animId
  let
    setBackground
      | hasFocus = Widget.backgroundColor AnimIds.backgroundCursorId blue
      | otherwise = id
  return .
    (Widget.atIsFocused . const) hasFocus . setBackground $
    Widget.takesFocus (const (return animId)) textView
  where
    blue = Draw.Color 0 0 1 0.8

makeChoice ::
  (Monad m) =>
  Anim.AnimId -> Transaction.Property t m Int -> Box.Orientation ->
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
  (Anim.AnimId -> CTransaction t m a) ->
  ((Widget (Transaction t m) -> Widget (Transaction t m)) -> a -> r) ->
  Anim.AnimId -> CTransaction t m r
wrapDelegatedWithKeys keys entryState mkResult atWidget animId = do
  let
    innerAnimId = AnimIds.delegating animId
    selfAnimId = AnimIds.notDelegating animId
    destAnimId =
      case entryState of
        FocusDelegator.NotDelegating -> selfAnimId
        FocusDelegator.Delegating -> innerAnimId
  assignCursor animId destAnimId $ do
    innerResult <- mkResult innerAnimId
    cursor <- readCursor
    let
      cursorSelf = Just FocusDelegator.NotDelegating
      cursorNotSelf innerWidget
        | Widget.isFocused innerWidget = Just FocusDelegator.Delegating
        | otherwise = Nothing
      makeDelegator delegateState =
        (Widget.atIsFocused . const) (isJust delegateState) .
        FocusDelegator.make entryState delegateState selfAnimId keys AnimIds.backgroundCursorId
      onWidget innerWidget =
        (`makeDelegator` innerWidget) . maybe (cursorNotSelf innerWidget) (const cursorSelf) . Anim.subId selfAnimId $ cursor
    return $ atWidget onWidget innerResult

wrapDelegated ::
  Monad m => FocusDelegator.IsDelegating ->
  (Anim.AnimId -> TWidget t m) -> Anim.AnimId -> TWidget t m
wrapDelegated entryState f = wrapDelegatedWithKeys FocusDelegator.defaultKeys entryState f id

makeTextEdit ::
  Monad m =>
  Transaction.Property t m String ->
  Anim.AnimId -> TWidget t m
makeTextEdit textRef animId = do
  text <- getP textRef
  let
    lifter (newText, eventRes) = do
      when (newText /= text) $ Property.set textRef newText
      return eventRes
  cursor <- readCursor
  style <- readTextStyle
  return .
    Widget.atEvents lifter $
    TextEdit.make style cursor text animId

makeWordEdit ::
  Monad m =>
  Transaction.Property t m String ->
  Anim.AnimId -> TWidget t m
makeWordEdit = (fmap . fmap . liftM . Widget.atEventMap) removeWordSeparators makeTextEdit
  where
    compose = foldr (.) id
    removeWordSeparators = compose $ map EventMap.delete [newlineKey, newwordKey]
    newlineKey = EventMap.KeyEventType EventMap.noMods EventMap.KeyEnter
    newwordKey = EventMap.SpaceKeyEventType EventMap.noMods

makeNameEdit :: Monad m => String -> IRef a -> Anim.AnimId -> TWidget t m
makeNameEdit emptyStr iref =
  (atEmptyStr . const) emptyStr . makeWordEdit (Anchors.aNameRef iref)

center :: Vector2 Draw.R
center = Vector2 0.5 0.5

hbox :: [Widget f] -> Widget f
hbox = Box.make Box.horizontal . map (Widget.align center)

vbox :: [Widget f] -> Widget f
vbox = Box.make Box.vertical . map (Widget.align center)
