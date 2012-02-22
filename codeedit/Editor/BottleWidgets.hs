{-# OPTIONS -Wall #-}
module Editor.BottleWidgets(
  makeTextView, makeLabel, makeChoice,
  makeFocusableView, makeFocusableTextView,
  wrapDelegatedWithKeys, wrapDelegated,
  makeTextEdit, makeWordEdit, makeNameEdit,
  hbox,  hboxAlign,  hboxSpaced,
  hboxK, hboxAlignK, hboxSpacedK,
  vbox,  vboxAlign,
  vboxK, vboxAlignK,
  spaceView, spaceWidget,
  setTextColor
) where

import Control.Arrow (second)
import Control.Monad (when, liftM)
import Data.ByteString.Char8 (pack)
import Data.List (intersperse)
import Data.List.Utils (enumerate, nth)
import Data.Maybe (isJust)
import Data.Store.IRef (IRef)
import Data.Store.Transaction (Transaction)
import Data.Vector.Vector2 (Vector2(..))
import Editor.CTransaction (TWidget, CTransaction, readTextStyle, readCursor, getP, assignCursor, atTextStyle)
import Editor.MonadF (MonadF)
import Graphics.UI.Bottle.Sized (Sized)
import Graphics.UI.Bottle.Widget (Widget)
import Graphics.UI.Bottle.Widgets.Box(KBox)
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Editor.Anchors as Anchors
import qualified Editor.WidgetIds as WidgetIds
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.EventMap as EventMap
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Box as Box
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Graphics.UI.Bottle.Widgets.Spacer as Spacer
import qualified Graphics.UI.Bottle.Widgets.TextEdit as TextEdit
import qualified Graphics.UI.Bottle.Widgets.TextView as TextView

makeTextView :: MonadF m => String -> Widget.Id -> TWidget t m
makeTextView text myId = do
  style <- readTextStyle
  return $
    TextView.makeWidget (TextEdit.sTextViewStyle style) text $ Widget.toAnimId myId

makeLabel :: MonadF m => String -> Widget.Id -> TWidget t m
makeLabel text prefix =
  makeTextView text $ Widget.joinId prefix [pack text]

makeFocusableView :: MonadF m => Widget.Id -> Widget (Transaction t m) -> TWidget t m
makeFocusableView myId widget = do
  hasFocus <- liftM (myId ==) readCursor
  let
    setBackground
      | hasFocus = Widget.backgroundColor WidgetIds.backgroundCursorId blue
      | otherwise = id
  return .
    (Widget.atIsFocused . const) hasFocus . setBackground $
    Widget.takesFocus (const (return myId)) widget
  where
    blue = Draw.Color 0 0 1 0.8

makeFocusableTextView :: MonadF m => String -> Widget.Id -> TWidget t m
makeFocusableTextView text myId = do
  textView <- makeTextView text myId
  makeFocusableView myId textView

makeChoice ::
  (Monad m) =>
  Widget.Id -> Transaction.Property t m Int -> Box.Orientation ->
  [TWidget t m] -> TWidget t m
makeChoice selectionAnimId curChoiceRef orientation children = do
  curChoice <- getP curChoiceRef
  focusables <- sequence children
  let
    widget =
      Box.toWidgetBiased curChoice .
      Box.make orientation .
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
    newwordKey = EventMap.KeyEventType EventMap.noMods EventMap.KeySpace

makeNameEdit :: Monad m => String -> IRef a -> Widget.Id -> TWidget t m
makeNameEdit emptyStr iref =
  (atTextStyle . TextEdit.atSEmptyString . const) emptyStr . makeWordEdit (Anchors.aNameRef iref)

boxAlignK :: Vector2 Widget.R -> Box.Orientation -> [(key, Widget f)] -> KBox key f
boxAlignK align orientation =
  Box.makeKeyed orientation .
  (map . second) (Widget.align align)

hboxAlignK :: Widget.R -> [(key, Widget f)] -> KBox key f
hboxAlignK align = boxAlignK (Vector2 0 align) Box.horizontal

vboxAlignK :: Widget.R -> [(key, Widget f)] -> KBox key f
vboxAlignK align = boxAlignK (Vector2 align 0) Box.vertical

hboxK :: [(key, Widget f)] -> KBox key f
hboxK = hboxAlignK 0.5

vboxK :: [(key, Widget f)] -> KBox key f
vboxK = vboxAlignK 0.5

unK
  :: ([((), Widget f1)]
  -> KBox key f)
  -> [Widget f1] -> Widget f
unK f = Box.toWidget . f . Box.unkey

hboxAlign
  :: Widget.R -> [Widget f] -> Widget f
hboxAlign = unK . hboxAlignK

vboxAlign
  :: Widget.R -> [Widget f] -> Widget f
vboxAlign = unK . vboxAlignK

hbox :: [Widget f] -> Widget f
hbox = unK hboxK

vbox :: [Widget f] -> Widget f
vbox = unK vboxK

hboxSpacedK :: key -> [(key, Widget f)] -> KBox key f
hboxSpacedK spaceKey = hboxK . intersperse (spaceKey, spaceWidget)

hboxSpaced :: [Widget f] -> Widget f
hboxSpaced = unK (hboxSpacedK ())

spaceView :: Sized Anim.Frame
spaceView = Spacer.makeHorizontal 20

spaceWidget :: Widget f
spaceWidget = Widget.liftView spaceView

setTextColor :: Draw.Color -> TWidget t m -> TWidget t m
setTextColor = atTextStyle . TextEdit.atSTextViewStyle . TextView.atStyleColor . const
