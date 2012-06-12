module Editor.BottleWidgets(
  makeTextView, makeLabel, makeChoice,
  makeFocusableView, makeFocusableTextView,
  wrapDelegatedWithConfig, wrapDelegated,
  makeTextEdit, makeWordEdit, makeNameEdit, getDisplayNameOf,
  hbox,  hboxAlign,  hboxSpaced,
  hboxK, hboxAlignK, hboxSpacedK,
  vbox,  vboxAlign,
  vboxK, vboxAlignK,
  gridHSpaced,
  spaceView, spaceWidget,
  setTextColor,
  empty
) where

import Control.Applicative (Applicative(..))
import Control.Arrow (first, second)
import Control.Monad (when, liftM)
import Data.ByteString.Char8 (pack)
import Data.List (findIndex, intersperse)
import Data.Monoid (mappend)
import Data.Store.Guid (Guid)
import Data.Store.Transaction (Transaction)
import Data.Vector.Vector2 (Vector2(..))
import Editor.CTransaction (TWidget, CTransaction, readTextStyle, readCursor, getP, atTextStyle, atCursor)
import Editor.MonadF (MonadF)
import Graphics.UI.Bottle.Animation (AnimId)
import Graphics.UI.Bottle.Sized (Sized)
import Graphics.UI.Bottle.Widget (Widget)
import Graphics.UI.Bottle.Widgets.Box(KBox)
import qualified Data.Store.Guid as Guid
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
import qualified Graphics.UI.Bottle.Widgets.Grid as Grid
import qualified Graphics.UI.Bottle.Widgets.Spacer as Spacer
import qualified Graphics.UI.Bottle.Widgets.TextEdit as TextEdit
import qualified Graphics.UI.Bottle.Widgets.TextView as TextView

makeTextView :: MonadF m => String -> Anim.AnimId -> CTransaction t m (Widget f)
makeTextView text myId = do
  style <- readTextStyle
  return $
    TextView.makeWidget (TextEdit.sTextViewStyle style) text myId

makeLabel :: MonadF m => String -> Anim.AnimId -> CTransaction t m (Widget f)
makeLabel text prefix =
  makeTextView text $ mappend prefix [pack text]

makeFocusableView
  :: (Applicative f, MonadF m)
  => Widget.Id -> Widget f
  -> CTransaction t m (Widget f)
makeFocusableView myId widget = do
  hasFocus <- liftM (myId ==) readCursor
  let
    setBackground
      | hasFocus = Widget.backgroundColor 10 WidgetIds.backgroundCursorId blue
      | otherwise = id
  return .
    (Widget.atIsFocused . const) hasFocus . setBackground $
    Widget.takesFocus (const (pure myId)) widget
  where
    blue = Draw.Color 0 0 1 0.8

makeFocusableTextView
  :: (Applicative f, MonadF m)
  => String -> Widget.Id
  -> CTransaction t m (Widget f)
makeFocusableTextView text myId = do
  textView <- makeTextView text $ Widget.toAnimId myId
  makeFocusableView myId textView

makeChoice
  :: Eq a
  => AnimId
  -> Box.Orientation
  -> [(a, Widget f)]
  -> a -> Widget f
makeChoice selectionAnimId orientation children curChild =
  maybe Box.toWidget Box.toWidgetBiased mCurChildIndex box
  where
    pairs = (map . first) (curChild ==) children
    mCurChildIndex = findIndex fst pairs
    box = Box.make orientation colorizedPairs
    colorizedPairs
      | any (Widget.isFocused . snd) pairs = map snd pairs
      | otherwise = map (uncurry colorize) pairs
      where
        colorize True = Widget.backgroundColor 9 selectionAnimId selectedColor
        colorize False = id
        selectedColor = Draw.Color 0 0.5 0 1

-- TODO: This logic belongs in the FocusDelegator itself
wrapDelegatedWithConfig
  :: (Applicative f, Monad m)
  => FocusDelegator.Config
  -> FocusDelegator.IsDelegating
  -> ((Widget f -> Widget f) -> a -> b)
  -> (Widget.Id -> CTransaction t m a)
  -> Widget.Id -> CTransaction t m b
wrapDelegatedWithConfig config entryState aToB mkA myId = do
  cursor <- readCursor
  FocusDelegator.wrapConfig config entryState mk
    WidgetIds.backgroundCursorId myId cursor
  where
    mk f innerId newCursor =
      liftM (aToB f) . (atCursor . const) newCursor $ mkA innerId

wrapDelegated
  :: (Monad m, Applicative f)
  => FocusDelegator.IsDelegating
  -> (Widget.Id -> CTransaction t m (Widget f))
  -> Widget.Id -> CTransaction t m (Widget f)
wrapDelegated entryState =
  wrapDelegatedWithConfig FocusDelegator.defaultConfig entryState id

makeTextEdit
  :: Monad m
  => Transaction.Property t m String
  -> Widget.Id -> TWidget t m
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

anonName :: Guid -> String
anonName guid = "<" ++ take 4 (Guid.asHex guid) ++ "..>"

getDisplayNameOf
  :: Monad m
  => Guid -> Transaction t m String
getDisplayNameOf guid = do
  name <- Property.get $ Anchors.aNameRef guid
  return $ if null name then anonName guid else name

makeNameEdit
  :: Monad m => String -> Guid -> Widget.Id -> TWidget t m
makeNameEdit editingEmptyStr ident =
  (atTextStyle . TextEdit.atSEmptyUnfocusedString . const)
    (anonName ident) .
  (atTextStyle . TextEdit.atSEmptyFocusedString . const)
    editingEmptyStr .
  makeWordEdit (Anchors.aNameRef ident)

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

setTextColor :: Draw.Color -> CTransaction t m (Widget f) -> CTransaction t m (Widget f)
setTextColor = atTextStyle . TextEdit.atSTextViewStyle . TextView.atStyleColor . const

gridHSpaced :: [[Widget f]] -> Widget f
gridHSpaced = Grid.toWidget . Grid.make . map (intersperse spaceWidget)

empty :: Widget f
empty = Spacer.makeWidget 0
