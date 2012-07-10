module Editor.BottleWidgets
  ( makeTextView, makeLabel
  , makeFocusableView, makeFocusableTextView
  , wrapDelegated
  , makeTextEdit, makeLineEdit, makeWordEdit, makeNameEdit, getDisplayNameOf
  , hboxAlign, vboxAlign
  , hboxCenteredSpaced
  , hboxCentered, vboxCentered
  , hbox, vbox
  , gridHSpaced, gridHSpacedCentered
  , spaceView, spaceWidget
  , setTextColor
  , empty
  ) where

import Control.Applicative (Applicative(..))
import Control.Monad (when, liftM)
import Data.ByteString.Char8 (pack)
import Data.List (intersperse)
import Data.Monoid (mappend)
import Data.Store.Guid (Guid)
import Data.Store.Transaction (Transaction)
import Editor.MonadF (MonadF)
import Editor.OTransaction (TWidget, OTransaction)
import Graphics.UI.Bottle.Animation (AnimId)
import Graphics.UI.Bottle.Widget (Widget)
import qualified Data.Store.Guid as Guid
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Editor.Anchors as Anchors
import qualified Editor.ITransaction as IT
import qualified Editor.OTransaction as OT
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

makeTextView :: Monad m => String -> AnimId -> OTransaction t m (Widget f)
makeTextView text myId = do
  style <- OT.readTextStyle
  return $
    TextView.makeWidget (TextEdit.sTextViewStyle style) text myId

makeLabel :: MonadF m => String -> AnimId -> OTransaction t m (Widget f)
makeLabel text prefix =
  makeTextView text $ mappend prefix [pack text]

makeFocusableView
  :: (Applicative f, MonadF m)
  => Widget.Id -> Widget f
  -> OTransaction t m (Widget f)
makeFocusableView myId widget = do
  hasFocus <- liftM (myId ==) OT.readCursor
  let
    setBackground
      | hasFocus = Widget.backgroundColor 10 WidgetIds.backgroundCursorId blue
      | otherwise = id
  return .
    (Widget.atWIsFocused . const) hasFocus . setBackground $
    Widget.takesFocus (const (pure myId)) widget
  where
    blue = Draw.Color 0 0 1 0.8

makeFocusableTextView
  :: (Applicative f, MonadF m)
  => String -> Widget.Id
  -> OTransaction t m (Widget f)
makeFocusableTextView text myId = do
  textView <- makeTextView text $ Widget.toAnimId myId
  makeFocusableView myId textView

-- TODO: This logic belongs in the FocusDelegator itself
wrapDelegated
  :: (Applicative f, Monad m)
  => FocusDelegator.Config
  -> FocusDelegator.IsDelegating
  -> ((Widget f -> Widget f) -> a -> b)
  -> (Widget.Id -> OTransaction t m a)
  -> Widget.Id -> OTransaction t m b
wrapDelegated config entryState aToB mkA myId = do
  cursor <- OT.readCursor
  FocusDelegator.wrapConfig config entryState mk
    WidgetIds.backgroundCursorId myId cursor
  where
    mk f innerId newCursor =
      liftM (aToB f) . (OT.atCursor . const) newCursor $ mkA innerId

makeTextEdit
  :: Monad m
  => Transaction.Property t m String
  -> Widget.Id -> TWidget t m
makeTextEdit textRef myId = do
  cursor <- OT.readCursor
  style <- OT.readTextStyle
  return .
    Widget.atEvents setter $
    TextEdit.make style cursor (Property.value textRef) myId
  where
    setter (newText, eventRes) = IT.transaction $ do
      when (newText /= Property.value textRef) $ Property.set textRef newText
      return eventRes

removeKeys
  :: (Monad m)
  => (a -> b -> m (Widget f))
  -> EventMap.ModKey
  -> a -> b -> m (Widget f)
removeKeys makeEdit key =
  (fmap . fmap . liftM . Widget.atWEventMap)
  (EventMap.deleteKey (EventMap.KeyEvent EventMap.Press key))
  makeEdit

makeLineEdit ::
  Monad m =>
  Transaction.Property t m String ->
  Widget.Id -> TWidget t m
makeLineEdit =
  removeKeys makeTextEdit $
  EventMap.ModKey EventMap.noMods EventMap.KeyEnter

makeWordEdit ::
  Monad m =>
  Transaction.Property t m String ->
  Widget.Id -> TWidget t m
makeWordEdit =
  removeKeys makeLineEdit $
  EventMap.ModKey EventMap.noMods EventMap.KeySpace

anonName :: Guid -> String
anonName guid = "<" ++ take 4 (Guid.asHex guid) ++ "..>"

getDisplayNameOf
  :: Monad m
  => Guid -> Transaction t m String
getDisplayNameOf guid = do
  name <- Anchors.getP $ Anchors.assocNameRef guid
  return $ if null name then anonName guid else name

makeNameEdit
  :: Monad m => String -> Guid -> Widget.Id -> TWidget t m
makeNameEdit editingEmptyStr ident myId =
  (OT.atTextStyle . TextEdit.atSEmptyUnfocusedString . const)
    (anonName ident) .
  (OT.atTextStyle . TextEdit.atSEmptyFocusedString . const)
    editingEmptyStr $
    OT.transaction (Anchors.assocNameRef ident) >>=
    flip makeEditor myId
  where
    makeEditor =
      (fmap . fmap . liftM . Widget.atWEventMap)
      (EventMap.filterChars (`notElem` "[]\\"))
      makeWordEdit

boxAlign :: Box.Orientation -> Box.Alignment -> [Widget f] -> Widget f
boxAlign orientation align =
  Box.toWidget .
  Box.makeAlign align orientation

hboxAlign :: Box.Alignment -> [Widget f] -> Widget f
hboxAlign align = boxAlign Box.horizontal align

vboxAlign :: Box.Alignment -> [Widget f] -> Widget f
vboxAlign align = boxAlign Box.vertical align

vboxCentered :: [Widget f] -> Widget f
vboxCentered = vboxAlign 0.5

hboxCentered :: [Widget f] -> Widget f
hboxCentered = hboxAlign 0.5

hbox :: [(Box.Alignment, Widget f)] -> Widget f
hbox = Box.toWidget . Box.make Box.horizontal

vbox :: [(Box.Alignment, Widget f)] -> Widget f
vbox = Box.toWidget . Box.make Box.vertical

spaceWidget :: Widget f
spaceWidget = uncurry Widget.liftView spaceView

hboxCenteredSpaced :: [Widget f] -> Widget f
hboxCenteredSpaced = hboxAlign 0.5 . intersperse spaceWidget

spaceView :: (Anim.Size, Anim.Frame)
spaceView = Spacer.makeHorizontal 20

setTextColor :: Draw.Color -> OTransaction t m a -> OTransaction t m a
setTextColor = OT.atTextStyle . TextEdit.atSTextViewStyle . TextView.atStyleColor . const

gridHSpaced :: [[(Grid.Alignment, Widget f)]] -> Widget f
gridHSpaced = Grid.toWidget . Grid.make . map (intersperse (0, spaceWidget))

gridHSpacedCentered :: [[Widget f]] -> Widget f
gridHSpacedCentered = gridHSpaced . (map . map) ((,) 0.5)

empty :: Widget f
empty = Spacer.makeWidget 0
