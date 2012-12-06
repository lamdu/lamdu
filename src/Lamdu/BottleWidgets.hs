module Lamdu.BottleWidgets
  ( makeTextView, makeLabel
  , makeFocusableView, makeFocusableTextView
  , wrapDelegatedWith, wrapDelegatedOT
  , makeTextEdit, makeLineEdit, makeWordEdit
  , stdSpaceWidget
  , hboxSpaced, hboxCenteredSpaced
  , gridHSpaced, gridHSpacedCentered
  ) where

import Control.Applicative (Applicative(..))
import Control.Lens ((^.))
import Control.Monad (when)
import Control.MonadA (MonadA)
import Data.ByteString.Char8 (pack)
import Data.List (intersperse)
import Data.Maybe (isJust)
import Data.Monoid (mappend)
import Data.Store.Property (Property)
import Lamdu.WidgetEnvT (WidgetEnvT)
import Graphics.UI.Bottle.Animation (AnimId)
import Graphics.UI.Bottle.Widget (Widget)
import qualified Control.Lens as Lens
import qualified Data.Store.Property as Property
import qualified Lamdu.Config as Config
import qualified Lamdu.Layers as Layers
import qualified Lamdu.WidgetEnvT as WE
import qualified Lamdu.WidgetIds as WidgetIds
import qualified Graphics.UI.Bottle.EventMap as EventMap
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Box as Box
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Graphics.UI.Bottle.Widgets.Grid as Grid
import qualified Graphics.UI.Bottle.Widgets.Spacer as Spacer
import qualified Graphics.UI.Bottle.Widgets.TextEdit as TextEdit
import qualified Graphics.UI.Bottle.Widgets.TextView as TextView

makeTextView :: MonadA m => String -> AnimId -> WidgetEnvT m (Widget f)
makeTextView text myId = do
  style <- WE.readTextStyle
  return $
    TextView.makeWidget (style ^. TextEdit.sTextViewStyle) text myId

makeLabel :: MonadA m => String -> AnimId -> WidgetEnvT m (Widget f)
makeLabel text prefix =
  makeTextView text $ mappend prefix [pack text]

makeFocusableView
  :: (Applicative f, MonadA m)
  => Widget.Id -> Widget f
  -> WidgetEnvT m (Widget f)
makeFocusableView myId widget = do
  hasFocus <- fmap isJust $ WE.subCursor myId
  let
    setBackground
      | hasFocus = Widget.backgroundColor Layers.cursorBG WidgetIds.backgroundCursorId Config.cursorBGColor
      | otherwise = id
  return .
    Lens.set Widget.wIsFocused hasFocus . setBackground $
    Widget.takesFocus (const (pure myId)) widget

makeFocusableTextView
  :: (Applicative f, MonadA m)
  => String -> Widget.Id
  -> WidgetEnvT m (Widget f)
makeFocusableTextView text myId = do
  textView <- makeTextView text $ Widget.toAnimId myId
  makeFocusableView myId textView

fdStyle :: FocusDelegator.Style
fdStyle = FocusDelegator.Style
  { FocusDelegator.color = Config.cursorBGColor
  , FocusDelegator.layer = Layers.cursorBG
  , FocusDelegator.cursorBGAnimId = WidgetIds.backgroundCursorId
  }

wrapDelegatedWith
  :: (Applicative f, MonadA m)
  => m Widget.Id
  -> ((Widget.Id -> Widget.Id) -> m a -> m a)
  -> FocusDelegator.Config
  -> FocusDelegator.IsDelegating
  -> ((Widget f -> Widget f) -> a -> b)
  -> (Widget.Id -> m a)
  -> Widget.Id -> m b
wrapDelegatedWith readCursor atCursor config entryState aToB mkA myId = do
  cursor <- readCursor
  FocusDelegator.wrapEnv (FocusDelegator.Env config fdStyle) entryState mk myId cursor
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
wrapDelegatedOT = wrapDelegatedWith WE.readCursor (WE.atEnv . Lens.over WE.envCursor)

makeTextEdit
  :: (MonadA m, MonadA f)
  => Property f String
  -> Widget.Id
  -> WidgetEnvT m (Widget f)
makeTextEdit textRef myId = do
  cursor <- WE.readCursor
  style <- WE.readTextStyle
  return .
    Widget.atEvents setter $
    TextEdit.make style cursor (Property.value textRef) myId
  where
    setter (newText, eventRes) = do
      when (newText /= Property.value textRef) $ Property.set textRef newText
      return eventRes

removeKey
  :: (MonadA m)
  => (a -> b -> m (Widget f))
  -> EventMap.ModKey
  -> a -> b -> m (Widget f)
removeKey makeEdit key =
  (fmap . fmap . fmap . Lens.over Widget.wEventMap)
  (EventMap.deleteKey (EventMap.KeyEvent EventMap.Press key))
  makeEdit

makeLineEdit ::
  (MonadA m, MonadA f) =>
  Property f String ->
  Widget.Id ->
  WidgetEnvT m (Widget f)
makeLineEdit =
  removeKey makeTextEdit $
  EventMap.ModKey EventMap.noMods EventMap.KeyEnter

makeWordEdit ::
  (MonadA m, MonadA f) =>
  Property f String ->
  Widget.Id ->
  WidgetEnvT m (Widget f)
makeWordEdit =
  removeKey makeLineEdit $
  EventMap.ModKey EventMap.noMods EventMap.KeySpace

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
