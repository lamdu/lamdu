{-# LANGUAGE OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.LiteralEdit
  ( makeInt
  ) where

import Control.Lens.Operators
import Control.MonadA (MonadA)
import Data.Monoid (Monoid(..))
import Data.Store.Transaction (Transaction)
import Graphics.UI.Bottle.Animation (AnimId)
import Lamdu.GUI.ExpressionGui (ExpressionGui)
import Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Data.Char as Char
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Graphics.UI.Bottle.Widgets.TextEdit as TextEdit
import qualified Lamdu.Config as Config
import qualified Lamdu.GUI.BottleWidgets as BWidgets
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.WidgetEnvT as WE
import qualified Lamdu.Sugar.Types as Sugar

setColor :: MonadA m => ExprGuiM m a -> ExprGuiM m a
setColor action = do
  config <- ExprGuiM.widgetEnv WE.readConfig
  ExprGuiM.withFgColor (Config.literalIntColor config) action

makeIntView ::
  MonadA m =>
  AnimId -> Integer ->
  ExprGuiM m (ExpressionGui m)
makeIntView myId integer =
  fmap ExpressionGui.fromValueWidget .
  setColor . ExprGuiM.widgetEnv $
  BWidgets.makeTextViewWidget (show integer) myId

makeIntEdit ::
  MonadA m =>
  Sugar.LiteralInteger m -> Widget.Id ->
  ExprGuiM m (ExpressionGui m)
makeIntEdit integer myId =
  case Sugar.liSetValue integer of
    Nothing -> makeIntView (Widget.toAnimId myId) (Sugar.liValue integer)
    Just setValue -> makeIntEditI integer myId setValue

makeIntEditI ::
  MonadA m =>
  Sugar.LiteralInteger m -> Widget.Id ->
  (Integer -> Transaction m ()) ->
  ExprGuiM m (ExpressionGui m)
makeIntEditI integer myId setValue = do
  cursor <- ExprGuiM.widgetEnv WE.readCursor
  let
    suffix = Widget.subId myId cursor
    isEmpty = Sugar.liValue integer == 0 && suffix == Just emptyZeroCursor
    (text, textCursor)
      | isEmpty = ("", TextEdit.makeTextEditCursor myId 0)
      | otherwise = (show (Sugar.liValue integer), cursor)
    setter (newText, eventRes)
      | newText == text = return eventRes
      | not (all Char.isDigit newText) = return mempty
      | null newText = do
        _ <- setValue 0
        return . Widget.eventResultFromCursor $ Widget.joinId myId emptyZeroCursor
      | otherwise = do
        _ <- setValue $ read newText
        return eventRes
  style <- ExprGuiM.widgetEnv WE.readTextStyle
  return $
    TextEdit.make
    (style & TextEdit.sEmptyFocusedString .~ "<0>")
    textCursor text myId
    & Widget.wEventMap %~ removeKeys
    & Widget.atEvents setter
    & ExpressionGui.fromValueWidget
  where
    removeKeys =
      (E.filterSChars . flip . const) Char.isDigit .
      foldr (.) id
      [ E.deleteKey (E.KeyEvent E.Press (E.ModKey E.noMods key))
      | key <- [E.KeyEnter, E.KeySpace]
      ]
    emptyZeroCursor = ["empty-zero"]

literalFDConfig :: FocusDelegator.Config
literalFDConfig = FocusDelegator.Config
  { FocusDelegator.startDelegatingKeys = [E.ModKey E.noMods E.KeyEnter]
  , FocusDelegator.startDelegatingDoc = E.Doc ["Edit", "Change integer"]
  , FocusDelegator.stopDelegatingKeys = [E.ModKey E.noMods E.KeyEsc]
  , FocusDelegator.stopDelegatingDoc = E.Doc ["Edit", "Stop changing integer"]
  }

makeInt ::
  MonadA m =>
  Sugar.LiteralInteger m ->
  Sugar.Payload Sugar.Name m a ->
  Widget.Id ->
  ExprGuiM m (ExpressionGui m)
makeInt integer pl =
  ExpressionGui.wrapDelegated pl literalFDConfig FocusDelegator.NotDelegating
  (setColor . makeIntEdit integer)
