{-# LANGUAGE OverloadedStrings #-}

module Editor.CodeEdit.ExpressionEdit.LiteralEdit(makeInt, makeIntView) where

import Control.Monad (liftM)
import Data.Store.Transaction (Transaction)
import Editor.Anchors(ViewTag)
import Editor.CodeEdit.ExpressionEdit.ExpressionGui (ExpressionGui)
import Editor.CodeEdit.ExpressionEdit.ExpressionGui.Monad (ExprGuiM)
import Editor.MonadF(MonadF)
import Graphics.UI.Bottle.Animation (AnimId)
import qualified Control.Lens as Lens
import qualified Data.Char as Char
import qualified Editor.BottleWidgets as BWidgets
import qualified Editor.CodeEdit.ExpressionEdit.ExpressionGui as ExpressionGui
import qualified Editor.CodeEdit.ExpressionEdit.ExpressionGui.Monad as ExprGuiM
import qualified Editor.CodeEdit.Sugar as Sugar
import qualified Editor.Config as Config
import qualified Editor.WidgetEnvT as WE
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Graphics.UI.Bottle.Widgets.TextEdit as TextEdit

setColor :: Monad m => ExprGuiM m a -> ExprGuiM m a
setColor = ExprGuiM.atEnv $ WE.setTextColor Config.literalIntColor

makeIntView
  :: Monad m
  => AnimId -> Integer
  -> ExprGuiM m (ExpressionGui m)
makeIntView myId integer =
  liftM ExpressionGui.fromValueWidget .
  setColor . ExprGuiM.widgetEnv $
  BWidgets.makeTextView (show integer) myId

makeIntEdit
  :: Monad m
  => Sugar.LiteralInteger m -> Widget.Id
  -> ExprGuiM m (ExpressionGui m)
makeIntEdit integer myId =
  case Sugar.liSetValue integer of
    Nothing -> makeIntView (Widget.toAnimId myId) (Sugar.liValue integer)
    Just setValue -> makeIntEditI integer myId setValue

makeIntEditI
  :: Monad m
  => Sugar.LiteralInteger m -> Widget.Id
  -> (Integer -> Transaction ViewTag m ())
  -> ExprGuiM m (ExpressionGui m)
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
      | not (all Char.isDigit newText) = return Widget.emptyEventResult
      | null newText = do
        _ <- setValue 0
        return . Widget.eventResultFromCursor $ Widget.joinId myId emptyZeroCursor
      | otherwise = do
        _ <- setValue $ read newText
        return eventRes
  style <- ExprGuiM.widgetEnv WE.readTextStyle
  return .
    ExpressionGui.fromValueWidget .
    Widget.atEvents setter .
    Lens.over Widget.wEventMap removeKeys $ TextEdit.make
    (Lens.set TextEdit.sEmptyFocusedString "<0>" style) textCursor text myId
  where
    removeKeys =
      E.filterChars Char.isDigit .
      foldr (.) id
      [ E.deleteKey (E.KeyEvent E.Press (E.ModKey E.noMods key))
      | key <- [E.KeyEnter, E.KeySpace]
      ]
    emptyZeroCursor = ["empty-zero"]

literalFDConfig :: FocusDelegator.Config
literalFDConfig = FocusDelegator.Config
  { FocusDelegator.startDelegatingKey = E.ModKey E.noMods E.KeyEnter
  , FocusDelegator.startDelegatingDoc = "Change integer"
  , FocusDelegator.stopDelegatingKey = E.ModKey E.noMods E.KeyEsc
  , FocusDelegator.stopDelegatingDoc = "Stop changing integer"
  }

makeInt
  :: MonadF m
  => Sugar.LiteralInteger m
  -> Widget.Id
  -> ExprGuiM m (ExpressionGui m)
makeInt integer =
  ExpressionGui.wrapDelegated literalFDConfig FocusDelegator.NotDelegating
  (setColor . makeIntEdit integer)
