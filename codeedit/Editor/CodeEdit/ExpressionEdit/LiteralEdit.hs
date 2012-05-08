{-# LANGUAGE OverloadedStrings #-}

module Editor.CodeEdit.ExpressionEdit.LiteralEdit(makeInt, makeIntView) where

import Data.Store.Transaction (Transaction)
import Editor.Anchors(ViewTag)
import Editor.CTransaction (CTransaction, TWidget, readCursor, readTextStyle)
import Editor.MonadF(MonadF)
import Graphics.UI.Bottle.Widget (Widget)
import qualified Data.Char as Char
import qualified Editor.BottleWidgets as BWidgets
import qualified Editor.Config as Config
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Graphics.UI.Bottle.Widgets.TextEdit as TextEdit

setColor :: TWidget t m -> TWidget t m
setColor = BWidgets.setTextColor Config.literalIntColor

makeIntView
  :: (MonadF m)
  => Widget.Id -> Integer
  -> CTransaction ViewTag m (Widget (Transaction ViewTag m))
makeIntView myId integer =
  setColor $ BWidgets.makeTextView (show integer) myId

makeIntEdit
  :: Monad m
  => Integer -> Widget.Id -> TWidget t m
makeIntEdit integer myId = do
  cursor <- readCursor
  let
    subCursor = Widget.subId myId cursor
    isEmpty = integer == 0 && subCursor == Just emptyZeroCursor
    (text, textCursor)
      | isEmpty = ("", TextEdit.makeTextEditCursor myId 0)
      | otherwise = (show integer, cursor)
    lifter (newText, eventRes)
      | newText == text = return eventRes
      | not (all Char.isDigit newText) = return Widget.emptyEventResult
      | null newText = do
        _ <- error "TODO: Set to 0"
        return . Widget.eventResultFromCursor $ Widget.joinId myId emptyZeroCursor
      | otherwise = do
        _ <- error "TODO: Set to (read newText)"
        return eventRes
  style <- readTextStyle
  return .
    Widget.atEvents lifter $
    TextEdit.make style { TextEdit.sEmptyString = "<0>" } textCursor text myId
  where
    emptyZeroCursor = ["empty-zero"]

makeInt
  :: Monad m
  => Integer
  -> Widget.Id
  -> TWidget ViewTag m
makeInt integer =
  BWidgets.wrapDelegatedWithKeys FocusDelegator.defaultKeys FocusDelegator.NotDelegating id
  (setColor . makeIntEdit integer)
