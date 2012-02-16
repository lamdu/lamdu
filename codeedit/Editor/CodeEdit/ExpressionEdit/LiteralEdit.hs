{-# LANGUAGE OverloadedStrings #-}

module Editor.CodeEdit.ExpressionEdit.LiteralEdit(makeInt, makeIntView) where

import Control.Monad (liftM)
import Data.Store.IRef (IRef)
import Data.Store.Transaction (Transaction)
import Editor.Anchors(ViewTag)
import Editor.CTransaction (CTransaction, TWidget, getP, readCursor, readTextStyle)
import Editor.MonadF(MonadF)
import Graphics.UI.Bottle.Widget (Widget)
import qualified Data.Char as Char
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Editor.BottleWidgets as BWidgets
import qualified Editor.Config as Config
import qualified Editor.Data as Data
import qualified Editor.WidgetIds as WidgetIds
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.TextEdit as TextEdit

setColor :: TWidget t m -> TWidget t m
setColor = BWidgets.setTextColor Config.literalIntColor

makeIntView ::
  (MonadF m) => Widget.Id -> Integer ->
  CTransaction ViewTag m (Widget (Transaction ViewTag m))
makeIntView myId integer =
  setColor $ BWidgets.makeTextView (show integer) myId

makeIntEdit ::
  Monad m =>
  Transaction.Property t m Integer ->
  Widget.Id -> TWidget t m
makeIntEdit intRef myId = do
  int <- getP intRef
  cursor <- readCursor
  let
    subCursor = Widget.subId myId cursor
    isEmpty = int == 0 && subCursor == Just emptyZeroCursor
    (text, textCursor)
      | isEmpty = ("", TextEdit.makeTextEditCursor myId 0)
      | otherwise = (show int, cursor)
    lifter (newText, eventRes)
      | newText == text = return eventRes
      | not (all Char.isDigit newText) = return Widget.emptyEventResult
      | null newText = do
        Property.set intRef 0
        return . Widget.eventResultFromCursor $ Widget.joinId myId emptyZeroCursor
      | otherwise = do
        Property.set intRef (read newText)
        return eventRes
  style <- readTextStyle
  return .
    Widget.atEvents lifter $
    TextEdit.make style { TextEdit.sEmptyString = "<0>" } textCursor text myId
  where
    emptyZeroCursor = ["empty-zero"]

makeInt ::
  (Monad m) =>
  IRef Data.Expression
  -> Integer
  -> Widget.Id
  -> CTransaction ViewTag m (Widget (Transaction ViewTag m), Widget.Id)
makeInt expressionI integer myId =
  liftM (flip (,) (WidgetIds.fromIRef expressionI)) . setColor $
    makeIntEdit intRef myId
  where
    expressionRef = Transaction.fromIRef expressionI
    intRef = Property.pureCompose (const integer) Data.ExpressionLiteralInteger expressionRef
