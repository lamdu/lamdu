{-# LANGUAGE OverloadedStrings #-}

module Editor.CodeEdit.ExpressionEdit.LiteralEdit(makeInt, makeIntView) where

import Editor.Anchors(ViewTag)
import Editor.MonadF(MonadF)
import Editor.OTransaction (TWidget)
import Graphics.UI.Bottle.Animation (AnimId)
import qualified Data.Char as Char
import qualified Editor.BottleWidgets as BWidgets
import qualified Editor.CodeEdit.Sugar as Sugar
import qualified Editor.Config as Config
import qualified Editor.ITransaction as IT
import qualified Editor.OTransaction as OT
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Graphics.UI.Bottle.Widgets.TextEdit as TextEdit

setColor :: TWidget t m -> TWidget t m
setColor = BWidgets.setTextColor Config.literalIntColor

makeIntView
  :: MonadF m
  => AnimId -> Integer
  -> TWidget t m
makeIntView myId integer =
  setColor $ BWidgets.makeTextView (show integer) myId

makeIntEdit
  :: Monad m
  => Sugar.LiteralInteger m -> Widget.Id -> TWidget ViewTag m
makeIntEdit integer myId = do
  cursor <- OT.readCursor
  let
    subCursor = Widget.subId myId cursor
    isEmpty = Sugar.liValue integer == 0 && subCursor == Just emptyZeroCursor
    (text, textCursor)
      | isEmpty = ("", TextEdit.makeTextEditCursor myId 0)
      | otherwise = (show (Sugar.liValue integer), cursor)
    lifter Nothing (_newText, eventRes) = return eventRes
    lifter (Just setValue) (newText, eventRes)
      | newText == text = return eventRes
      | not (all Char.isDigit newText) = return Widget.emptyEventResult
      | null newText = do
        _ <- IT.transaction $ setValue 0
        return . Widget.eventResultFromCursor $ Widget.joinId myId emptyZeroCursor
      | otherwise = do
        _ <- IT.transaction $ setValue $ read newText
        return eventRes
  style <- OT.readTextStyle
  return .
    Widget.atEvents (lifter (Sugar.liSetValue integer)) $ TextEdit.make
    style { TextEdit.sEmptyFocusedString = "<0>" } textCursor text myId
  where
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
  -> TWidget ViewTag m
makeInt integer =
  BWidgets.wrapDelegated literalFDConfig FocusDelegator.NotDelegating id
  (setColor . makeIntEdit integer)
