{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Main(main) where

import Control.Monad (liftM, unless)
import Editor.Anchors (DBTag)
import Editor.CTransaction (runCTransaction)
import Graphics.UI.Bottle.MainLoop(mainLoopWidget)
import qualified Data.Store.Db as Db
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Editor.Anchors as Anchors
import qualified Editor.AnimIds as AnimIds
import qualified Editor.BranchGUI as BranchGUI
import qualified Editor.CodeEdit as CodeEdit
import qualified Editor.Config as Config
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.EventMapDoc as EventMapDoc
import qualified Graphics.UI.Bottle.Widgets.TextEdit as TextEdit
import qualified Graphics.UI.Bottle.Widgets.TextView as TextView
import qualified System.Info

defaultFont :: String -> FilePath
defaultFont "darwin" = "/Library/Fonts/Arial.ttf"
defaultFont _ = "/usr/share/fonts/truetype/freefont/FreeSerifBold.ttf"

main :: IO ()
main = do
  font <- Draw.openFont (defaultFont System.Info.os)
  Db.withDb "/tmp/codeedit.db" $ runDbStore font . Anchors.dbStore

runDbStore :: Draw.Font -> Transaction.Store DBTag IO -> IO a
runDbStore font store = do
  Anchors.initDB store
  addHelp <- EventMapDoc.makeToggledHelpAdder Config.overlayDocKeys helpStyle
  mainLoopWidget $ addHelp =<< makeWidget
  where
    helpStyle = TextView.Style {
      TextView.styleColor = Draw.Color 1 1 1 1,
      TextView.styleFont = font,
      TextView.styleFontSize = 10
      }
    style = TextEdit.Style {
      TextEdit.sTextViewStyle =
        TextView.Style {
          TextView.styleColor = Draw.Color 1 1 1 1,
          TextView.styleFont = font,
          TextView.styleFontSize = 25
          },
      TextEdit.sCursorColor = TextEdit.defaultCursorColor,
      TextEdit.sCursorWidth = TextEdit.defaultCursorWidth,
      TextEdit.sTextCursorId = AnimIds.textCursorId,
      TextEdit.sBackgroundCursorId = AnimIds.backgroundCursorId,
      TextEdit.sEmptyString = "<empty>"
      }

    fromCursor cursor =
      runCTransaction cursor style $
      BranchGUI.makeRootWidget CodeEdit.makePanesEdit
    -- TODO: Move this logic to some more common place?
    makeWidget = widgetDownTransaction $ do
      cursor <- Property.get Anchors.cursor
      candidateWidget <- fromCursor cursor
      focusable <-
        if Widget.isFocused candidateWidget
        then return candidateWidget
        else fromCursor (AnimIds.fromIRef Anchors.rootIRef)
      unless (Widget.isFocused focusable) $
        fail "Root cursor did not match"
      return $ Widget.atEvents (>>= attachCursor) focusable

    widgetDownTransaction =
      Transaction.run store .
      (liftM . Widget.atEvents) (Transaction.run store)

    attachCursor eventResult = do
      maybe (return ()) (Property.set Anchors.cursor) $ Widget.eCursor eventResult
      return eventResult
