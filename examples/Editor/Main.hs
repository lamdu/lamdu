{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Main(main) where

import Control.Monad (liftM, unless)
import Data.List (findIndex, elemIndex)
import Data.List.Utils (removeAt)
import Data.Maybe (fromMaybe)
import Data.Monoid(Monoid(..))
import Data.Store.Property(Property)
import Data.Store.Rev.View (View)
import Data.Store.Transaction (Transaction)
import Editor.Anchors (DBTag)
import Editor.CTransaction (runCTransaction, runNestedCTransaction, transaction, getP, TWidget)
import Editor.MonadF (MonadF)
import Graphics.UI.Bottle.MainLoop(mainLoopWidget)
import qualified Data.Store.Db as Db
import qualified Data.Store.Property as Property
import qualified Data.Store.Rev.Branch as Branch
import qualified Data.Store.Rev.Version as Version
import qualified Data.Store.Rev.View as View
import qualified Data.Store.Transaction as Transaction
import qualified Editor.Anchors as Anchors
import qualified Editor.AnimIds as AnimIds
import qualified Editor.BottleWidgets as BWidgets
import qualified Editor.CodeEdit as CodeEdit
import qualified Editor.Config as Config
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Box as Box
import qualified Graphics.UI.Bottle.Widgets.EventMapDoc as EventMapDoc
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Graphics.UI.Bottle.Widgets.Spacer as Spacer
import qualified Graphics.UI.Bottle.Widgets.TextEdit as TextEdit
import qualified Graphics.UI.Bottle.Widgets.TextView as TextView
import qualified System.Info

-- Apply the transactions to the given View and convert them to
-- transactions on a DB
makeWidgetForView :: MonadF m => View -> TWidget DBTag m
makeWidgetForView view = do
  versionData <- transaction $ Version.versionData =<< View.curVersion view
  focusable <-
    runNestedCTransaction store .
    (liftM . Widget.atEvents) (>>= applyAndReturn saveCursor) $
    CodeEdit.makeDefinitionEdit Anchors.rootIRef
  let undoEventMap = maybe mempty makeUndoEventMap (Version.parent versionData)
  return $ Widget.strongerEvents undoEventMap focusable
  where
    makeUndoEventMap = Widget.actionEventMapMovesCursor Config.undoKeys "Undo" . (>> fetchRevisionCursor) . View.move view
    fetchRevisionCursor = Transaction.run store $ Property.get Anchors.cursor
    store = Anchors.viewStore view
    saveCursor eventResult = do
      isEmpty <- Transaction.isEmpty
      unless isEmpty $ maybeUpdateCursor eventResult

maybeUpdateCursor :: Monad m => Widget.EventResult -> Transaction t m ()
maybeUpdateCursor = maybe (return ()) (Property.set Anchors.cursor) . Widget.eCursor

defaultFont :: String -> FilePath
defaultFont "darwin" = "/Library/Fonts/Arial.ttf"
defaultFont _ = "/usr/share/fonts/truetype/freefont/FreeSerifBold.ttf"

main :: IO ()
main = do
  font <- Draw.openFont (defaultFont System.Info.os)
  Db.withDb "/tmp/codeedit.db" $ runDbStore font . Anchors.dbStore

deleteCurrentBranch :: Monad m => Transaction DBTag m ()
deleteCurrentBranch = do
  branch <- Property.get Anchors.currentBranch
  branches <- Property.get Anchors.branches
  let
    index =
      fromMaybe (error "Invalid current branch!") $
      findIndex ((branch ==) . snd) branches
    newBranches = removeAt index branches
  Property.set Anchors.branches newBranches
  Property.set Anchors.currentBranch . snd $
    newBranches !! min (length newBranches - 1) index

makeBranch :: Monad m => View -> Transaction DBTag m ()
makeBranch view = do
  newBranch <- Branch.new =<< View.curVersion view
  textEditModelIRef <- Transaction.newIRef "New view"
  let viewPair = (textEditModelIRef, newBranch)
  Property.pureModify Anchors.branches (++ [viewPair])
  Property.set Anchors.currentBranch newBranch

applyAndReturn :: Monad m => (a -> m ()) -> a -> m a
applyAndReturn f val = f val >> return val

branchSelectorProperty :: Monad m => View -> [Branch.Branch] -> Property (Transaction DBTag m) Int
branchSelectorProperty view branches =
  Property.pureCompose
    (fromMaybe (error "Selected branch not in branch list") .
     (`elemIndex` branches)) (branches !!) .
  Property.compose return (applyAndReturn (View.setBranch view)) $
  Anchors.currentBranch

makeRootWidget :: MonadF m => TWidget DBTag m
makeRootWidget = do
  view <- getP Anchors.view
  namedBranches <- getP Anchors.branches

  viewEdit <- makeWidgetForView view

  let
    branchIndexRef = branchSelectorProperty view $ map snd namedBranches
    makeBranchNameEdit textEditModelIRef =
      BWidgets.wrapDelegated FocusDelegator.NotDelegating
      (BWidgets.makeTextEdit (Transaction.fromIRef textEditModelIRef)) $
      AnimIds.fromIRef textEditModelIRef
  branchSelector <-
    BWidgets.makeChoice AnimIds.branchSelection branchIndexRef
    Box.vertical $ map (makeBranchNameEdit . fst) namedBranches

  let
    delBranchEventMap
      | null (drop 1 namedBranches) = mempty
      | otherwise = Widget.actionEventMap Config.delBranchKeys "Delete Branch" deleteCurrentBranch
  return .
    (Widget.strongerEvents . mconcat)
      [Widget.actionEventMap Config.quitKeys "Quit" (error "Quit")
      ,Widget.actionEventMap Config.makeBranchKeys "New Branch" (makeBranch view)
      ] .
    Box.make Box.horizontal $
    [viewEdit
    ,Widget.liftView Spacer.makeHorizontalExpanding
    ,Widget.strongerEvents delBranchEventMap branchSelector
    ]

runDbStore :: Draw.Font -> Transaction.Store DBTag IO -> IO a
runDbStore font store = do
  Anchors.initDB store
  addHelp <- EventMapDoc.makeToggledHelpAdder Config.overlayDocKeys helpStyle
  mainLoopWidget $ addHelp =<< makeWidget
  where
    helpStyle = TextView.Style {
      TextView.styleFont = font,
      TextView.styleFontSize = 10
      }
    style = TextEdit.Style {
      TextEdit.sTextViewStyle =
        TextView.Style {
          TextView.styleFont = font,
          TextView.styleFontSize = 25
          },
      TextEdit.sCursorColor = TextEdit.defaultCursorColor,
      TextEdit.sCursorWidth = TextEdit.defaultCursorWidth,
      TextEdit.sTextCursorId = AnimIds.textCursorId,
      TextEdit.sBackgroundCursorId = AnimIds.backgroundCursorId,
      TextEdit.sEmptyString = "<empty>"
      }

    fromCursor cursor = runCTransaction cursor style makeRootWidget
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
      maybeUpdateCursor eventResult
      return eventResult
