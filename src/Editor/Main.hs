{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE TypeOperators, Rank2Types, OverloadedStrings #-}

module Main(main) where

import Control.Category ((.))
import Control.Monad (when, liftM)
import Data.List.Utils (index)
import Data.Maybe.Utils (unsafeUnjust)
import Data.Monoid(Monoid(..))
import Data.Store.BottleWidgets(MWidget, makeTextEdit, widgetDownTransaction, makeBox, appendBoxChild, popCurChild, makeChoiceWidget)
import Data.Store.Guid (bs)
import Data.Store.IRef (IRef, guid)
import Data.Store.Property (composeLabel)
import Data.Store.Rev.View (View)
import Data.Store.Transaction (Transaction)
import Editor.Anchors (DBTag, ViewTag)
import Editor.Data (ITreeD)
import Graphics.UI.Bottle.MainLoop(mainLoopWidget)
import Graphics.UI.Bottle.Widget (Widget)
import Prelude hiding ((.))
import qualified Data.Store.Db as Db
import qualified Data.Store.Property as Property
import qualified Data.Store.Rev.Branch as Branch
import qualified Data.Store.Rev.Version as Version
import qualified Data.Store.Rev.View as View
import qualified Data.Store.Transaction as Transaction
import qualified Editor.Anchors as Anchors
import qualified Editor.Config as Config
import qualified Editor.Data as Data
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.AnimIds as AnimIds
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Box as Box
import qualified Graphics.UI.Bottle.Widgets.Spacer as Spacer
import qualified Graphics.UI.Bottle.Widgets.TextEdit as TextEdit
import qualified Graphics.UI.Bottle.Widgets.TextView as TextView
import qualified System.Info

focusableTextView :: TextView.Style -> [String] -> Anim.AnimId -> Widget a
focusableTextView style textLines animId =
  (Widget.whenFocused . Widget.atImageWithSize . Anim.backgroundColor AnimIds.backgroundCursorId 10) blue .
  Widget.takesFocus $ TextView.makeWidget style textLines animId
  where
    blue = Draw.Color 0 0 1 0.8


removeAt :: Int -> [a] -> [a]
removeAt n xs = take n xs ++ drop (n+1) xs

indentSize :: Num a => a
indentSize = 80

makeChildBox ::
  Monad m =>
  TextEdit.Style ->
  Int ->
  Transaction.Property ViewTag m [ITreeD] ->
  Transaction.Property ViewTag m Box.Cursor ->
  Transaction.Property ViewTag m Box.Cursor ->
  Transaction.Property ViewTag m [ITreeD] ->
  Transaction ViewTag m (Widget (Transaction ViewTag m ()))
makeChildBox style depth clipboardRef outerBoxCursorRef childrenBoxCursorRef childrenIRefsRef = do
  childItems <- mapM (makeTreeEdit style (depth+1) clipboardRef) =<< Property.get childrenIRefsRef
  curChildIndex <- getChildIndex . length $ childItems
  childBox <- makeBox Box.vertical childItems childrenBoxCursorRef
  return .
    Widget.weakerKeys
    (mappend
     delNodeEventMap cutNodeEventMap
     curChildIndex) .
    Spacer.indentRightWidget indentSize $
    childBox
  where
    cutNodeEventMap =
      maybe mempty (fromKeyGroups Config.cutKeys "Cut node" . cutChild)
    delNodeEventMap =
      maybe mempty (fromKeyGroups Config.delChildKeys "Del node" . delChild)
    cutChild ix = do
      childrenIRefs <- Property.get childrenIRefsRef
      Property.pureModify clipboardRef (unsafeUnjust "Cut of wrong index" (index childrenIRefs ix) :)
      delChild ix
    delChild ix = do
      Property.pureModify childrenIRefsRef $ removeAt ix
      isEmpty <- null `liftM` Property.get childrenIRefsRef
      when isEmpty . Property.set outerBoxCursorRef $ 0
    getChildIndex count = validateIndex count `liftM`
                          Property.get childrenBoxCursorRef
    validateIndex count ix
      | 0 <= ix && ix < count = Just ix
      | otherwise = Nothing

simpleTextEdit ::
  Monad m =>
  TextEdit.Style -> Anim.AnimId ->
  Transaction.Property t m TextEdit.Model ->
  MWidget (Transaction t m)
simpleTextEdit style = makeTextEdit style "<empty>"

makeTreeEdit ::
  Monad m =>
  TextEdit.Style ->
  Int -> Transaction.Property ViewTag m [ITreeD] ->
  ITreeD -> Transaction ViewTag m (Widget (Transaction ViewTag m ()))
makeTreeEdit style depth clipboardRef treeIRef
  | depth >= Config.maxDepth =
    return $ Widget.strongerKeys goInEventMap $ focusableTextView style ["[Go deeper]"] ["deeper", bs $ guid treeIRef]
  | otherwise = do
    isExpanded <- Property.get isExpandedRef
    valueEdit <- liftM (Widget.strongerKeys $ expandCollapseEventMap isExpanded) $
                 simpleTextEdit style ["value edit", bs $ guid treeIRef] valueTextEditModelRef
    childrenIRefs <- Property.get childrenIRefsRef
    childBox <- if isExpanded && not (null childrenIRefs)
                then liftM ((:[]) . Widget.weakerKeys moveToParentEventMap) $
                     makeChildBox style depth clipboardRef outerBoxCursorRef childrenBoxCursorRef childrenIRefsRef
                else return []
    cValueEdit <- makeBox Box.horizontal
                  [collapser isExpanded,
                   Widget.liftView $ Spacer.makeHorizontal 1,
                   valueEdit]
                  (treeNodeBoxCursorRef 2) -- 2 points to valueEdit
    outerBox <- makeBox Box.vertical (cValueEdit : childBox) outerBoxCursorRef
    clipboard <- Property.get clipboardRef
    let keymap =
          mconcat [
            pasteEventMap clipboard,
            appendNewNodeEventMap,
            setFocalPointEventMap
            ]
    return . Widget.weakerKeys keymap $ outerBox
    where
      goInEventMap = fromKeyGroups Config.actionKeys "Go deeper" setFocalPoint
      treeRef = Transaction.fromIRef treeIRef
      valueRef = Data.nodeValue `composeLabel` treeRef
      boxCursorsContainer def k = Data.boxCursor def k `composeLabel` valueRef
      valueTextEditModelRef = Data.textEditModel `composeLabel` valueRef
      childrenIRefsRef = Data.nodeChildrenRefs `composeLabel` treeRef
      isExpandedRef = Data.isExpanded `composeLabel` valueRef
      outerBoxCursorRef = boxCursorsContainer 0 "outer"
      treeNodeBoxCursorRef initValue = boxCursorsContainer initValue "treeNode"
      childrenBoxCursorRef = boxCursorsContainer 0 "children"
      expandCollapseEventMap isExpanded =
        if isExpanded
        then fromKeyGroups Config.collapseKeys "Collapse" collapse
        else fromKeyGroups Config.expandKeys "Expand" expand
      collapse = Property.set isExpandedRef False
      expand = Property.set isExpandedRef True
      collapser isExpanded =
        flip (TextView.makeWidget style) ["collapser", bs $ guid treeIRef] $
        if isExpanded
        then ["[-]"]
        else ["[+]"]
      pasteEventMap [] = mempty
      pasteEventMap (cbChildRef:xs) =
        fromKeyGroups Config.pasteKeys "Paste" $ do
          appendChild cbChildRef
          Property.set clipboardRef xs

      appendNewNodeEventMap =
        fromKeyGroups Config.appendChildKeys "Append new child node" $
        appendChild =<< Data.makeLeafRef ""

      moveToParentEventMap =
        fromKeyGroups Config.moveToParentKeys "Move to parent" .
        Property.set outerBoxCursorRef $ 0

      setFocalPointEventMap = fromKeyGroups Config.setFocalPointKeys "Set focal point" $ setFocalPoint
      setFocalPoint = Property.pureModify Anchors.focalPointIRefs (treeIRef:)
      appendChild newRef = do
        appendBoxChild childrenBoxCursorRef childrenIRefsRef newRef
        Property.set outerBoxCursorRef $ 1

makeEditWidget ::
  Monad m =>
  TextEdit.Style ->
  Transaction.Property ViewTag m [ITreeD] ->
  Transaction ViewTag m (Widget (Transaction ViewTag m ()))
makeEditWidget style clipboardRef = do
  focalPointIRefs <- Property.get focalPointIRefsRef
  treeEdit <- makeTreeEdit style 0 clipboardRef (foldr const Anchors.rootIRef focalPointIRefs)
  widget <-
    if not $ isAtRoot focalPointIRefs
    then makeBox Box.vertical [goUpButton, treeEdit] (Anchors.viewBoxsAnchor "goUp")
    else return treeEdit
  return .
    Widget.strongerKeys (goUpEventMap focalPointIRefs) $
    widget
  where
    goUpButton = Widget.strongerKeys (fromKeyGroups Config.actionKeys "Go up" goUp) $
                 focusableTextView style ["[go up]"] ["upper", "edit widget"]
    focalPointIRefsRef = Anchors.focalPointIRefs
    isAtRoot = null
    goUpEventMap focalPointIRefs =
      if isAtRoot focalPointIRefs
      then mempty
      else fromKeyGroups Config.goUpKeys "Go up" goUp
    goUp = Property.pureModify focalPointIRefsRef (drop 1)

branchSelectorBoxCursor :: Monad m => Transaction.Property DBTag m Box.Cursor
branchSelectorBoxCursor = Anchors.dbBoxsAnchor "branchSelector"

-- Apply the transactions to the given View and convert them to
-- transactions on a DB
makeWidgetForView :: Monad m => TextEdit.Style -> View -> Transaction DBTag m (Widget (Transaction DBTag m ()))
makeWidgetForView style view = do
  versionData <- Version.versionData =<< View.curVersion view
  widget <- widgetDownTransaction (Anchors.viewStore view) $
            makeEditWidget style Anchors.clipboard
  let undoEventMap = maybe mempty makeUndoEventMap (Version.parent versionData)
  return $ Widget.strongerKeys undoEventMap widget
  where
    makeUndoEventMap = fromKeyGroups Config.undoKeys "Undo" . View.move view

fromKeyGroups :: [E.EventType] -> String -> a -> E.EventMap a
fromKeyGroups keys _doc act = mconcat $ map (`E.fromEventType` act) keys

defaultFont :: String -> FilePath
defaultFont "darwin" = "/Library/Fonts/Arial.ttf"
defaultFont _ = "/usr/share/fonts/truetype/freefont/FreeSerifBold.ttf"

main :: IO ()
main = do
  font <- Draw.openFont (defaultFont System.Info.os)
  Db.withDb "/tmp/treeedit.db" $ runDbStore font . Anchors.dbStore

runDbStore :: Draw.Font -> Transaction.Store DBTag IO -> IO a
runDbStore font store = do
  Anchors.initDB store
  mainLoopWidget . makeWidget $ store
  where
    style = TextEdit.Style font 50
    makeWidget dbStore = widgetDownTransaction dbStore $ do
      view <- Property.get Anchors.view
      branches <- Property.get Anchors.branches
      pairs <- mapM pair branches
      (branchSelector, branch) <- makeChoiceWidget Box.vertical pairs branchSelectorBoxCursor
      View.setBranch view branch
      viewEdit <- makeWidgetForView style view
      box <- makeBox Box.horizontal
        [viewEdit,
         Widget.liftView Spacer.makeHorizontalExpanding,
         Widget.strongerKeys (delBranchEventMap (length branches)) branchSelector]
        (Anchors.dbBoxsAnchor "main")
      return $ Widget.strongerKeys (mappend quitEventMap makeBranchEventMap) box

    pair (textEditModelIRef, version) = do
      textEdit <- simpleTextEdit style [bs . guid $ textEditModelIRef] . Transaction.fromIRef $ textEditModelIRef
      return (textEdit, version)

    delBranchEventMap numBranches
      | 1 == numBranches = mempty
      | otherwise =
        fromKeyGroups Config.delBranchKeys "Delete Branch" $ do
          _ <- popCurChild branchSelectorBoxCursor Anchors.branches
          return ()

    quitEventMap = fromKeyGroups Config.quitKeys "Quit" (fail "Quit")

    makeBranchEventMap =
      fromKeyGroups Config.makeBranchKeys "New Branch" $ do
        view <- Property.get Anchors.view
        branch <- Branch.new =<< View.curVersion view
        textEditModelIRef <- Transaction.newIRef $ TextEdit.makeModel "New view"
        let viewPair = (textEditModelIRef, branch)
        appendBoxChild branchSelectorBoxCursor Anchors.branches viewPair
