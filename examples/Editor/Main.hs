{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE FlexibleInstances, TypeOperators, Rank2Types,
             OverloadedStrings, UndecidableInstances,
             GeneralizedNewtypeDeriving #-}

module Main(main) where

import Control.Arrow (first)
import Control.Category ((.))
import Control.Monad (when, liftM, forM, unless)
import Control.Monad.Trans.Class (lift)
import Data.List (findIndex, elemIndex)
import Data.List.Utils (enumerate, nth, removeAt)
import Data.Maybe (fromMaybe, isJust)
import Data.Monoid(Monoid(..))
import Data.Store.Property (pureCompose, composeLabel)
import Data.Store.Rev.View (View)
import Data.Store.Transaction (Transaction)
import Editor.Anchors (DBTag, ViewTag, Cursor)
import Editor.Data (ITreeD)
import Graphics.UI.Bottle.MainLoop(mainLoopWidget)
import Graphics.UI.Bottle.Widget (Widget)
import Prelude hiding ((.))
import qualified Control.Monad.Trans.Reader as Reader
import qualified Data.Store.Db as Db
import qualified Data.Store.Property as Property
import qualified Data.Store.Rev.Branch as Branch
import qualified Data.Store.Rev.Version as Version
import qualified Data.Store.Rev.View as View
import qualified Data.Store.Transaction as Transaction
import qualified Editor.Anchors as Anchors
import qualified Editor.AnimIds as AnimIds
import qualified Editor.Config as Config
import qualified Editor.Data as Data
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Box as Box
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Graphics.UI.Bottle.Widgets.Spacer as Spacer
import qualified Graphics.UI.Bottle.Widgets.TextEdit as TextEdit
import qualified Graphics.UI.Bottle.Widgets.TextView as TextView
import qualified System.Info

newtype CTransaction t m a = CTransaction {
  unCTransaction :: Reader.ReaderT Cursor (Transaction t m) a
  }
  deriving (Monad)

runCTransaction :: Cursor -> CTransaction t m a -> Transaction t m a
runCTransaction cursor =
  (`Reader.runReaderT` cursor) . unCTransaction

readCursor :: Monad m => CTransaction t m Cursor
readCursor = CTransaction Reader.ask

transaction :: Monad m => Transaction t m a -> CTransaction t m a
transaction = CTransaction . lift

type TWidget t m = CTransaction t m (Widget (Transaction t m))

class (Monad m, Functor m) => MonadF m
instance (Monad m, Functor m) => MonadF m

focusableTextView :: MonadF m => TextEdit.Style -> String -> Anim.AnimId -> TWidget t m
focusableTextView style text animId = do
  hasFocus <- liftM (animId ==) readCursor
  let
    textView =
      Widget.takesFocus (const (return animId)) $
      TextView.makeWidget (TextEdit.sTextViewStyle style) text animId
    widget =
      (if hasFocus
         then Widget.backgroundColor AnimIds.backgroundCursorId blue
         else id) textView
  return $ (Widget.atIsFocused . const) hasFocus widget
  where
    blue = Draw.Color 0 0 1 0.8

makeChoice ::
  (Monad m) =>
  Anim.AnimId -> Transaction.Property t m Int -> Box.Orientation ->
  [TWidget t m] -> TWidget t m
makeChoice selectionAnimId curChoiceRef orientation children = do
  curChoice <- transaction $ Property.get curChoiceRef
  focusables <- sequence children
  let
    widget =
      Box.makeBiased orientation curChoice .
      nth curChoice (Widget.backgroundColor selectionAnimId selectedColor) .
      map updateCurChoice $
      enumerate focusables
  return widget
  where
    updateCurChoice (i, focusable) =
      Widget.atEvents (Property.set curChoiceRef i >>) focusable
    selectedColor = Draw.Color 0 0.5 0 1

wrapDelegated :: Monad m => (Anim.AnimId -> TWidget t m) -> Anim.AnimId -> TWidget t m
wrapDelegated f animId = do
  let textEditAnimId = AnimIds.delegating animId
  innerWidget <- f textEditAnimId
  let
    cursorSelf _ = makeDelegator $ Just FocusDelegator.NotDelegating
    cursorNotSelf
      | Widget.isFocused innerWidget =
        makeDelegator $ Just FocusDelegator.Delegating
      | otherwise = makeDelegator Nothing

    selfAnimId = AnimIds.notDelegating animId

    entryState = FocusDelegator.NotDelegating

    makeDelegator delegateCursor =
      (Widget.atIsFocused . const) (isJust delegateCursor) $
      FocusDelegator.make entryState delegateCursor
      selfAnimId FocusDelegator.defaultKeys
      AnimIds.backgroundCursorId innerWidget
  cursor <- readCursor
  return .
    fromMaybe cursorNotSelf . fmap cursorSelf $
    Anim.subId selfAnimId cursor

simpleTextEdit ::
  Monad m =>
  TextEdit.Style -> Transaction.Property t m String ->
  Anim.AnimId -> TWidget t m
simpleTextEdit style textRef animId = do
  text <- transaction $ Property.get textRef
  let
    lifter newText newCursor = do
      when (newText /= text) $ Property.set textRef newText
      return newCursor
  cursor <- readCursor
  return .
    Widget.atEvents (uncurry lifter) $
    TextEdit.make style cursor text animId

------

makeChildBox ::
  MonadF m => TextEdit.Style -> Cursor -> Int ->
  Transaction.Property ViewTag m [ITreeD] ->
  Transaction.Property ViewTag m [ITreeD] ->
  TWidget ViewTag m
makeChildBox style parentCursor depth clipboardRef childrenIRefsRef = do
  childrenIRefs <- transaction $ Property.get childrenIRefsRef
  childItems <-
    forM (enumerate childrenIRefs) $ \(curChildIndex, childIRef) ->
      let
        delNodeEventMap =
          fromKeyGroups Config.delChildKeys "Del node" delChild
        cutNodeEventMap =
          fromKeyGroups Config.cutKeys "Cut node" $ do
            Property.pureModify clipboardRef (childIRef:)
            delChild

        delChild = do
          Property.pureModify childrenIRefsRef $ removeAt curChildIndex
          return $ Widget.eventResultFromCursor parentCursor
      in
        liftM
          (Widget.weakerKeys $
           mappend delNodeEventMap cutNodeEventMap) $
        makeTreeEdit style (depth+1) clipboardRef childIRef

  let childBox = Box.make Box.vertical childItems
  return .
    Spacer.indentRightWidget indentSize $
    childBox
  where
    indentSize = 80

makeTreeEdit ::
  MonadF m =>
  TextEdit.Style ->
  Int -> Transaction.Property ViewTag m [ITreeD] ->
  ITreeD -> TWidget ViewTag m
makeTreeEdit style depth clipboardRef treeIRef
  | depth >= Config.maxDepth =
    liftM (Widget.strongerKeys goDeeperEventMap) $
      focusableTextView style "[Go deeper]" animId
  | otherwise = do
    isExpanded <- transaction $ Property.get isExpandedRef
    valueEdit <-
      liftM (Widget.strongerKeys $ expandCollapseEventMap isExpanded) $
      simpleTextEdit style valueTextEditModelRef animId
    childrenIRefs <- transaction $ Property.get childrenIRefsRef
    childBoxL <-
      if isExpanded && not (null childrenIRefs)
        then do
          let
            moveToParentEventMap =
              fromKeyGroups Config.moveToParentKeys "Move to parent" .
              return $ Widget.eventResultFromCursor myCursor
          liftM ((:[]) . Widget.weakerKeys moveToParentEventMap) $
            makeChildBox style myCursor depth clipboardRef childrenIRefsRef
        else return []
    let
      cValueEdit =
        Box.make Box.horizontal
        [Widget.liftView $ collapser isExpanded,
         Widget.liftView $ Spacer.makeHorizontal 1,
         valueEdit]
      outerBox = Box.make Box.vertical (cValueEdit : childBoxL)
    clipboard <- transaction $ Property.get clipboardRef
    let
      keymap =
        mconcat [
          pasteEventMap clipboard,
          appendNewNodeEventMap,
          setFocalPointEventMap
          ]
    return $ Widget.weakerKeys keymap outerBox
    where
      goDeeperEventMap = fromKeyGroups Config.actionKeys "Go deeper" setFocalPoint
      animId = AnimIds.fromIRef treeIRef
      myCursor = animId
      treeRef = Transaction.fromIRef treeIRef
      valueRef                  = Data.nodeValue        `composeLabel` treeRef
      valueTextEditModelRef     = Data.textEditModel    `composeLabel` valueRef
      childrenIRefsRef          = Data.nodeChildrenRefs `composeLabel` treeRef
      isExpandedRef             = Data.isExpanded       `composeLabel` valueRef
      expandCollapseEventMap isExpanded =
        (fmap . liftM . const) Widget.emptyEventResult $
        if isExpanded
        then fromKeyGroups Config.collapseKeys "Collapse" collapse
        else fromKeyGroups Config.expandKeys "Expand" expand
      collapse = Property.set isExpandedRef False
      expand = Property.set isExpandedRef True
      collapser isExpanded =
        flip (TextView.make (TextEdit.sTextViewStyle style)) (AnimIds.collapserId animId) $
        if isExpanded
        then "[-]"
        else "[+]"

      pasteEventMap [] = mempty
      pasteEventMap (cbChildRef:xs) =
        fromKeyGroups Config.pasteKeys "Paste" $ do
          Property.set clipboardRef xs
          appendChild cbChildRef

      appendNewNodeEventMap =
        fromKeyGroups Config.appendChildKeys "Append new child node" $
        appendChild =<< Data.makeLeafRef ""

      setFocalPointEventMap = fromKeyGroups Config.setFocalPointKeys "Set focal point" setFocalPoint
      setFocalPoint = do
        Property.pureModify Anchors.focalPointIRefs (treeIRef:)
        return $ Widget.eventResultFromCursor AnimIds.goUpId
      appendChild newRef = do
        Property.pureModify childrenIRefsRef (++ [newRef])
        return . Widget.eventResultFromCursor $ AnimIds.fromIRef newRef

getFocalPoint :: Monad m => Transaction ViewTag m (Bool, ITreeD)
getFocalPoint = do
  focalPointIRefs <- Property.get Anchors.focalPointIRefs
  return $ case focalPointIRefs of
    [] -> (True, Anchors.rootIRef)
    (x:_) -> (False, x)

makeEditWidget ::
  MonadF m =>
  TextEdit.Style ->
  Transaction.Property ViewTag m [ITreeD] ->
  TWidget ViewTag m
makeEditWidget style clipboardRef = do
  (isAtRoot, focalPoint) <- transaction getFocalPoint
  treeEdit <- makeTreeEdit style 0 clipboardRef focalPoint
  let
    goUp = do
      Property.pureModify Anchors.focalPointIRefs (drop 1)
      return . Widget.eventResultFromCursor $ AnimIds.fromIRef focalPoint
    goUpButtonEventMap =
      fromKeyGroups Config.actionKeys "Go up" goUp
    goUpEventMap =
      if isAtRoot
      then mempty
      else fromKeyGroups Config.goUpKeys "Go up" goUp

  goUpButton <-
    liftM
    (Widget.strongerKeys goUpButtonEventMap) $
    focusableTextView style "[go up]" AnimIds.goUpId

  let
    focusable
      | isAtRoot = treeEdit
      | otherwise = Box.make Box.vertical [goUpButton, treeEdit]

  return $ Widget.strongerKeys goUpEventMap focusable

-- Apply the transactions to the given View and convert them to
-- transactions on a DB
makeWidgetForView :: MonadF m => TextEdit.Style -> View -> TWidget DBTag m
makeWidgetForView style view = do
  versionData <- transaction $ Version.versionData =<< View.curVersion view
  cursor <- readCursor
  focusable <-
    widgetDownTransaction .
    runCTransaction cursor .
    (liftM . Widget.atEvents) (>>= saveCursor) $
    makeEditWidget style Anchors.clipboard
  let undoEventMap = maybe mempty makeUndoEventMap (Version.parent versionData)
  return $ Widget.strongerKeys undoEventMap focusable
  where
    makeUndoEventMap = fromKeyGroups Config.undoKeys "Undo" . (>> fetchRevisionCursor) . View.move view
    fetchRevisionCursor =
      liftM Widget.eventResultFromCursor .
      Transaction.run store $
      Property.get Anchors.cursor
    store = Anchors.viewStore view
    widgetDownTransaction =
      transaction . Transaction.run store .
      (liftM . Widget.atEvents) (Transaction.run store)
    saveCursor eventResult = do
      isEmpty <- Transaction.isEmpty
      unless isEmpty $ maybeUpdateCursor eventResult
      return eventResult

maybeUpdateCursor :: Monad m => Widget.EventResult -> Transaction t m ()
maybeUpdateCursor = maybe (return ()) (Property.set Anchors.cursor) . Widget.eCursor

fromKeyGroups :: [E.EventType] -> String -> a -> E.EventMap a
fromKeyGroups keys _doc act = mconcat $ map (`E.fromEventType` act) keys

defaultFont :: String -> FilePath
defaultFont "darwin" = "/Library/Fonts/Arial.ttf"
defaultFont _ = "/usr/share/fonts/truetype/freefont/FreeSerifBold.ttf"

main :: IO ()
main = do
  font <- Draw.openFont (defaultFont System.Info.os)
  Db.withDb "/tmp/treeedit.db" $ runDbStore font . Anchors.dbStore

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

makeRootWidget :: MonadF m => TextEdit.Style -> TWidget DBTag m
makeRootWidget style = do
  view <- transaction $ Property.get Anchors.view
  namedBranches <- transaction $ Property.get Anchors.branches

  let
    makeBranchNameEdit textEditModelIRef =
      wrapDelegated (simpleTextEdit style (Transaction.fromIRef textEditModelIRef)) (AnimIds.fromIRef textEditModelIRef)
    guiBranches = (map . first) makeBranchNameEdit namedBranches

    branches = map snd guiBranches
    branchIndexRef =
      pureCompose
        (fromMaybe (error "Selected branch not in branch list") .
         (`elemIndex` branches)) (branches !!)
        Anchors.currentBranch

  branchSelector <-
    makeChoice AnimIds.branchSelection branchIndexRef
    Box.vertical (map fst guiBranches)
  branch <- transaction $ Property.get Anchors.currentBranch

  -- TODO: This setBranch should happen in the choice widget and not
  -- here!
  transaction $ View.setBranch view branch

  viewEdit <- makeWidgetForView style view
  let
    delBranchEventMap numBranches
      | 1 == numBranches = mempty
      | otherwise =
        fromKeyGroups Config.delBranchKeys "Delete Branch" $ do
          deleteCurrentBranch
          return Widget.emptyEventResult
    box =
      Box.make Box.horizontal
      [viewEdit
      ,Widget.liftView Spacer.makeHorizontalExpanding
      ,Widget.strongerKeys (delBranchEventMap (length branches))
       branchSelector
      ]

    makeBranchEventMap =
      fromKeyGroups Config.makeBranchKeys "New Branch" $ do
        newBranch <- Branch.new =<< View.curVersion view
        textEditModelIRef <- Transaction.newIRef "New view"
        let viewPair = (textEditModelIRef, newBranch)
        Property.pureModify Anchors.branches (++ [viewPair])
        Property.set Anchors.currentBranch newBranch
        return Widget.emptyEventResult

    quitEventMap = fromKeyGroups Config.quitKeys "Quit" (error "Quit")

  return $
    Widget.strongerKeys
      (mappend quitEventMap makeBranchEventMap)
    box

runDbStore :: Draw.Font -> Transaction.Store DBTag IO -> IO a
runDbStore font store = do
  Anchors.initDB store
  mainLoopWidget makeWidget
  where
    style = TextEdit.Style {
      TextEdit.sTextViewStyle =
        TextView.Style {
          TextView.styleFont = font,
          TextView.styleFontSize = 50
          },
      TextEdit.sCursorColor = TextEdit.defaultCursorColor,
      TextEdit.sCursorWidth = TextEdit.defaultCursorWidth,
      TextEdit.sTextCursorId = AnimIds.textCursorId,
      TextEdit.sBackgroundCursorId = AnimIds.backgroundCursorId,
      TextEdit.sEmptyString = "<empty>"
      }

    fromCursor cursor = runCTransaction cursor $ makeRootWidget style
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

    widgetDownTransaction = Transaction.run store . (liftM . Widget.atEvents) (Transaction.run store)

    attachCursor eventResult = do
      maybeUpdateCursor eventResult
      return eventResult
