{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE FlexibleInstances, Rank2Types,
             OverloadedStrings, UndecidableInstances,
             GeneralizedNewtypeDeriving #-}

module Main(main) where

import Control.Category ((.))
import Control.Monad (when, liftM, forM, unless)
import Control.Monad.Trans.Class (lift)
import Data.List (findIndex, elemIndex)
import Data.List.Utils (enumerate, nth, removeAt)
import Data.Maybe (fromMaybe, isJust)
import Data.Monoid(Monoid(..))
import Data.Store.Property (Property)
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
import qualified Graphics.UI.Bottle.Widgets.EventMapDoc as EventMapDoc
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

getP :: Monad m => Property (Transaction t m) a -> CTransaction t m a
getP = transaction . Property.get

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
  curChoice <- getP curChoiceRef
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
  text <- getP textRef
  let
    lifter newText =
      applyAndReturn . const . when (newText /= text) $ Property.set textRef newText
  cursor <- readCursor
  return .
    Widget.atEvents (uncurry lifter) $
    TextEdit.make style cursor text animId

eventMapMovesCursor ::
  Monad m => [E.EventType] -> E.Doc -> m Cursor -> Widget.EventHandlers m
eventMapMovesCursor keys doc act =
  (fmap . liftM) Widget.eventResultFromCursor .
  mconcat $
  map (flip (`E.fromEventType` doc) act) keys

------

makeChildBox ::
  MonadF m => TextEdit.Style -> Cursor -> Int ->
  Transaction.Property ViewTag m [ITreeD] ->
  Transaction.Property ViewTag m [ITreeD] ->
  TWidget ViewTag m
makeChildBox style parentCursor depth clipboardRef childrenIRefsRef = do
  childrenIRefs <- getP childrenIRefsRef
  childItems <-
    forM (enumerate childrenIRefs) $ \(curChildIndex, childIRef) ->
      let
        delNodeEventMap =
          eventMapMovesCursor Config.delChildKeys "Del node" delChild
        cutNodeEventMap =
          eventMapMovesCursor Config.cutKeys "Cut node" $ do
            Property.pureModify clipboardRef (childIRef:)
            delChild

        delChild = do
          Property.pureModify childrenIRefsRef $ removeAt curChildIndex
          return parentCursor
      in
        liftM
          (Widget.weakerEvents $
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
    liftM (Widget.strongerEvents goDeeperEventMap) $
      focusableTextView style "[Go deeper]" animId
  | otherwise = do
    isExpanded <- getP isExpandedRef
    valueEdit <-
      liftM (Widget.strongerEvents $ expandCollapseEventMap isExpanded) $
      simpleTextEdit style valueTextEditModelRef animId
    childrenIRefs <- getP childrenIRefsRef
    childBoxL <-
      if isExpanded && not (null childrenIRefs)
        then do
          let
            moveToParentEventMap =
              eventMapMovesCursor Config.moveToParentKeys "Move to parent" $
              return myCursor
          liftM ((:[]) . Widget.weakerEvents moveToParentEventMap) $
            makeChildBox style myCursor depth clipboardRef childrenIRefsRef
        else return []
    let
      cValueEdit =
        Box.make Box.horizontal
        [Widget.liftView $ collapser isExpanded,
         Widget.liftView $ Spacer.makeHorizontal 1,
         valueEdit]
      outerBox = Box.make Box.vertical (cValueEdit : childBoxL)
    clipboard <- getP clipboardRef
    let
      keymap =
        mconcat [
          pasteEventMap clipboard,
          appendNewNodeEventMap,
          setFocalPointEventMap
          ]
    return $ Widget.weakerEvents keymap outerBox
    where
      animId = AnimIds.fromIRef treeIRef
      myCursor = animId
      treeRef = Transaction.fromIRef treeIRef
      valueRef = Property.composeLabel Data.nodeValue Data.atNodeValue treeRef
      valueTextEditModelRef = Property.composeLabel Data.textEditModel Data.atTextEditModel valueRef
      childrenIRefsRef = Property.composeLabel Data.nodeChildrenRefs Data.atNodeChildrenRefs treeRef
      isExpandedRef = Property.composeLabel Data.isExpanded Data.atIsExpanded valueRef
      expandCollapseEventMap isExpanded
        | isExpanded = Widget.actionEventMap Config.collapseKeys "Collapse" collapse
        | otherwise = Widget.actionEventMap Config.expandKeys "Expand" expand
      collapse = Property.set isExpandedRef False
      expand = Property.set isExpandedRef True
      collapser isExpanded =
        flip (TextView.make (TextEdit.sTextViewStyle style)) (AnimIds.collapserId animId) $
        if isExpanded
        then "[-]"
        else "[+]"

      pasteEventMap [] = mempty
      pasteEventMap (cbChildRef:xs) =
        eventMapMovesCursor Config.pasteKeys "Paste" $ do
          Property.set clipboardRef xs
          appendChild cbChildRef

      appendNewNodeEventMap =
        eventMapMovesCursor Config.appendChildKeys "Append new child node" $
        appendChild =<< Data.makeLeafRef ""

      goDeeperEventMap = eventMapMovesCursor Config.actionKeys "Go deeper" setFocalPoint
      setFocalPointEventMap = eventMapMovesCursor Config.setFocalPointKeys "Set focal point" setFocalPoint
      setFocalPoint = do
        Property.pureModify Anchors.focalPointIRefs (treeIRef:)
        return AnimIds.goUpId
      appendChild newRef = do
        Property.pureModify childrenIRefsRef (++ [newRef])
        return $ AnimIds.fromIRef newRef

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
      return $ AnimIds.fromIRef focalPoint
    goUpButtonEventMap =
      eventMapMovesCursor Config.actionKeys "Go up" goUp
    goUpEventMap =
      if isAtRoot
      then mempty
      else eventMapMovesCursor Config.goUpKeys "Go up" goUp

  goUpButton <-
    liftM
    (Widget.strongerEvents goUpButtonEventMap) $
    focusableTextView style "[go up]" AnimIds.goUpId

  let
    focusable
      | isAtRoot = treeEdit
      | otherwise = Box.make Box.vertical [goUpButton, treeEdit]

  return $ Widget.strongerEvents goUpEventMap focusable

-- Apply the transactions to the given View and convert them to
-- transactions on a DB
makeWidgetForView :: MonadF m => TextEdit.Style -> View -> TWidget DBTag m
makeWidgetForView style view = do
  versionData <- transaction $ Version.versionData =<< View.curVersion view
  cursor <- readCursor
  focusable <-
    widgetDownTransaction .
    runCTransaction cursor .
    (liftM . Widget.atEvents) (>>= applyAndReturn saveCursor) $
    makeEditWidget style Anchors.clipboard
  let undoEventMap = maybe mempty makeUndoEventMap (Version.parent versionData)
  return $ Widget.strongerEvents undoEventMap focusable
  where
    makeUndoEventMap = eventMapMovesCursor Config.undoKeys "Undo" . (>> fetchRevisionCursor) . View.move view
    fetchRevisionCursor = Transaction.run store $ Property.get Anchors.cursor
    store = Anchors.viewStore view
    widgetDownTransaction =
      transaction . Transaction.run store .
      (liftM . Widget.atEvents) (Transaction.run store)
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

makeRootWidget :: MonadF m => TextEdit.Style -> TWidget DBTag m
makeRootWidget style = do
  view <- getP Anchors.view
  namedBranches <- getP Anchors.branches

  viewEdit <- makeWidgetForView style view

  let
    branchIndexRef = branchSelectorProperty view $ map snd namedBranches
    makeBranchNameEdit textEditModelIRef =
      wrapDelegated (simpleTextEdit style (Transaction.fromIRef textEditModelIRef)) (AnimIds.fromIRef textEditModelIRef)
  branchSelector <-
    makeChoice AnimIds.branchSelection branchIndexRef
    Box.vertical $ map (makeBranchNameEdit . fst) namedBranches

  let
    delBranchEventMap
      | null (drop 1 namedBranches) = mempty
      | otherwise = Widget.actionEventMap Config.delBranchKeys "Delete Branch" deleteCurrentBranch
  return .
    (Widget.strongerEvents . mconcat)
      [Widget.actionEventMap Config.quitKeys "Quit" (error "Quit")
      ,Widget.actionEventMap Config.makeBranchKeys "New Branch" (makeBranch view)
      ] $
    Box.make Box.horizontal
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

    widgetDownTransaction =
      Transaction.run store .
      (liftM . Widget.atEvents) (Transaction.run store)

    attachCursor eventResult = do
      maybeUpdateCursor eventResult
      return eventResult
