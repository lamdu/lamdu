{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE TypeOperators, Rank2Types, OverloadedStrings #-}

module Main(main) where

import Control.Arrow (first)
import Control.Category ((.))
import Control.Monad (liftM, forM, when)
import Data.List (findIndex, isPrefixOf, elemIndex)
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
import qualified AnimIds
import qualified Data.Binary.Utils as BinUtils
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
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Box as Box
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Graphics.UI.Bottle.Widgets.Spacer as Spacer
import qualified Graphics.UI.Bottle.Widgets.TextEdit as TextEdit
import qualified Graphics.UI.Bottle.Widgets.TextView as TextView
import qualified System.Info

type TWidget t m =
  Cursor -> Transaction t m (Widget (Transaction t m Cursor))

focusableTextView :: Monad m => TextEdit.Style -> [String] -> Anim.AnimId -> TWidget t m
focusableTextView style textLines animId cursor =
  return .
    (Widget.atIsFocused . const) hasFocus $
    fmap return widget
  where
    widget =
      (if hasFocus
         then Widget.backgroundColor AnimIds.backgroundCursorId blue
         else id) .
      Widget.takesFocus (const animId) $
      TextView.makeWidget (TextEdit.sTextViewStyle style) textLines animId

    hasFocus = animId == cursor
    blue = Draw.Color 0 0 1 0.8

indentSize :: Num a => a
indentSize = 80

makeChoice ::
  (Monad m) =>
  Anim.AnimId -> Transaction.Property t m Int -> Box.Orientation ->
  [TWidget t m] -> TWidget t m
makeChoice selectionAnimId curChoiceRef orientation children cursor = do
  curChoice <- Property.get curChoiceRef
  focusables <- mapM ($ cursor) children
  let
    widget =
      Box.makeBiased orientation curChoice .
      nth curChoice (Widget.backgroundColor selectionAnimId selectedColor) .
      map updateCurChoice $
      enumerate focusables
  return widget
  where
    updateCurChoice (i, focusable) =
      fmap (Property.set curChoiceRef i >>) focusable
    selectedColor = Draw.Color 0 0.5 0 1

makeChildBox ::
  Monad m =>
  TextEdit.Style ->
  Cursor ->
  Int ->
  Transaction.Property ViewTag m [ITreeD] ->
  Transaction.Property ViewTag m [ITreeD] ->
  TWidget ViewTag m
makeChildBox style parentCursor depth clipboardRef childrenIRefsRef cursor = do
  childrenIRefs <- Property.get childrenIRefsRef
  childItems <-
    forM (enumerate childrenIRefs) $ \(curChildIndex, childIRef) ->
      let
        delNodeEventMap =
          fromKeyGroups Config.delChildKeys "Del node" $ delChild
        cutNodeEventMap =
          fromKeyGroups Config.cutKeys "Cut node" $ do
            Property.pureModify clipboardRef (childIRef:)
            delChild

        delChild = do
          Property.pureModify childrenIRefsRef $ removeAt curChildIndex
          return parentCursor
      in
        liftM
          (Widget.weakerKeys $
           mappend delNodeEventMap cutNodeEventMap) $
        makeTreeEdit style (depth+1) clipboardRef childIRef cursor

  let childBox = Box.make Box.vertical childItems
  return .
    Spacer.indentRightWidget indentSize $
    childBox

subId :: Anim.AnimId -> Anim.AnimId -> Maybe Anim.AnimId
subId folder path
  | folder `isPrefixOf` path = Just $ drop (length folder) path
  | otherwise = Nothing

wrapDelegated :: Monad m => (Anim.AnimId -> TWidget t m) -> Anim.AnimId -> TWidget t m
wrapDelegated f animId cursor = do
  let textEditAnimId = AnimIds.delegating animId
  innerWidget <- f textEditAnimId cursor
  let
    cursorSelf _ = makeDelegator $ Just FocusDelegator.NotDelegating
    cursorNotSelf
      | Widget.wIsFocused innerWidget =
        makeDelegator $ Just FocusDelegator.Delegating
      | otherwise = makeDelegator Nothing

    selfAnimId = AnimIds.notDelegating animId

    entryState = FocusDelegator.NotDelegating

    makeDelegator delegateCursor =
      (Widget.atIsFocused . const) (isJust delegateCursor) $
      FocusDelegator.make entryState delegateCursor
      (return selfAnimId) FocusDelegator.defaultKeys
      AnimIds.backgroundCursorId innerWidget
  return .
    fromMaybe cursorNotSelf . fmap cursorSelf $
    subId selfAnimId cursor

simpleTextEdit ::
  Monad m =>
  TextEdit.Style -> Transaction.Property t m String ->
  Anim.AnimId ->
  TWidget t m
simpleTextEdit style textRef animId cursor = do
  text <- Property.get textRef
  let
    mCursor = fmap extractTextEditCursor $ subId animId cursor
    extractTextEditCursor [x] = BinUtils.decodeS x
    extractTextEditCursor _ = length text
    lifter newCursor newText = do
      Property.set textRef newText
      return . Anim.joinId animId . (:[]) . BinUtils.encodeS $ newCursor
  return .
    (Widget.atIsFocused . const) (isJust mCursor) .
    fmap (uncurry lifter) $
    TextEdit.make style "<empty>" mCursor text animId

animIdOfTreeIRef :: ITreeD -> Anim.AnimId
animIdOfTreeIRef = AnimIds.valueEditId . AnimIds.fromIRef

makeTreeEdit ::
  Monad m =>
  TextEdit.Style ->
  Int -> Transaction.Property ViewTag m [ITreeD] ->
  ITreeD -> TWidget ViewTag m
makeTreeEdit style depth clipboardRef treeIRef cursor
  | depth >= Config.maxDepth =
    liftM (Widget.strongerKeys goDeeperEventMap) $
      focusableTextView style ["[Go deeper]"] (AnimIds.deeperId animId) cursor
  | otherwise = do
    isExpanded <- Property.get isExpandedRef
    valueEdit <-
      liftM (Widget.strongerKeys $ expandCollapseEventMap isExpanded) $
      simpleTextEdit style valueTextEditModelRef valueEditAnimId cursor
    childrenIRefs <- Property.get childrenIRefsRef
    childBoxL <-
      if isExpanded && not (null childrenIRefs)
        then do
          let
            moveToParentEventMap =
              fromKeyGroups Config.moveToParentKeys "Move to parent" $
              return myCursor
          liftM ((:[]) . Widget.weakerKeys moveToParentEventMap) $
            makeChildBox style myCursor depth clipboardRef childrenIRefsRef cursor
        else return []
    let
      cValueEdit =
        Box.make Box.horizontal
        [Widget.liftView $ collapser isExpanded,
         Widget.liftView $ Spacer.makeHorizontal 1,
         valueEdit]
      outerBox = Box.make Box.vertical (cValueEdit : childBoxL)
    clipboard <- Property.get clipboardRef
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
      valueEditAnimId = AnimIds.valueEditId animId
      myCursor = valueEditAnimId
      treeRef = Transaction.fromIRef treeIRef
      valueRef                  = Data.nodeValue        `composeLabel` treeRef
      valueTextEditModelRef     = Data.textEditModel    `composeLabel` valueRef
      childrenIRefsRef          = Data.nodeChildrenRefs `composeLabel` treeRef
      isExpandedRef             = Data.isExpanded       `composeLabel` valueRef
      expandCollapseEventMap isExpanded =
        (fmap . liftM . const) cursor $
        if isExpanded
        then fromKeyGroups Config.collapseKeys "Collapse" collapse
        else fromKeyGroups Config.expandKeys "Expand" expand
      collapse = Property.set isExpandedRef False
      expand = Property.set isExpandedRef True
      collapser isExpanded =
        flip (TextView.make (TextEdit.sTextViewStyle style)) (AnimIds.collapserId animId) $
        if isExpanded
        then ["[-]"]
        else ["[+]"]

      pasteEventMap [] = mempty
      pasteEventMap (cbChildRef:xs) =
        fromKeyGroups Config.pasteKeys "Paste" $ do
          Property.set clipboardRef xs
          appendChild cbChildRef

      appendNewNodeEventMap =
        fromKeyGroups Config.appendChildKeys "Append new child node" $
        appendChild =<< Data.makeLeafRef ""

      setFocalPointEventMap = fromKeyGroups Config.setFocalPointKeys "Set focal point" setFocalPoint
      setFocalPoint =
        Property.pureModify Anchors.focalPointIRefs (treeIRef:) >> return AnimIds.goUpId
      appendChild newRef = do
        Property.pureModify childrenIRefsRef (++ [newRef])
        return . animIdOfTreeIRef $ newRef

getFocalPoint :: Monad m => Transaction ViewTag m (Bool, ITreeD)
getFocalPoint = do
  focalPointIRefs <- Property.get Anchors.focalPointIRefs
  return $ case focalPointIRefs of
    [] -> (True, Anchors.rootIRef)
    (x:_) -> (False, x)

makeEditWidget ::
  Monad m =>
  TextEdit.Style ->
  Transaction.Property ViewTag m [ITreeD] ->
  TWidget ViewTag m
makeEditWidget style clipboardRef cursor = do
  (isAtRoot, focalPoint) <- getFocalPoint
  treeEdit <- makeTreeEdit style 0 clipboardRef focalPoint cursor
  let
    goUp = do
      Property.pureModify Anchors.focalPointIRefs (drop 1)
      liftM (animIdOfTreeIRef . snd) getFocalPoint
    goUpEventMap =
      if isAtRoot
      then mempty
      else fromKeyGroups Config.goUpKeys "Go up" goUp

  goUpButton <-
    liftM
    (Widget.strongerKeys
      (fromKeyGroups Config.actionKeys "Go up" goUp)) $
    focusableTextView style ["[go up]"] AnimIds.goUpId cursor

  let
    focusable
      | isAtRoot = treeEdit
      | otherwise = Box.make Box.vertical [goUpButton, treeEdit]

  return $ Widget.strongerKeys goUpEventMap focusable

-- Apply the transactions to the given View and convert them to
-- transactions on a DB
makeWidgetForView :: Monad m => TextEdit.Style -> View -> TWidget DBTag m
makeWidgetForView style view cursor = do
  versionData <- Version.versionData =<< View.curVersion view
  focusable <-
    widgetDownTransaction (Anchors.viewStore view) $
    makeEditWidget style Anchors.clipboard cursor
  let undoEventMap = maybe mempty makeUndoEventMap (Version.parent versionData)
  return $ Widget.strongerKeys undoEventMap focusable
  where
    makeUndoEventMap = (fmap . liftM . const) cursor . fromKeyGroups Config.undoKeys "Undo" . View.move view
    widgetDownTransaction store =
      Transaction.run store . (liftM . fmap) (Transaction.run store)

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
  Property.set Anchors.branches $ newBranches
  Property.set Anchors.currentBranch . snd $
    newBranches !! min (length newBranches - 1) index

makeRootWidget ::
  Monad m =>
  TextEdit.Style -> Cursor ->
  Transaction DBTag m (Widget (Transaction DBTag m Cursor))
makeRootWidget style cursor = do
  view <- Property.get Anchors.view
  namedBranches <- Property.get Anchors.branches

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
    Box.vertical (map fst guiBranches) cursor
  branch <- Property.get Anchors.currentBranch

  View.setBranch view branch
  viewEdit <- makeWidgetForView style view cursor
  let
    delBranchEventMap numBranches
      | 1 == numBranches = mempty
      | otherwise =
        fromKeyGroups Config.delBranchKeys "Delete Branch" $
          deleteCurrentBranch >> return cursor
    box =
      Box.make Box.horizontal
      [viewEdit
      ,Widget.liftView Spacer.makeHorizontalExpanding
      ,Widget.strongerKeys (delBranchEventMap (length branches))
       branchSelector
      ]
  let
    makeBranchEventMap =
      fromKeyGroups Config.makeBranchKeys "New Branch" $ do
        newBranch <- Branch.new =<< View.curVersion view
        textEditModelIRef <- Transaction.newIRef "New view"
        let viewPair = (textEditModelIRef, newBranch)
        Property.pureModify Anchors.branches (++ [viewPair])
        Property.set Anchors.currentBranch newBranch
        return cursor

  let
    quitEventMap = fromKeyGroups Config.quitKeys "Quit" (error "Quit")

  return $
    (Widget.strongerKeys
     (mappend quitEventMap makeBranchEventMap))
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
      TextEdit.sBackgroundCursorId = AnimIds.backgroundCursorId
      }
    makeWidget = widgetDownTransaction $ do
      cursor <- Property.get Anchors.cursor
      candidateWidget <- makeRootWidget style cursor
      focusable <-
        if Widget.wIsFocused candidateWidget
        then
          return candidateWidget
        else
          makeRootWidget style $ animIdOfTreeIRef Anchors.rootIRef

      when (not $ Widget.wIsFocused focusable) $
        fail "Root cursor did not match"

      return . fmap attachCursor $ focusable

    widgetDownTransaction = Transaction.run store . (liftM . fmap) (Transaction.run store)

    attachCursor transaction = Property.set Anchors.cursor =<< transaction
