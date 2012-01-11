{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE TypeOperators, Rank2Types, OverloadedStrings #-}

module Main(main) where

import Control.Arrow (first)
import Control.Category ((.))
import Control.Monad (liftM, forM)
import Data.List (find, findIndex, isPrefixOf, elemIndex)
import Data.List.Utils (enumerate, nth, removeAt)
import Data.Maybe (fromMaybe, isJust)
import Data.Monoid(Monoid(..))
import Data.Store.Property (pureCompose, composeLabel)
import Data.Store.Rev.View (View)
import Data.Store.Transaction (Transaction)
import Editor.Anchors (DBTag, ViewTag, Cursor)
import Editor.Data (ITreeD)
import Graphics.UI.Bottle.MainLoop(mainLoopWidget)
import Graphics.UI.Bottle.Sized (Sized)
import Graphics.UI.Bottle.Widget (Widget)
import Prelude hiding ((.))
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
import qualified Graphics.UI.Bottle.AnimIds as AnimIds
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Box as Box
import qualified Graphics.UI.Bottle.Widgets.Spacer as Spacer
import qualified Graphics.UI.Bottle.Widgets.TextEdit as TextEdit
import qualified Graphics.UI.Bottle.Widgets.TextView as TextView
import qualified System.Info

data Focusable a = Focusable {
  fIsFocused :: Bool,
  fWidget :: Widget a
  }

focusableFromView :: Sized Anim.Frame -> Focusable a
focusableFromView sizedFrame = Focusable {
  fIsFocused = False,
  fWidget = Widget.liftView $ sizedFrame
  }

atWidget ::
  (Widget a -> Widget b) ->
  Focusable a -> Focusable b
atWidget f (Focusable isFocused widget) =
  (Focusable isFocused (f widget))

type TWidget t m =
  Cursor -> Transaction t m (Focusable (Transaction t m Cursor))

focusableTextView :: Monad m => TextView.Style -> [String] -> Anim.AnimId -> TWidget t m
focusableTextView style textLines animId cursor = return focusable
  where
    focusable = Focusable {
      fIsFocused = hasFocus,
      fWidget = fmap return widget
    }

    widget =
      (if hasFocus
         then Widget.backgroundColor AnimIds.backgroundCursorId blue
         else id) .
      Widget.takesFocus animId $
      TextView.makeWidget style textLines animId

    hasFocus = animId == cursor
    blue = Draw.Color 0 0 1 0.8

indentSize :: Num a => a
indentSize = 80

makeBox ::
  Monad m => Box.Orientation -> [Focusable (m Cursor)] -> Focusable (m Cursor)
makeBox orientation children =
  Focusable {
    fIsFocused = isJust mChildIndex,
    fWidget = Box.make orientation mChildIndex $ map fWidget children
    }
  where
    mChildIndex = fmap fst . find (fIsFocused . snd) $ enumerate children

makeChoice ::
  (Monad m) =>
  Anim.AnimId -> Transaction.Property t m Int -> Box.Orientation ->
  [(Cursor, TWidget t m)] -> TWidget t m
makeChoice selectionAnimId curChoiceRef orientation children cursor = do
  curChoice <- Property.get curChoiceRef
  let ids = map fst children
  focusables <- mapM (($ cursor) . snd) children
  return .
    (atWidget . Widget.atEnter . const . return) (ids !! curChoice) .
    makeBox orientation .
    (nth curChoice . atWidget) (Widget.backgroundColor selectionAnimId selectedColor) .
    map updateCurChoice $
    enumerate focusables
  where
    updateCurChoice (i, focusable) =
      (atWidget . fmap) (Property.set curChoiceRef i >>) focusable
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
        (liftM . atWidget)
          (Widget.weakerKeys $
           mappend delNodeEventMap cutNodeEventMap) $
        makeTreeEdit style (depth+1) clipboardRef childIRef cursor

  let childBox = makeBox Box.vertical childItems
  return .
    atWidget (Spacer.indentRightWidget indentSize) $
    childBox

subId :: Anim.AnimId -> Anim.AnimId -> Maybe Anim.AnimId
subId folder path
  | folder `isPrefixOf` path = Just $ drop (length folder) path
  | otherwise = Nothing

joinId :: Anim.AnimId -> Anim.AnimId -> Anim.AnimId
joinId = (++)

simpleTextEdit ::
  Monad m =>
  TextEdit.Style -> Anim.AnimId ->
  Transaction.Property t m String ->
  TWidget t m
simpleTextEdit style animId textRef cursor = do
  text <- Property.get textRef
  let
    mCursor = fmap extractTextEditCursor $ subId animId cursor
    extractTextEditCursor [x] = BinUtils.decodeS x
    extractTextEditCursor _ = length text
    lifter newCursor newText = do
      Property.set textRef newText
      return . joinId animId . (:[]) . BinUtils.encodeS $ newCursor
  return $ Focusable {
    fIsFocused = isJust mCursor,
    fWidget = fmap (uncurry lifter) $ TextEdit.make style "<empty>" mCursor text animId
    }

makeTreeEdit ::
  Monad m =>
  TextEdit.Style ->
  Int -> Transaction.Property ViewTag m [ITreeD] ->
  ITreeD -> TWidget ViewTag m
makeTreeEdit style depth clipboardRef treeIRef cursor
  | depth >= Config.maxDepth =
    (liftM . atWidget) (Widget.strongerKeys goDeeperEventMap) $
      focusableTextView style ["[Go deeper]"] (AnimIds.deeperId animId) cursor
  | otherwise = do
    isExpanded <- Property.get isExpandedRef
    valueEdit <-
      (liftM . atWidget) (Widget.strongerKeys $ expandCollapseEventMap isExpanded) $
      simpleTextEdit style valueEditAnimId valueTextEditModelRef cursor
    childrenIRefs <- Property.get childrenIRefsRef
    childBoxL <-
      if isExpanded && not (null childrenIRefs)
        then do
          let
            moveToParentEventMap =
              fromKeyGroups Config.moveToParentKeys "Move to parent" $
              return myCursor
          liftM ((:[]) . atWidget (Widget.weakerKeys moveToParentEventMap)) $
            makeChildBox style myCursor depth clipboardRef childrenIRefsRef cursor
        else return []
    let
      cValueEdit =
        makeBox Box.horizontal
        [focusableFromView $ collapser isExpanded,
         focusableFromView $ Spacer.makeHorizontal 1,
         valueEdit]
      outerBox = makeBox Box.vertical (cValueEdit : childBoxL)
    clipboard <- Property.get clipboardRef
    let
      keymap =
        mconcat [
          pasteEventMap clipboard,
          appendNewNodeEventMap,
          setFocalPointEventMap
          ]
    return $ atWidget (Widget.weakerKeys keymap) outerBox
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
        flip (TextView.make style) (AnimIds.collapserId animId) $
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
        Property.pureModify Anchors.focalPointIRefs (treeIRef:) >> return myCursor
      appendChild newRef = do
        Property.pureModify childrenIRefsRef (++ [newRef])
        return . animIdOfTreeIRef $ newRef

animIdOfTreeIRef :: ITreeD -> Anim.AnimId
animIdOfTreeIRef = AnimIds.valueEditId . AnimIds.fromIRef -- todo: Remove this ugly duplication

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
    (liftM . atWidget)
    (Widget.strongerKeys
      (fromKeyGroups Config.actionKeys "Go up" goUp)) $
    focusableTextView style ["[go up]"] AnimIds.goUpId cursor

  let
    focusable
      | isAtRoot = treeEdit
      | otherwise = makeBox Box.vertical [goUpButton, treeEdit]

  return $ atWidget (Widget.strongerKeys goUpEventMap) focusable

-- Apply the transactions to the given View and convert them to
-- transactions on a DB
makeWidgetForView :: Monad m => TextEdit.Style -> View -> TWidget DBTag m
makeWidgetForView style view cursor = do
  versionData <- Version.versionData =<< View.curVersion view
  focusable <-
    widgetDownTransaction (Anchors.viewStore view) $
    makeEditWidget style Anchors.clipboard cursor
  let undoEventMap = maybe mempty makeUndoEventMap (Version.parent versionData)
  return $ atWidget (Widget.strongerKeys undoEventMap) focusable
  where
    makeUndoEventMap = (fmap . liftM . const) cursor . fromKeyGroups Config.undoKeys "Undo" . View.move view
    widgetDownTransaction store =
      Transaction.run store . (liftM . atWidget . fmap) (Transaction.run store)

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

runDbStore :: Draw.Font -> Transaction.Store DBTag IO -> IO a
runDbStore font store = do
  Anchors.initDB store
  mainLoopWidget makeWidget
  where
    style = TextEdit.Style font 50
    makeWidget = widgetDownTransaction $ do
      cursor <- Property.get Anchors.cursor
      view <- Property.get Anchors.view
      namedBranches <- Property.get Anchors.branches

      let
        makeBranchNameEdit textEditModelIRef =
          (animId,
           simpleTextEdit style animId (Transaction.fromIRef textEditModelIRef))
          where
            animId = AnimIds.fromIRef textEditModelIRef
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
          makeBox Box.horizontal
          [viewEdit
          ,focusableFromView Spacer.makeHorizontalExpanding
          ,atWidget (Widget.strongerKeys (delBranchEventMap (length branches)))
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

        focusable =
          atWidget
          (Widget.strongerKeys
           (mappend quitEventMap makeBranchEventMap))
          box
      return . fmap attachCursor . fWidget $ focusable

    widgetDownTransaction = Transaction.run store . (liftM . fmap) (Transaction.run store)

    attachCursor transaction = Property.set Anchors.cursor =<< transaction

    quitEventMap = fromKeyGroups Config.quitKeys "Quit" (error "Quit")
