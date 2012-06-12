{-# LANGUAGE TypeOperators #-}
module Editor.BranchGUI(makeRootWidget) where

import Control.Compose ((:.))
import Control.Monad (liftM, liftM2, unless)
import Data.List (find, findIndex)
import Data.List.Utils (removeAt)
import Data.Maybe (fromMaybe)
import Data.Monoid(Monoid(..))
import Data.Store.Rev.Branch (Branch)
import Data.Store.Rev.View (View)
import Data.Store.Transaction (Transaction)
import Editor.Anchors (ViewTag, DBTag)
import Editor.CTransaction (CTransaction, TWidget, runNestedCTransaction, transaction, getP, readCursor)
import Editor.MonadF (MonadF)
import Graphics.UI.Bottle.Widget (Widget)
import qualified Control.Compose as Compose
import qualified Data.Store.Property as Property
import qualified Data.Store.Rev.Branch as Branch
import qualified Data.Store.Rev.Version as Version
import qualified Data.Store.Rev.View as View
import qualified Data.Store.Transaction as Transaction
import qualified Editor.Anchors as Anchors
import qualified Editor.BottleWidgets as BWidgets
import qualified Editor.Config as Config
import qualified Editor.WidgetIds as WidgetIds
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Box as Box
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Graphics.UI.Bottle.Widgets.Spacer as Spacer

setCurrentBranch :: Monad m => View -> Branch -> Transaction DBTag m ()
setCurrentBranch view branch = do
  Property.set Anchors.currentBranch branch
  View.setBranch view branch

deleteCurrentBranch :: Monad m => View -> Transaction DBTag m Widget.Id
deleteCurrentBranch view = do
  branch <- Property.get Anchors.currentBranch
  branches <- Property.get Anchors.branches
  let
    index =
      fromMaybe (error "Invalid current branch!") $
      findIndex ((branch ==) . snd) branches
    newBranches = removeAt index branches
  Property.set Anchors.branches newBranches
  let
    newCurrentBranch =
      newBranches !! min (length newBranches - 1) index
  setCurrentBranch view $ snd newCurrentBranch
  return . WidgetIds.fromIRef $ fst newCurrentBranch

makeBranch :: Monad m => View -> Transaction DBTag m ()
makeBranch view = do
  newBranch <- Branch.new =<< View.curVersion view
  textEditModelIRef <- Transaction.newIRef "New view"
  let viewPair = (textEditModelIRef, newBranch)
  Property.pureModify Anchors.branches (++ [viewPair])
  setCurrentBranch view newBranch

addNewCache
  :: Monad m
  => Transaction ViewTag (Transaction DBTag m) versionCache
  -> View -> Transaction DBTag m a
  -> (Transaction DBTag m :. (,) (Maybe versionCache)) a
addNewCache mkCache view act = Compose.O $ do
  result <- act
  newCache <- Transaction.run (Anchors.viewStore view) mkCache
  return (Just newCache, result)

makeRootWidget
  :: MonadF m
  => Transaction ViewTag (Transaction DBTag m) versionCache
  -> TWidget ViewTag (Transaction DBTag m)
  -> CTransaction DBTag m (Widget (Transaction DBTag m :. (,) (Maybe versionCache)))
makeRootWidget mkCache widget = do
  view <- getP Anchors.view
  namedBranches <- getP Anchors.branches
  viewEdit <- makeWidgetForView mkCache view widget
  currentBranch <- getP Anchors.currentBranch

  let
    withNewCache = addNewCache mkCache view
    makeBranchNameEdit (textEditModelIRef, branch) = do
      let branchEditId = WidgetIds.fromIRef textEditModelIRef
      branchNameEdit <-
        BWidgets.wrapDelegated FocusDelegator.NotDelegating
        (BWidgets.makeTextEdit (Transaction.fromIRef textEditModelIRef)) $
        branchEditId
      let
        setBranch (Compose.O action) = withNewCache $ do
          setCurrentBranch view branch
          (_versionCache, result) <- action
          -- get rid of any VersionCache because we generate a new one
          -- in addNewCache:
          return result
      return
        ( branch
        , (Widget.atMaybeEnter . fmap . fmap . Widget.atEnterResultEvent) setBranch .
          Widget.atEvents (Compose.O . liftM ((,) Nothing)) $
          branchNameEdit
        )
  -- there must be an active branch:
  let
    Just currentBranchWidgetId =
      fmap (WidgetIds.fromIRef . fst) $ find ((== currentBranch) . snd) namedBranches

  branchNameEdits <- mapM makeBranchNameEdit namedBranches
  let
    branchSelector =
      BWidgets.makeChoice WidgetIds.branchSelection Box.vertical branchNameEdits currentBranch
    delBranchEventMap
      | null (drop 1 namedBranches) = mempty
      | otherwise =
        Widget.actionEventMapMovesCursor Config.delBranchKeys "Delete Branch" .
        withNewCache $ deleteCurrentBranch view
  return .
    (Widget.strongerEvents . mconcat)
      [ Widget.actionEventMap Config.quitKeys "Quit" (error "Quit")
      , Widget.actionEventMap Config.makeBranchKeys "New Branch" .
        noCacheChange $ makeBranch view
      , Widget.actionEventMapMovesCursor Config.jumpToBranchesKeys
        "Jump to branches" . noCacheChange $
        return currentBranchWidgetId
      ] .
    Box.toWidget . Box.make Box.horizontal $
    [viewEdit
    ,Widget.liftView Spacer.makeHorizontalExpanding
    ,Widget.strongerEvents delBranchEventMap branchSelector
    ]
  where
    noCacheChange = Compose.O . liftM ((,) Nothing)

-- Apply the transactions to the given View and convert them to
-- transactions on a DB
makeWidgetForView
  :: MonadF m
  => Transaction ViewTag (Transaction DBTag m) versionCache
  -> View
  -> TWidget ViewTag (Transaction DBTag m)
  -> CTransaction DBTag m (Widget (Transaction DBTag m :. (,) (Maybe versionCache)))
makeWidgetForView mkCache view innerWidget = do
  curVersion <- transaction $ View.curVersion view
  curVersionData <- transaction $ Version.versionData curVersion
  redos <- getP Anchors.redos
  cursor <- readCursor

  let
    redo version newRedos = do
      Property.set Anchors.redos newRedos
      View.move view version
      Transaction.run store $ Property.get Anchors.postCursor
    undo parentVersion = do
      preCursor <- Transaction.run store $ Property.get Anchors.preCursor
      View.move view parentVersion
      Property.pureModify Anchors.redos (curVersion:)
      return preCursor

    redoEventMap [] = mempty
    redoEventMap (version:restRedos) =
      Widget.actionEventMapMovesCursor Config.redoKeys "Redo" $
      redo version restRedos
    undoEventMap =
      maybe mempty
      (Widget.actionEventMapMovesCursor Config.undoKeys "Undo" .
       undo) $ Version.parent curVersionData

    eventMap = fmap (addNewCache mkCache view) $ mconcat [undoEventMap, redoEventMap redos]

    afterEvent action = Compose.O $ do
      eventResult <- action
      isEmpty <- Transaction.isEmpty
      mCache <-
        if isEmpty
        then return Nothing
        else do
          Property.set Anchors.preCursor cursor
          Property.set Anchors.postCursor . fromMaybe cursor $ Widget.eCursor eventResult
          liftM Just mkCache
      return (mCache, eventResult)

  vWidget <-
    runNestedCTransaction store $
    (liftM . Widget.atEvents) afterEvent innerWidget

  let
    lowerWTransaction act = Compose.O $ do
      (r, isEmpty) <-
        Transaction.run store $ liftM2 (,) (Compose.unO act) Transaction.isEmpty
      unless isEmpty $ Property.set Anchors.redos []
      return r

  return .
    Widget.strongerEvents eventMap $
    Widget.atEvents lowerWTransaction vWidget
  where
    store = Anchors.viewStore view
