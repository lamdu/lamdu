{-# LANGUAGE TypeOperators #-}
module Editor.BranchGUI(makeRootWidget) where

import Control.Arrow (second)
import Control.Applicative (pure)
import Control.Monad (liftM, liftM2, unless)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Writer (WriterT)
import Data.List (find, findIndex)
import Data.List.Utils (removeAt)
import Data.Maybe (fromMaybe, isJust)
import Data.Monoid(Monoid(..), Last(..))
import Data.Store.Rev.Branch (Branch)
import Data.Store.Rev.View (View)
import Data.Store.Transaction (Transaction)
import Editor.Anchors (ViewTag, DBTag)
import Editor.CTransaction (CTransaction, TWidget)
import Editor.MonadF (MonadF)
import Graphics.UI.Bottle.Widget (Widget)
import qualified Control.Monad.Trans.Writer as Writer
import qualified Data.Store.Property as Property
import qualified Data.Store.Rev.Branch as Branch
import qualified Data.Store.Rev.Version as Version
import qualified Data.Store.Rev.View as View
import qualified Data.Store.Transaction as Transaction
import qualified Editor.Anchors as Anchors
import qualified Editor.BottleWidgets as BWidgets
import qualified Editor.CTransaction as CT
import qualified Editor.Config as Config
import qualified Editor.WidgetIds as WidgetIds
import qualified Graphics.UI.Bottle.EventMap as E
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

makeBranch :: Monad m => View -> Transaction DBTag m Widget.Id
makeBranch view = do
  newBranch <- Branch.new =<< View.curVersion view
  textEditModelIRef <- Transaction.newIRef "New view"
  let viewPair = (textEditModelIRef, newBranch)
  Property.pureModify Anchors.branches (++ [viewPair])
  setCurrentBranch view newBranch
  return . FocusDelegator.delegatingId $
    WidgetIds.fromIRef textEditModelIRef

type CacheUpdatingTransaction t versionCache m =
  WriterT (Last versionCache) (Transaction t m)

tellNewCache
  :: Monad m
  => Transaction ViewTag (Transaction DBTag m) versionCache
  -> View -> CacheUpdatingTransaction DBTag versionCache m a
  -> CacheUpdatingTransaction DBTag versionCache m a
tellNewCache mkCache view act = do
  result <- act
  newCache <- lift $ Transaction.run (Anchors.viewStore view) mkCache
  Writer.tell $ Last (Just newCache)
  return result

branchNameFDConfig :: FocusDelegator.Config
branchNameFDConfig = FocusDelegator.Config
  { FocusDelegator.startDelegatingKey = E.ModKey E.noMods E.KeyF2
  , FocusDelegator.startDelegatingDoc = "Rename branch"
  , FocusDelegator.stopDelegatingKey = E.ModKey E.noMods E.KeyEnter
  , FocusDelegator.stopDelegatingDoc = "Stop renaming"
  }

branchSelectionFocusDelegatorConfig :: FocusDelegator.Config
branchSelectionFocusDelegatorConfig = FocusDelegator.Config
  { FocusDelegator.startDelegatingKey = E.ModKey E.noMods E.KeyEnter
  , FocusDelegator.startDelegatingDoc = "Enter select branches mode"
  , FocusDelegator.stopDelegatingKey = E.ModKey E.noMods E.KeyEnter
  , FocusDelegator.stopDelegatingDoc = "Select branch"
  }

makeRootWidget
  :: MonadF m
  => Transaction ViewTag (Transaction DBTag m) versionCache
  -> TWidget ViewTag (Transaction DBTag m)
  -> CTransaction DBTag m (Widget (CacheUpdatingTransaction DBTag versionCache m))
makeRootWidget mkCache widget = do
  view <- CT.getP Anchors.view
  namedBranches <- CT.getP Anchors.branches
  viewEdit <- makeWidgetForView mkCache view widget
  currentBranch <- CT.getP Anchors.currentBranch

  let
    withNewCache = tellNewCache mkCache view
    makeBranchNameEdit (textEditModelIRef, branch) = do
      let branchEditId = WidgetIds.fromIRef textEditModelIRef
      branchNameEdit <-
        BWidgets.wrapDelegated branchNameFDConfig
        FocusDelegator.NotDelegating id
        (BWidgets.makeLineEdit (Transaction.fromIRef textEditModelIRef))
        branchEditId
      let
        setBranch action = withNewCache $ do
          lift $ setCurrentBranch view branch
          action
      return
        ( branch
        , (Widget.atMaybeEnter . fmap . fmap . Widget.atEnterResultEvent) setBranch .
          Widget.atEvents lift $ branchNameEdit
        )
    -- there must be an active branch:
    Just currentBranchWidgetId =
      fmap (WidgetIds.fromIRef . fst) $ find ((== currentBranch) . snd) namedBranches

  let
    delBranchEventMap
      | null (drop 1 namedBranches) = mempty
      | otherwise =
        Widget.keysEventMapMovesCursor Config.delBranchKeys "Delete Branch" .
        withNewCache . lift $ deleteCurrentBranch view

  branchSelectorFocused <-
    liftM isJust $ CT.subCursor WidgetIds.branchSelection
  branchSelector <-
    flip
    (BWidgets.wrapDelegated
     branchSelectionFocusDelegatorConfig
     FocusDelegator.NotDelegating id)
    WidgetIds.branchSelection $ \innerId ->
    CT.assignCursor innerId currentBranchWidgetId $ do
      branchNameEdits <-
        mapM ((liftM . second) (Widget.align 0) . makeBranchNameEdit)
        namedBranches
      return .
        Widget.strongerEvents delBranchEventMap $
        BWidgets.makeChoice branchSelectorFocused
        (Widget.toAnimId WidgetIds.branchSelection)
        Box.vertical branchNameEdits currentBranch

  let
    eventMap = mconcat
      [ Widget.keysEventMap Config.quitKeys "Quit" (error "Quit")
      , Widget.keysEventMapMovesCursor Config.makeBranchKeys "New Branch" .
        lift $ makeBranch view
      , Widget.keysEventMapMovesCursor Config.jumpToBranchesKeys
        "Select current branch" $ pure currentBranchWidgetId
      ]
  return .
    Widget.strongerEvents eventMap .
    BWidgets.vboxAlign 0 $
    [viewEdit
    ,Widget.liftView Spacer.makeVerticalExpanding
    ,branchSelector
    ]

-- Apply the transactions to the given View and convert them to
-- transactions on a DB
makeWidgetForView
  :: MonadF m
  => Transaction ViewTag (Transaction DBTag m) versionCache
  -> View
  -> TWidget ViewTag (Transaction DBTag m)
  -> CTransaction DBTag m (Widget (WriterT (Last versionCache) (Transaction DBTag m)))
makeWidgetForView mkCache view innerWidget = do
  curVersion <- CT.transaction $ View.curVersion view
  curVersionData <- CT.transaction $ Version.versionData curVersion
  redos <- CT.getP Anchors.redos
  cursor <- CT.readCursor

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
      Widget.keysEventMapMovesCursor Config.redoKeys "Redo" $
      redo version restRedos
    undoEventMap =
      maybe mempty
      (Widget.keysEventMapMovesCursor Config.undoKeys "Undo" .
       undo) $ Version.parent curVersionData

    eventMap = fmap (tellNewCache mkCache view . lift) $ mconcat [undoEventMap, redoEventMap redos]

    afterEvent action = do
      (eventResult, mCache) <- lift $ do
        eventResult <- action
        isEmpty <- Transaction.isEmpty
        mCache <-
          if isEmpty
          then return Nothing
          else do
            Property.set Anchors.preCursor cursor
            Property.set Anchors.postCursor . fromMaybe cursor $ Widget.eCursor eventResult
            liftM Just mkCache
        return (eventResult, mCache)
      Writer.tell $ Last mCache
      return eventResult

  vWidget <-
    CT.runNested store $
    (liftM . Widget.atEvents) afterEvent innerWidget

  let
    lowerWTransaction act = do
      (r, isEmpty) <-
        Writer.mapWriterT (Transaction.run store) .
        liftM2 (,) act $ lift Transaction.isEmpty
      lift . unless isEmpty $ Property.set Anchors.redos []
      return r

  return .
    Widget.strongerEvents eventMap $
    Widget.atEvents lowerWTransaction vWidget
  where
    store = Anchors.viewStore view
