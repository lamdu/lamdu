{-# LANGUAGE TypeOperators #-}
module Editor.BranchGUI(makeRootWidget) where

import Control.Applicative (pure, (<*))
import Control.Arrow (second)
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
import Editor.ITransaction (ITransaction)
import Editor.MonadF (MonadF)
import Editor.OTransaction (OTransaction, TWidget)
import Graphics.UI.Bottle.Widget (Widget)
import qualified Control.Monad.Trans.Writer as Writer
import qualified Data.Store.Rev.Branch as Branch
import qualified Data.Store.Rev.Version as Version
import qualified Data.Store.Rev.View as View
import qualified Data.Store.Transaction as Transaction
import qualified Editor.Anchors as Anchors
import qualified Editor.BottleWidgets as BWidgets
import qualified Editor.Config as Config
import qualified Editor.ITransaction as IT
import qualified Editor.OTransaction as OT
import qualified Editor.WidgetIds as WidgetIds
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Box as Box
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Graphics.UI.Bottle.Widgets.Spacer as Spacer

setCurrentBranch :: Monad m => View -> Branch -> Transaction DBTag m ()
setCurrentBranch view branch = do
  Anchors.setP Anchors.currentBranch branch
  View.setBranch view branch

deleteCurrentBranch :: Monad m => View -> Transaction DBTag m Widget.Id
deleteCurrentBranch view = do
  branch <- Anchors.getP Anchors.currentBranch
  branches <- Anchors.getP Anchors.branches
  let
    index =
      fromMaybe (error "Invalid current branch!") $
      findIndex ((branch ==) . snd) branches
    newBranches = removeAt index branches
  Anchors.setP Anchors.branches newBranches
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
  Anchors.modP Anchors.branches (++ [viewPair])
  setCurrentBranch view newBranch
  return . FocusDelegator.delegatingId $
    WidgetIds.fromIRef textEditModelIRef

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

viewToDb
  :: Monad m => View
  -> Transaction ViewTag (Transaction DBTag m) a
  -> Transaction DBTag m a
viewToDb = Transaction.run . Anchors.viewStore

type CachedITrans t versionCache m =
  WriterT (Last versionCache) (ITransaction t m)

itrans :: Monad m => Transaction t m a -> CachedITrans t versionCache m a
itrans = lift . IT.transaction

tellNewCache
  :: MonadF m
  => Transaction DBTag m versionCache
  -> CachedITrans DBTag versionCache m a
  -> CachedITrans DBTag versionCache m a
tellNewCache mkCache act = act <* do
  newCache <- itrans mkCache
  Writer.tell $ Last (Just newCache)

makeRootWidget
  :: MonadF m
  => Transaction ViewTag (Transaction DBTag m) versionCache
  -> TWidget ViewTag (Transaction DBTag m)
  -> OTransaction DBTag m (Widget (CachedITrans DBTag versionCache m))
makeRootWidget mkCacheInView widget = do
  view <- OT.getP Anchors.view
  let
    toDb = viewToDb view
    mkCache = toDb mkCacheInView
  namedBranches <- OT.getP Anchors.branches
  viewEdit <- makeWidgetForView mkCache view widget
  currentBranch <- OT.getP Anchors.currentBranch

  let
    withNewCache = tellNewCache mkCache
    makeBranchNameEdit (textEditModelIRef, branch) = do
      let branchEditId = WidgetIds.fromIRef textEditModelIRef
      nameProp <- OT.transaction $ Transaction.fromIRef textEditModelIRef
      branchNameEdit <-
        BWidgets.wrapDelegated branchNameFDConfig
        FocusDelegator.NotDelegating id
        (BWidgets.makeLineEdit nameProp)
        branchEditId
      let
        setBranch action = withNewCache $ do
          itrans $ setCurrentBranch view branch
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
        withNewCache . itrans $ deleteCurrentBranch view

  branchSelectorFocused <-
    liftM isJust $ OT.subCursor WidgetIds.branchSelection
  branchSelector <-
    flip
    (BWidgets.wrapDelegated
     branchSelectionFocusDelegatorConfig
     FocusDelegator.NotDelegating id)
    WidgetIds.branchSelection $ \innerId ->
    OT.assignCursor innerId currentBranchWidgetId $ do
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
        itrans $ makeBranch view
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
  => Transaction DBTag m versionCache
  -> View
  -> TWidget ViewTag (Transaction DBTag m)
  -> OTransaction DBTag m (Widget (CachedITrans DBTag versionCache m))
makeWidgetForView mkCache view innerWidget = do
  curVersion <- OT.transaction $ View.curVersion view
  curVersionData <- OT.transaction $ Version.versionData curVersion
  redos <- OT.getP Anchors.redos
  cursor <- OT.readCursor

  let
    redo version newRedos = do
      Anchors.setP Anchors.redos newRedos
      View.move view version
      toDb $ Anchors.getP Anchors.postCursor
    undo parentVersion = do
      preCursor <- toDb $ Anchors.getP Anchors.preCursor
      View.move view parentVersion
      Anchors.modP Anchors.redos (curVersion:)
      return preCursor

    afterEvent action = do
      eventResult <- action
      IT.transaction $ do
        isEmpty <- Transaction.isEmpty
        unless isEmpty $ do
          Anchors.setP Anchors.preCursor cursor
          Anchors.setP Anchors.postCursor . fromMaybe cursor $ Widget.eCursor eventResult
      return eventResult

  vWidget <-
    OT.unWrapInner toDb $
    (liftM . Widget.atEvents) afterEvent innerWidget

  let
    runTrans = itrans . toDb . IT.runITransaction
    lowerWTransaction act = do
      (r, isEmpty) <- runTrans . liftM2 (,) act $ IT.transaction Transaction.isEmpty
      newCache <-
        if isEmpty
        then return Nothing
        else liftM Just . itrans $ do
          Anchors.setP Anchors.redos []
          mkCache
      Writer.tell $ Last newCache
      return r

    redoEventMap [] = mempty
    redoEventMap (version:restRedos) =
      Widget.keysEventMapMovesCursor Config.redoKeys "Redo" $
      redo version restRedos
    undoEventMap =
      maybe mempty
      (Widget.keysEventMapMovesCursor Config.undoKeys "Undo" .
       undo) $ Version.parent curVersionData

    eventMap = fmap (tellNewCache mkCache . itrans) $ mconcat [undoEventMap, redoEventMap redos]

  return .
    Widget.strongerEvents eventMap $
    Widget.atEvents lowerWTransaction vWidget
  where
    toDb = viewToDb view
