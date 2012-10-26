{-# LANGUAGE TypeOperators #-}
module Editor.BranchGUI
  ( makeRootWidget
  , CachedITrans, CachedTWidget
  ) where

import Control.Applicative (pure, (<*))
import Control.Arrow (first)
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
import Editor.WidgetEnvT (WidgetEnvT)
import Graphics.UI.Bottle.Animation (AnimId)
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
import qualified Editor.Layers as Layers
import qualified Editor.WidgetEnvT as WE
import qualified Editor.WidgetIds as WidgetIds
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Box as Box
import qualified Graphics.UI.Bottle.Widgets.Edges as Edges
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator

type TDB = Transaction DBTag
type TV m = Transaction ViewTag (TDB m)
type ITV m = ITransaction ViewTag (TDB m)

setCurrentBranch :: Monad m => View -> Branch -> TDB m ()
setCurrentBranch view branch = do
  Anchors.setP Anchors.currentBranch branch
  View.setBranch view branch

deleteCurrentBranch :: Monad m => View -> TDB m Widget.Id
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

makeBranch :: Monad m => View -> TDB m Widget.Id
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
  -> TV m a
  -> TDB m a
viewToDb = Transaction.run . Anchors.viewStore

type CachedITrans t versionCache m =
  WriterT (Last versionCache) (ITransaction t m)

type CachedTWidget versionCache m =
  WidgetEnvT (TDB m) (Widget (CachedITrans DBTag versionCache m))

itrans :: Monad m => Transaction t m a -> CachedITrans t versionCache m a
itrans = lift . IT.transaction

tellNewCache
  :: MonadF m
  => TDB m versionCache
  -> CachedITrans DBTag versionCache m a
  -> CachedITrans DBTag versionCache m a
tellNewCache mkCache act = act <* do
  newCache <- itrans mkCache
  Writer.tell $ Last (Just newCache)

makeRootWidget
  :: MonadF m
  => Widget.Size -> TV m versionCache
  -> WidgetEnvT (TV m) (Widget (ITV m))
  -> CachedTWidget versionCache m
makeRootWidget size mkCacheInView widget = do
  view <- WE.getP Anchors.view
  let
    toDb = viewToDb view
    mkCache = toDb mkCacheInView
  namedBranches <- WE.getP Anchors.branches
  viewEdit <- makeWidgetForView mkCache view widget
  currentBranch <- WE.getP Anchors.currentBranch

  let
    withNewCache = tellNewCache mkCache
    makeBranchNameEdit (textEditModelIRef, branch) = do
      let branchEditId = WidgetIds.fromIRef textEditModelIRef
      nameProp <-
        lift $ Transaction.fromIRef textEditModelIRef
      branchNameEdit <-
        BWidgets.wrapDelegatedOT branchNameFDConfig
        FocusDelegator.NotDelegating id
        (BWidgets.makeLineEdit nameProp)
        branchEditId
      let
        setBranch action = withNewCache $ do
          itrans $ setCurrentBranch view branch
          action
      return
        ( branch
        , (Widget.atWMaybeEnter . fmap . fmap . Widget.atEnterResultEvent) setBranch .
          Widget.atEvents (lift . IT.transaction) $ branchNameEdit
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
    liftM isJust $ WE.subCursor WidgetIds.branchSelection
  branchSelector <-
    flip
    (BWidgets.wrapDelegatedOT
     branchSelectionFocusDelegatorConfig
     FocusDelegator.NotDelegating id)
    WidgetIds.branchSelection $ \innerId ->
    WE.assignCursor innerId currentBranchWidgetId $ do
      branchNameEdits <- mapM makeBranchNameEdit namedBranches
      return .
        Widget.strongerEvents delBranchEventMap $
        makeBranchChoice branchSelectorFocused
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
    Widget.strongerEvents eventMap $
    Edges.makeVertical size viewEdit branchSelector

makeBranchChoice
  :: Eq a
  => Bool -> AnimId
  -> Box.Orientation
  -> [(a, Widget f)]
  -> a
  -> Widget f
makeBranchChoice forceExpand selectionAnimId orientation children curChild =
  maybe Box.toWidget Box.toWidgetBiased mCurChildIndex box
  where
    childFocused = any (Widget.wIsFocused . snd) children
    pairs = (map . first) (curChild ==) children
    visiblePairs
      | childFocused || forceExpand = pairs
      | otherwise = filter fst pairs
    mCurChildIndex = findIndex fst visiblePairs
    box = Box.makeAlign 0 orientation colorizedPairs
    colorizedPairs
      -- focus shows selection already
      | childFocused = map snd visiblePairs
      -- need to show selection even as focus is elsewhere
      | otherwise = map colorize visiblePairs
      where
        colorize (True, w) = Widget.backgroundColor Layers.branchChoice selectionAnimId selectedColor w
        colorize (False, w) = w
        selectedColor = Config.selectedBranchColor

-- Apply the transactions to the given View and convert them to
-- transactions on a DB
makeWidgetForView
  :: MonadF m
  => TDB m versionCache
  -> View
  -> WidgetEnvT (TV m) (Widget (ITV m))
  -> CachedTWidget versionCache m
makeWidgetForView mkCache view innerWidget = do
  curVersion <- lift $ View.curVersion view
  curVersionData <- lift $ Version.versionData curVersion
  redos <- WE.getP Anchors.redos
  cursor <- WE.readCursor

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
    WE.unWrapInner toDb $
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
