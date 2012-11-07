module Editor.VersionControl (makeActions, runAction, runEvent) where

import Control.Monad (unless)
import Data.List (findIndex)
import Data.List.Utils (removeAt)
import Data.Maybe (fromMaybe)
import Data.Store.Rev.Branch (Branch)
import Data.Store.Rev.View (View)
import Data.Store.Transaction (Transaction)
import Editor.Anchors (ViewTag, DBTag)
import Editor.VersionControl.Actions (Actions(Actions))
import qualified Data.Store.Rev.Branch as Branch
import qualified Data.Store.Rev.Version as Version
import qualified Data.Store.Rev.View as View
import qualified Data.Store.Transaction as Transaction
import qualified Editor.Anchors as Anchors
import qualified Editor.VersionControl.Actions as Actions
import qualified Graphics.UI.Bottle.Widget as Widget

type TDB = Transaction DBTag
type TV m = Transaction ViewTag (TDB m)

setCurrentBranch :: Monad m => View -> Branch -> TDB m ()
setCurrentBranch view branch = do
  Anchors.setP Anchors.currentBranch branch
  View.setBranch view branch

deleteBranch ::
  Monad m => View -> [Branch] -> Branch ->
  TDB m Branch
deleteBranch view branches branch = do
  Anchors.setP Anchors.branches newBranches
  setCurrentBranch view newBranch
  return newBranch
  where
    newBranch = newBranches !! min (length newBranches - 1) index
    index =
      fromMaybe (error "Invalid current branch!") $
      findIndex (branch ==) branches
    newBranches = removeAt index branches

makeBranch :: Monad m => View -> TDB m Branch
makeBranch view = do
  newBranch <- Branch.new =<< View.curVersion view
  Anchors.modP Anchors.branches (++ [newBranch])
  setCurrentBranch view newBranch
  return newBranch

runAction :: Monad m => TV m a -> TDB m a
runAction action = do
  view <- Anchors.getP Anchors.view
  Transaction.run (Anchors.viewStore view) action

runEvent ::
  Monad m => Widget.Id -> TV m Widget.EventResult -> TDB m Widget.EventResult
runEvent preCursor eventHandler = do
  (eventResult, isEmpty) <- runAction $ do
    eventResult <- eventHandler
    isEmpty <- Transaction.isEmpty
    unless isEmpty $ do
      Anchors.setP Anchors.preCursor preCursor
      Anchors.setP Anchors.postCursor . fromMaybe preCursor $ Widget.eCursor eventResult
    return (eventResult, isEmpty)
  unless isEmpty $ Anchors.setP Anchors.redos []
  return eventResult

makeActions :: Monad m => Transaction DBTag m (Actions (Transaction DBTag m))
makeActions = do
  view <- Anchors.getP Anchors.view
  branches <- Anchors.getP Anchors.branches
  currentBranch <- Anchors.getP Anchors.currentBranch
  curVersion <- View.curVersion view
  curVersionData <- Version.versionData curVersion
  allRedos <- Anchors.getP Anchors.redos
  let
    toDb = Transaction.run (Anchors.viewStore view)
    undo parentVersion = do
      preCursor <- toDb $ Anchors.getP Anchors.preCursor
      View.move view parentVersion
      Anchors.modP Anchors.redos (curVersion :)
      return preCursor
    mkRedo [] = Nothing
    mkRedo (redo : redos) = Just $ do
      Anchors.setP Anchors.redos redos
      View.move view redo
      toDb $ Anchors.getP Anchors.postCursor
  return Actions
    { Actions.branches = branches
    , Actions.currentBranch = currentBranch
    , Actions.setCurrentBranch = setCurrentBranch view
    , Actions.deleteBranch = deleteBranch view branches
    , Actions.makeBranch = makeBranch view
    , Actions.mUndo = fmap undo $ Version.parent curVersionData
    , Actions.mRedo = mkRedo allRedos
    }
