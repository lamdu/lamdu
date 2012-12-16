module Lamdu.VersionControl (makeActions, runAction, runEvent) where

import Control.Monad (unless)
import Data.List (elemIndex)
import Data.List.Utils (removeAt)
import Data.Maybe (fromMaybe)
import Data.Store.IRef (Tag)
import Data.Store.Rev.Branch (Branch)
import Data.Store.Rev.View (View)
import Data.Store.Transaction (Transaction)
import Lamdu.Anchors (DbM)
import Lamdu.VersionControl.Actions (Actions(Actions))
import qualified Control.Lens as Lens
import qualified Data.Store.Rev.Branch as Branch
import qualified Data.Store.Rev.Version as Version
import qualified Data.Store.Rev.View as View
import qualified Data.Store.Transaction as Transaction
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Lamdu.Anchors as Anchors
import qualified Lamdu.VersionControl.Actions as Actions

-- TODO: Use the monad newtypes:
type TDB = Transaction DbM
type TV = Transaction Anchors.ViewM

setCurrentBranch :: View (Tag DbM) -> Branch (Tag DbM) -> TDB ()
setCurrentBranch view branch = do
  Anchors.setP Anchors.currentBranch branch
  View.setBranch view branch

deleteBranch :: View (Tag DbM) -> [Branch (Tag DbM)] -> Branch (Tag DbM) -> TDB (Branch (Tag DbM))
deleteBranch view branches branch = do
  Anchors.setP Anchors.branches newBranches
  setCurrentBranch view newBranch
  return newBranch
  where
    newBranch = newBranches !! min (length newBranches - 1) index
    index =
      fromMaybe (error "Invalid current branch!") $
      elemIndex branch branches
    newBranches = removeAt index branches

makeBranch :: View (Tag DbM) -> TDB (Branch (Tag DbM))
makeBranch view = do
  newBranch <- Branch.new =<< View.curVersion view
  Anchors.modP Anchors.branches (++ [newBranch])
  setCurrentBranch view newBranch
  return newBranch

runAction :: TV a -> TDB a
runAction action = do
  view <- Anchors.getP Anchors.view
  Anchors.runViewTransaction view action

runEvent :: Widget.Id -> TV Widget.EventResult -> TDB Widget.EventResult
runEvent preCursor eventHandler = do
  (eventResult, isEmpty) <- runAction $ do
    eventResult <- eventHandler
    isEmpty <- Transaction.isEmpty
    unless isEmpty $ do
      Anchors.setP Anchors.preCursor preCursor
      Anchors.setP Anchors.postCursor . fromMaybe preCursor $ Lens.view Widget.eCursor eventResult
    return (eventResult, isEmpty)
  unless isEmpty $ Anchors.setP Anchors.redos []
  return eventResult

makeActions :: Transaction DbM (Actions (Tag DbM) (Transaction DbM))
makeActions = do
  view <- Anchors.getP Anchors.view
  branches <- Anchors.getP Anchors.branches
  currentBranch <- Anchors.getP Anchors.currentBranch
  curVersion <- View.curVersion view
  curVersionData <- Version.versionData curVersion
  allRedos <- Anchors.getP Anchors.redos
  let
    toDb = Anchors.runViewTransaction view
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
