module Lamdu.VersionControl (makeActions, runAction, runEvent) where

import Control.Lens.Operators
import Control.Monad (unless)
import Data.List (elemIndex)
import Data.List.Utils (removeAt)
import Data.Maybe (fromMaybe)
import Data.Store.IRef (Tag)
import Data.Store.Rev.Branch (Branch)
import Data.Store.Rev.View (View)
import Data.Store.Transaction (Transaction, setP, getP, modP)
import Lamdu.Data.DbLayout (DbM)
import Lamdu.VersionControl.Actions (Actions(Actions))
import qualified Data.Monoid as Monoid
import qualified Data.Store.Rev.Branch as Branch
import qualified Data.Store.Rev.Version as Version
import qualified Data.Store.Rev.View as View
import qualified Data.Store.Transaction as Transaction
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Lamdu.Data.DbLayout as DbLayout
import qualified Lamdu.VersionControl.Actions as Actions

-- TODO: Use the monad newtypes:
type TDB = Transaction DbM
type TV = Transaction DbLayout.ViewM

revProp :: (DbLayout.RevisionProps -> a) -> a
revProp x = x DbLayout.revisionProps

codeProp :: (DbLayout.CodeProps -> a) -> a
codeProp x = x DbLayout.codeProps

setCurrentBranch :: View (Tag DbM) -> Branch (Tag DbM) -> TDB ()
setCurrentBranch view branch = do
  setP (revProp DbLayout.currentBranch) branch
  View.setBranch view branch

deleteBranch :: View (Tag DbM) -> [Branch (Tag DbM)] -> Branch (Tag DbM) -> TDB (Branch (Tag DbM))
deleteBranch view branches branch = do
  setP (revProp DbLayout.branches) newBranches
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
  modP (revProp DbLayout.branches) (++ [newBranch])
  setCurrentBranch view newBranch
  return newBranch

runAction :: TV a -> TDB a
runAction action = do
  view <- getP $ revProp DbLayout.view
  DbLayout.runViewTransaction view action

runEvent :: Widget.Id -> TV Widget.EventResult -> TDB Widget.EventResult
runEvent preCursor eventHandler = do
  (eventResult, isEmpty) <- runAction $ do
    eventResult <- eventHandler
    isEmpty <- Transaction.isEmpty
    unless isEmpty $ do
      setP (codeProp DbLayout.preCursor) preCursor
      setP (codeProp DbLayout.postCursor) .
        fromMaybe preCursor . Monoid.getLast $ eventResult ^. Widget.eCursor
    return (eventResult, isEmpty)
  unless isEmpty $ setP (revProp DbLayout.redos) []
  return eventResult

makeActions :: Transaction DbM (Actions (Tag DbM) (Transaction DbM))
makeActions = do
  view <- getP $ revProp DbLayout.view
  branches <- getP $ revProp DbLayout.branches
  currentBranch <- getP $ revProp DbLayout.currentBranch
  curVersion <- View.curVersion view
  curVersionData <- Version.versionData curVersion
  allRedos <- getP $ revProp DbLayout.redos
  let
    toDb = DbLayout.runViewTransaction view
    undo parentVersion = do
      preCursor <- toDb . getP $ codeProp DbLayout.preCursor
      View.move view parentVersion
      modP (revProp DbLayout.redos) (curVersion :)
      return preCursor
    mkRedo [] = Nothing
    mkRedo (redo : redos) = Just $ do
      setP (revProp DbLayout.redos) redos
      View.move view redo
      toDb . getP $ codeProp DbLayout.postCursor
  return Actions
    { Actions.branches = branches
    , Actions.currentBranch = currentBranch
    , Actions.setCurrentBranch = setCurrentBranch view
    , Actions.deleteBranch = deleteBranch view branches
    , Actions.makeBranch = makeBranch view
    , Actions.mUndo = fmap undo $ Version.parent curVersionData
    , Actions.mRedo = mkRedo allRedos
    }
