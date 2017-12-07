{-# LANGUAGE NoImplicitPrelude #-}
module Lamdu.VersionControl
    ( makeActions, runAction, runEvent, getVersion
    ) where

import qualified Control.Lens as Lens
import           Data.List (elemIndex)
import           Data.List.Utils (removeAt)
import           Data.Maybe.Utils (unsafeUnjust)
import           Data.Store.Rev.Branch (Branch)
import qualified Data.Store.Rev.Branch as Branch
import           Data.Store.Rev.Version (Version)
import qualified Data.Store.Rev.Version as Version
import           Data.Store.Rev.View (View)
import qualified Data.Store.Rev.View as View
import           Data.Store.Transaction (Transaction, setP, getP, modP)
import qualified Data.Store.Transaction as Transaction
import           GUI.Momentu.State (GUIState)
import qualified GUI.Momentu.State as GuiState
import           Lamdu.Data.DbLayout (DbM)
import qualified Lamdu.Data.DbLayout as DbLayout
import           Lamdu.VersionControl.Actions (Actions(Actions))
import qualified Lamdu.VersionControl.Actions as Actions

import           Lamdu.Prelude

-- TODO: Use the monad newtypes:
type TDB = Transaction DbM
type TV = Transaction DbLayout.ViewM

revProp :: (DbLayout.RevisionProps -> a) -> a
revProp x = x DbLayout.revisionProps

codeProp :: (DbLayout.CodeAnchors -> a) -> a
codeProp x = x DbLayout.codeAnchors

setCurrentBranch :: View DbM -> Branch DbM -> TDB ()
setCurrentBranch view branch = do
    setP (revProp DbLayout.currentBranch) branch
    View.setBranch view branch

deleteBranch :: View DbM -> [Branch DbM] -> Branch DbM -> TDB (Branch DbM)
deleteBranch view branches branch = do
    setP (revProp DbLayout.branches) newBranches
    setCurrentBranch view newBranch
    return newBranch
    where
        newBranch = newBranches !! min (length newBranches - 1) index
        index =
            unsafeUnjust "Invalid current branch!" $
            elemIndex branch branches
        newBranches = removeAt index branches

makeBranch :: View DbM -> TDB (Branch DbM)
makeBranch view = do
    newBranch <- Branch.new =<< View.curVersion view
    modP (revProp DbLayout.branches) (++ [newBranch])
    setCurrentBranch view newBranch
    return newBranch

runAction :: TV a -> TDB a
runAction action = do
    view <- getP $ revProp DbLayout.view
    DbLayout.runViewTransaction view action

getVersion :: TDB (Version DbM)
getVersion =
    do
        currentBranch <- getP $ revProp DbLayout.currentBranch
        Branch.curVersion currentBranch

runEvent :: Traversable t => GUIState -> TV (t GuiState.Update) -> TDB (t GuiState.Update)
runEvent preGuiState eventHandler = do
    (eventResult, isEmpty) <- runAction $ do
        eventResult <- eventHandler
        isEmpty <- Transaction.isEmpty
        unless isEmpty $ do
            setP (codeProp DbLayout.preGuiState) preGuiState
            preGuiState
                & GuiState.update (eventResult ^. Lens.traversed)
                & setP (codeProp DbLayout.postGuiState)
        return (eventResult, isEmpty)
    unless isEmpty $ setP (revProp DbLayout.redos) []
    return eventResult

makeActions :: Transaction DbM (Actions DbM (Transaction DbM))
makeActions = do
    view <- getP $ revProp DbLayout.view
    branches <- getP $ revProp DbLayout.branches
    currentBranch <- getP $ revProp DbLayout.currentBranch
    curVersion <- View.curVersion view
    curVersionData <- Version.versionData curVersion
    allRedos <- getP $ revProp DbLayout.redos
    let toDb = DbLayout.runViewTransaction view
        undo parentVersion = do
            preGuiState <- toDb . getP $ codeProp DbLayout.preGuiState
            View.move view parentVersion
            modP (revProp DbLayout.redos) (curVersion :)
            return preGuiState
        mkRedo [] = Nothing
        mkRedo (redo : redos) = Just $ do
            setP (revProp DbLayout.redos) redos
            View.move view redo
            toDb . getP $ codeProp DbLayout.postGuiState
    return Actions
        { Actions.branches = branches
        , Actions.currentBranch = currentBranch
        , Actions.setCurrentBranch = setCurrentBranch view
        , Actions.deleteBranch = deleteBranch view branches
        , Actions.makeBranch = makeBranch view
        , Actions.mUndo = undo <$> Version.parent curVersionData
        , Actions.mRedo = mkRedo allRedos
        }
