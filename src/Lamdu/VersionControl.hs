module Lamdu.VersionControl
    ( makeActions, runAction, runEvent, getVersion
    ) where

import qualified Control.Lens as Lens
import           Data.List.Extended (elemIndex, removeAt)
import           Data.Maybe.Extended (unsafeUnjust)
import           Data.Property (Property(..))
import qualified Data.Property as Property
import           GUI.Momentu (GUIState, Update)
import qualified GUI.Momentu.State as GuiState
import           Lamdu.Data.Db.Layout (DbM)
import qualified Lamdu.Data.Db.Layout as DbLayout
import           Lamdu.VersionControl.Actions (Actions(Actions))
import qualified Lamdu.VersionControl.Actions as Actions
import           Revision.Deltum.Rev.Branch (Branch)
import qualified Revision.Deltum.Rev.Branch as Branch
import           Revision.Deltum.Rev.Version (Version)
import qualified Revision.Deltum.Rev.Version as Version
import           Revision.Deltum.Rev.View (View)
import qualified Revision.Deltum.Rev.View as View
import           Revision.Deltum.Transaction (Transaction)
import qualified Revision.Deltum.Transaction as Transaction

import           Lamdu.Prelude

-- TODO: Use the monad newtypes:
type TDB = Transaction DbM
type TV = Transaction DbLayout.ViewM

revProp :: (DbLayout.RevisionProps -> a) -> a
revProp x = x DbLayout.revisionProps

guiProp :: (DbLayout.GuiAnchors -> a) -> a
guiProp x = x DbLayout.guiAnchors

setCurrentBranch :: View DbM -> Branch DbM -> TDB ()
setCurrentBranch view branch = do
    Property.setP (revProp DbLayout.currentBranch) branch
    View.setBranch view branch

deleteBranch :: View DbM -> [Branch DbM] -> Branch DbM -> TDB (Branch DbM)
deleteBranch view branches branch = do
    Property.setP (revProp DbLayout.branches) newBranches
    setCurrentBranch view newBranch
    pure newBranch
    where
        newBranch = newBranches !! min (length newBranches - 1) index
        index =
            unsafeUnjust "Invalid current branch!" $
            elemIndex branch branches
        newBranches = removeAt index branches

makeBranch :: View DbM -> TDB (Branch DbM)
makeBranch view = do
    newBranch <- Branch.new =<< View.curVersion view
    Property.modP (revProp DbLayout.branches) (++ [newBranch])
    setCurrentBranch view newBranch
    pure newBranch

runAction :: TV a -> TDB a
runAction action = do
    view <- Property.getP $ revProp DbLayout.view
    DbLayout.runViewTransaction view action

getVersion :: TDB (Version DbM)
getVersion =
    do
        currentBranch <- Property.getP $ revProp DbLayout.currentBranch
        Branch.curVersion currentBranch

runEvent ::
    Traversable t =>
    GUIState -> TV (t Update) -> TDB (t Update)
runEvent preGuiState eventHandler = do
    (eventResult, isEmpty) <- runAction $ do
        eventResult <- eventHandler
        isEmpty <- Transaction.isEmpty
        unless isEmpty $ do
            Property.setP (guiProp DbLayout.preGuiState) preGuiState
            preGuiState
                & GuiState.update (eventResult ^. Lens.traversed)
                & Property.setP (guiProp DbLayout.postGuiState)
        pure (eventResult, isEmpty)
    unless isEmpty $ Property.setP (revProp DbLayout.redos) []
    pure eventResult

makeActions :: Transaction DbM (Actions DbM (Transaction DbM))
makeActions = do
    view <- Property.getP $ revProp DbLayout.view
    branches <- Property.getP $ revProp DbLayout.branches
    currentBranch <- Property.getP $ revProp DbLayout.currentBranch
    curVersion <- View.curVersion view
    curVersionData <- Version.versionData curVersion
    allRedos <- Property.getP $ revProp DbLayout.redos
    let toDb = DbLayout.runViewTransaction view
        undo parentVersion = do
            preGuiState <- toDb . Property.getP $ guiProp DbLayout.preGuiState
            View.move view parentVersion
            Property.modP (revProp DbLayout.redos) (curVersion :)
            pure preGuiState
        mkRedo [] = Nothing
        mkRedo (redo : redos) = Just $ do
            Property.setP (revProp DbLayout.redos) redos
            View.move view redo
            toDb . Property.getP $ guiProp DbLayout.postGuiState
    pure Actions
        { Actions.branches = branches
        , Actions.currentBranch = Property currentBranch (setCurrentBranch view)
        , Actions.deleteBranch = deleteBranch view branches
        , Actions.makeBranch = makeBranch view
        , Actions.mUndo = undo <$> Version.parent curVersionData
        , Actions.mRedo = mkRedo allRedos
        }
