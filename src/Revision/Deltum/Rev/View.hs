module Revision.Deltum.Rev.View
    ( View, curVersion, branch, setBranch, move, new, store
    ) where

import           Control.Monad ((<=<))
import qualified Data.List as List
import           Data.Maybe (catMaybes)
import qualified Data.Property as Property
import           Revision.Deltum.IRef (IRef)
import           Revision.Deltum.Rev.Branch (Branch)
import qualified Revision.Deltum.Rev.Branch as Branch
import           Revision.Deltum.Rev.Change (Change(..))
import qualified Revision.Deltum.Rev.Change as Change
import           Revision.Deltum.Rev.Version (Version)
import qualified Revision.Deltum.Rev.Version as Version
import           Revision.Deltum.Rev.ViewBranchInternal (BranchData, ViewData(..), View(..), Branch(..), moveView, makeViewKey, applyChangesToView, brViews, vdBranch)
import           Revision.Deltum.Transaction (Transaction, Store(..))
import qualified Revision.Deltum.Transaction as Transaction

import           Lamdu.Prelude

-- | A Version Map is a large mapping of ObjectKeys to their
-- | "current-version" values. This serves as a "cache" which is
-- | redundant to the lists of changes stored in the version graph.

type T = Transaction

lookupBS :: Monad m => View m -> Change.Key -> T m (Maybe Change.Value)
lookupBS view = Transaction.lookupBS . makeViewKey view

setBranch :: Monad m => View m -> Branch m -> T m ()
setBranch view@(View viewDataIRef) newBranch@(Branch newBranchDataIRef) = do
    branchRef <-
        Property.composeLens vdBranch <$>
        Transaction.fromIRef viewDataIRef
    let oldBranch@(Branch oldBranchDataIRef) = Property.value branchRef
    oldVersion <- Branch.curVersion oldBranch
    newVersion <- Branch.curVersion newBranch
    moveView view oldVersion newVersion
    Property.set branchRef newBranch
    modifyViews oldBranchDataIRef $ List.delete view
    modifyViews newBranchDataIRef (view:)

modifyViews
    :: Monad m
    => IRef m (BranchData m)
    -> ([View m] -> [View m]) -> T m ()
modifyViews iref f = do
    prop <- Transaction.fromIRef iref
    Property.pureModify (Property.composeLens brViews prop) f

new :: Monad m => Branch m -> T m (View m)
new br@(Branch branchDataIRef) = do
    view <- View `fmap` Transaction.newIRef (ViewData br)
    version <- Branch.curVersion br
    applyHistory view =<< Version.versionData version
    modifyViews branchDataIRef (view:)
    pure view
    where
        applyHistory view versionData = do
            maybe (pure ()) (applyHistory view <=< Version.versionData) . Version.parent $ versionData
            applyChangesToView view Change.newValue $ Version.changes versionData

curVersion :: Monad m => View m -> T m (Version m)
curVersion = Branch.curVersion <=< branch

move :: Monad m => View m -> Version m -> T m ()
move view version = (`Branch.move` version) =<< branch view

branch :: Monad m => View m -> T m (Branch m)
branch (View iref) = (^. vdBranch) <$> Transaction.readIRef iref

transaction :: Monad m => View m -> [(Change.Key, Maybe Change.Value)] -> T m ()
transaction view updates = do
    changes <- catMaybes <$> traverse makeChange updates
    unless (null changes) $ do
        b <- branch view
        Branch.newVersion b changes
    where
        makeChange (key, value) =
            lookupBS view key <&> \prev ->
            Change key prev value <$ guard (value /= prev)

-- You get a store tagged however you like...
store :: Monad m => View m -> Store (T m)
store view =
    Store
    { storeNewKey = Transaction.newKey
    , storeLookup = lookupBS view
    , storeAtomicWrite = transaction view
    }
