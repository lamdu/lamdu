{-# LANGUAGE NoImplicitPrelude #-}
module Data.Store.Rev.View
    ( View, curVersion, branch, setBranch, move, new, store
    ) where

import           Prelude.Compat hiding (lookup)

import           Control.Lens.Operators
import           Control.Monad ((<=<), guard, unless)
import           Control.MonadA (MonadA)
import qualified Data.List as List
import           Data.Maybe (catMaybes)
import           Data.Store.IRef (IRef)
import qualified Data.Store.Property as Property
import           Data.Store.Rev.Branch (Branch)
import qualified Data.Store.Rev.Branch as Branch
import qualified Data.Store.Rev.Change as Change
import           Data.Store.Rev.Change (Change(..))
import           Data.Store.Rev.Version (Version)
import qualified Data.Store.Rev.Version as Version
import           Data.Store.Rev.ViewBranchInternal (BranchData, ViewData(..), View(..), Branch(..), moveView, makeViewKey, applyChangesToView, brViews, vdBranch)
import           Data.Store.Transaction (Transaction, Store(..))
import qualified Data.Store.Transaction as Transaction

-- | A Version Map is a large mapping of ObjectKeys to their
-- | "current-version" values. This serves as a "cache" which is
-- | redundant to the lists of changes stored in the version graph.

lookupBS :: MonadA m => View m -> Change.Key -> Transaction m (Maybe Change.Value)
lookupBS view = Transaction.lookupBS . makeViewKey view

setBranch :: MonadA m => View m -> Branch m -> Transaction m ()
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
    :: MonadA m
    => IRef m (BranchData m)
    -> ([View m] -> [View m]) -> Transaction m ()
modifyViews iref f = do
    prop <- Transaction.fromIRef iref
    Property.pureModify (Property.composeLens brViews prop) f

new :: MonadA m => Branch m -> Transaction m (View m)
new br@(Branch branchDataIRef) = do
    view <- View `fmap` Transaction.newIRef (ViewData br)
    version <- Branch.curVersion br
    applyHistory view =<< Version.versionData version
    modifyViews branchDataIRef (view:)
    return view
    where
        applyHistory view versionData = do
            maybe (return ()) (applyHistory view <=< Version.versionData) . Version.parent $ versionData
            applyChangesToView view Change.newValue $ Version.changes versionData

curVersion :: MonadA m => View m -> Transaction m (Version m)
curVersion = Branch.curVersion <=< branch

move :: MonadA m => View m -> Version m -> Transaction m ()
move view version = (`Branch.move` version) =<< branch view

branch :: MonadA m => View m -> Transaction m (Branch m)
branch (View iref) = (^. vdBranch) <$> Transaction.readIRef iref

transaction :: MonadA m => View m -> [(Change.Key, Maybe Change.Value)] -> Transaction m ()
transaction view updates = do
    changes <- catMaybes <$> traverse makeChange updates
    unless (null changes) $ do
        b <- branch view
        Branch.newVersion b changes 
    where
        makeChange (key, value) = do
            prev <- lookupBS view key
            return $ Change key prev value <$ guard (value /= prev)

-- You get a store tagged however you like...
store :: MonadA m => View m -> Store (Transaction m)
store view =
    Store
    { storeNewKey = Transaction.newKey
    , storeLookup = lookupBS view
    , storeAtomicWrite = transaction view
    }
