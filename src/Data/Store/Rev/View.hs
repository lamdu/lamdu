{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Store.Rev.View
    (View, curVersion, branch, setBranch, move, new, store)
where

import Control.Monad (liftM, (<=<))
import Data.Store.Property (composeLabel)
import Data.Store.Rev.Branch (Branch)
import Data.Store.Rev.Change(Change(..))
import Data.Store.Rev.Version (Version)
import Data.Store.Rev.ViewBranchInternal (ViewData(..), View(..), Branch(..), moveView, makeViewKey, applyChangesToView, brViews, vdBranch)
import Data.Store.Transaction (Transaction, Store(..))
import Prelude hiding (lookup)
import qualified Data.List as List
import qualified Data.Record.Label as Label
import qualified Data.Store.Property as Property
import qualified Data.Store.Rev.Branch as Branch
import qualified Data.Store.Rev.Change as Change
import qualified Data.Store.Rev.Version as Version
import qualified Data.Store.Transaction as Transaction

-- | A Version Map is a large mapping of ObjectKeys to their
-- | "current-version" values. This serves as a "cache" which is
-- | redundant to the lists of changes stored in the version graph.

lookupBS :: Monad m => View -> Change.Key -> Transaction t m (Maybe Change.Value)
lookupBS view = Transaction.lookupBS . makeViewKey view

setBranch :: Monad m => View -> Branch -> Transaction t m ()
setBranch view@(View viewDataIRef) newBranch@(Branch newBranchDataIRef) = do
  let branchRef = vdBranch `composeLabel` Transaction.fromIRef viewDataIRef
  oldBranch@(Branch oldBranchDataIRef) <- Property.get branchRef
  oldVersion <- Branch.curVersion oldBranch
  newVersion <- Branch.curVersion newBranch
  moveView view oldVersion newVersion
  Property.set branchRef newBranch
  Property.pureModify (brViews `composeLabel` Transaction.fromIRef oldBranchDataIRef) (List.delete view)
  Property.pureModify (brViews `composeLabel` Transaction.fromIRef newBranchDataIRef) (view:)

new :: Monad m => Branch -> Transaction t m View
new br@(Branch branchDataIRef) = do
  view <- View `liftM` Transaction.newIRef (ViewData br)
  version <- Branch.curVersion br
  applyHistory view =<< Version.versionData version
  Property.pureModify (brViews `composeLabel` Transaction.fromIRef branchDataIRef) (view:)
  return view
  where
    applyHistory view versionData = do
      maybe (return ()) (applyHistory view <=< Version.versionData) . Version.parent $ versionData
      applyChangesToView view Change.newValue $ Version.changes versionData

curVersion :: Monad m => View -> Transaction t m Version
curVersion = Branch.curVersion <=< branch

move :: Monad m => View -> Version -> Transaction t m ()
move view version = (`Branch.move` version) =<< branch view

branch :: Monad m => View -> Transaction t m Branch
branch (View iref) = liftM (Label.getL vdBranch) . Transaction.readIRef $ iref

transaction :: Monad m => View -> [(Change.Key, Maybe Change.Value)] -> Transaction t m ()
transaction _    [] = return ()
transaction view changes = do
  b <- branch view
  Branch.newVersion b =<< mapM makeChange changes
  where
    makeChange (key, value) =
      flip (Change key) value `liftM` lookupBS view key

-- You get a store tagged however you like...
store :: Monad m => View -> Store t' (Transaction t m)
store view = Store {
  storeNewKey = Transaction.newKey,
  storeLookup = lookupBS view,
  storeAtomicWrite = transaction view
  }
