{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Store.Rev.View
    (View, curVersion, branch, setBranch, move, new, store)
where

import Control.Applicative ((<$>), (<$))
import Control.Lens.Operators
import Control.Monad ((<=<), guard, unless)
import Control.MonadA (MonadA)
import Data.Maybe (catMaybes)
import Data.Store.IRef (IRef, Tag)
import Data.Store.Rev.Branch (Branch)
import Data.Store.Rev.Change(Change(..))
import Data.Store.Rev.Version (Version)
import Data.Store.Rev.ViewBranchInternal (BranchData, ViewData(..), View(..), Branch(..), moveView, makeViewKey, applyChangesToView, brViews, vdBranch)
import Data.Store.Transaction (Transaction, Store(..))
import Data.Traversable (traverse)
import Prelude hiding (lookup)
import qualified Data.List as List
import qualified Data.Store.Property as Property
import qualified Data.Store.Rev.Branch as Branch
import qualified Data.Store.Rev.Change as Change
import qualified Data.Store.Rev.Version as Version
import qualified Data.Store.Transaction as Transaction

-- | A Version Map is a large mapping of ObjectKeys to their
-- | "current-version" values. This serves as a "cache" which is
-- | redundant to the lists of changes stored in the version graph.

lookupBS :: MonadA m => View (Tag m) -> Change.Key -> Transaction m (Maybe Change.Value)
lookupBS view = Transaction.lookupBS . makeViewKey view

setBranch :: MonadA m => View (Tag m) -> Branch (Tag m) -> Transaction m ()
setBranch view@(View viewDataIRef) newBranch@(Branch newBranchDataIRef) = do
  branchRef <-
    fmap (Property.composeLens vdBranch) $
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
  => IRef (Tag m) (BranchData (Tag m))
  -> ([View (Tag m)] -> [View (Tag m)]) -> Transaction m ()
modifyViews iref f = do
  prop <- Transaction.fromIRef iref
  Property.pureModify (Property.composeLens brViews prop) f

new :: MonadA m => Branch (Tag m) -> Transaction m (View (Tag m))
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

curVersion :: MonadA m => View (Tag m) -> Transaction m (Version (Tag m))
curVersion = Branch.curVersion <=< branch

move :: MonadA m => View (Tag m) -> Version (Tag m) -> Transaction m ()
move view version = (`Branch.move` version) =<< branch view

branch :: MonadA m => View (Tag m) -> Transaction m (Branch (Tag m))
branch (View iref) = (^. vdBranch) <$> Transaction.readIRef iref

transaction :: MonadA m => View (Tag m) -> [(Change.Key, Maybe Change.Value)] -> Transaction m ()
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
store :: MonadA m => View (Tag m) -> Store (Transaction m)
store view = Store {
  storeNewKey = Transaction.newKey,
  storeLookup = lookupBS view,
  storeAtomicWrite = transaction view
  }
