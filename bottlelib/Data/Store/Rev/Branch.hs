{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Store.Rev.Branch
    (Branch, guid, new, move, curVersion, newVersion)
where

import Control.Lens.Operators
import Control.MonadA (MonadA)
import Data.Foldable (traverse_)
import Data.Store.Guid (Guid)
import Data.Store.IRef (Tag)
import Data.Store.Rev.Change (Change)
import Data.Store.Rev.Version (Version)
import Data.Store.Rev.ViewBranchInternal (BranchData(..), Branch(..), moveView, brVersion)
import Data.Store.Transaction (Transaction)
import qualified Data.Store.IRef as IRef
import qualified Data.Store.Rev.Version as Version
import qualified Data.Store.Transaction as Transaction

guid :: Branch t -> Guid
guid = IRef.guid . unBranch

move :: MonadA m => Branch (Tag m) -> Version (Tag m) -> Transaction m ()
move (Branch dataIRef) destVersion = do
  BranchData srcVersion views <- Transaction.readIRef dataIRef
  traverse_ (moveToDest srcVersion) views
  Transaction.writeIRef dataIRef (BranchData destVersion views)
  where
    moveToDest srcVersion view = moveView view srcVersion destVersion

curVersion :: MonadA m => Branch (Tag m) -> Transaction m (Version (Tag m))
curVersion (Branch dataIRef) = (^. brVersion) `fmap` Transaction.readIRef dataIRef

-- | A Branch is a mutable version ptr
new :: MonadA m => Version (Tag m) -> Transaction m (Branch (Tag m))
new version = Branch `fmap`
              Transaction.newIRef (BranchData version [])

newVersion :: MonadA m => Branch (Tag m) -> [Change] -> Transaction m ()
newVersion branch changes = do
  version <- curVersion branch
  move branch =<< Version.newVersion version changes
