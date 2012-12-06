{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Store.Rev.Branch
    (Branch, guid, new, move, curVersion, newVersion)
where

import Control.MonadA (MonadA)
import Data.Foldable (traverse_)
import Data.Store.Guid (Guid)
import Data.Store.Rev.Change (Change)
import Data.Store.Rev.Version (Version)
import Data.Store.Rev.ViewBranchInternal (BranchData(..), Branch(..), moveView, brVersion)
import Data.Store.Transaction (Transaction)
import qualified Control.Lens as Lens
import qualified Data.Store.IRef as IRef
import qualified Data.Store.Rev.Version as Version
import qualified Data.Store.Transaction as Transaction

guid :: Branch t -> Guid
guid = IRef.guid . unBranch

move :: MonadA m => Branch (m ()) -> Version (m ()) -> Transaction m ()
move (Branch dataIRef) destVersion = do
  BranchData srcVersion views <- Transaction.readIRef dataIRef
  traverse_ (moveToDest srcVersion) views
  Transaction.writeIRef dataIRef (BranchData destVersion views)
  where
    moveToDest srcVersion view = moveView view srcVersion destVersion

curVersion :: MonadA m => Branch (m ()) -> Transaction m (Version (m ()))
curVersion (Branch dataIRef) = Lens.view brVersion `fmap` Transaction.readIRef dataIRef

-- | A Branch is a mutable version ptr
new :: MonadA m => Version (m ()) -> Transaction m (Branch (m ()))
new version = Branch `fmap`
              Transaction.newIRef (BranchData version [])

newVersion :: MonadA m => Branch (m ()) -> [Change] -> Transaction m ()
newVersion branch changes = do
  version <- curVersion branch
  move branch =<< Version.newVersion version changes
