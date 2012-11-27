{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Store.Rev.Branch
    (Branch, guid, new, move, curVersion, newVersion)
where

import Control.Monad (liftM)
import Data.Store.Guid (Guid)
import Data.Store.Rev.Change (Change)
import Data.Store.Rev.Version (Version)
import Data.Store.Rev.ViewBranchInternal (BranchData(..), Branch(..), moveView, brVersion)
import Data.Store.Transaction (Transaction)
import qualified Control.Lens as Lens
import qualified Data.Store.IRef as IRef
import qualified Data.Store.Rev.Version as Version
import qualified Data.Store.Transaction as Transaction

guid :: Branch -> Guid
guid = IRef.guid . unBranch

move :: Monad m => Branch -> Version -> Transaction m ()
move (Branch dataIRef) destVersion = do
  BranchData srcVersion views <- Transaction.readIRef dataIRef
  mapM_ (moveToDest srcVersion) views
  Transaction.writeIRef dataIRef (BranchData destVersion views)
  where
    moveToDest srcVersion view = moveView view srcVersion destVersion

curVersion :: Monad m => Branch -> Transaction m Version
curVersion (Branch dataIRef) = Lens.view brVersion `liftM` Transaction.readIRef dataIRef

-- | A Branch is a mutable version ptr
new :: Monad m => Version -> Transaction m Branch
new version = Branch `liftM`
              Transaction.newIRef (BranchData version [])

newVersion :: Monad m => Branch -> [Change] -> Transaction m ()
newVersion branch changes = do
  version <- curVersion branch
  move branch =<< Version.newVersion version changes
