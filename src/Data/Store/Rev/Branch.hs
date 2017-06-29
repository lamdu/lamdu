module Data.Store.Rev.Branch
    (Branch, uuid, new, move, curVersion, newVersion)
where

import           Data.UUID.Types (UUID)
import qualified Data.Store.IRef as IRef
import           Data.Store.Rev.Change (Change)
import           Data.Store.Rev.Version (Version)
import qualified Data.Store.Rev.Version as Version
import           Data.Store.Rev.ViewBranchInternal (BranchData(..), Branch(..), moveView, brVersion)
import           Data.Store.Transaction (Transaction)
import qualified Data.Store.Transaction as Transaction

import           Lamdu.Prelude

uuid :: Branch t -> UUID
uuid = IRef.uuid . unBranch

move :: Monad m => Branch m -> Version m -> Transaction m ()
move (Branch dataIRef) destVersion = do
    BranchData srcVersion views <- Transaction.readIRef dataIRef
    traverse_ (moveToDest srcVersion) views
    Transaction.writeIRef dataIRef (BranchData destVersion views)
    where
        moveToDest srcVersion view = moveView view srcVersion destVersion

curVersion :: Monad m => Branch m -> Transaction m (Version m)
curVersion (Branch dataIRef) = (^. brVersion) `fmap` Transaction.readIRef dataIRef

-- | A Branch is a mutable version ptr
new :: Monad m => Version m -> Transaction m (Branch m)
new version = Branch `fmap`
                            Transaction.newIRef (BranchData version [])

newVersion :: Monad m => Branch m -> [Change] -> Transaction m ()
newVersion branch changes = do
    version <- curVersion branch
    move branch =<< Version.newVersion version changes
