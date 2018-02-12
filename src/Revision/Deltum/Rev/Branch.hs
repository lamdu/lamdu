module Revision.Deltum.Rev.Branch
    (Branch, uuid, new, move, curVersion, newVersion)
where

import           Data.UUID.Types (UUID)
import qualified Revision.Deltum.IRef as IRef
import           Revision.Deltum.Rev.Change (Change)
import           Revision.Deltum.Rev.Version (Version)
import qualified Revision.Deltum.Rev.Version as Version
import           Revision.Deltum.Rev.ViewBranchInternal (BranchData(..), Branch(..), moveView, brVersion)
import           Revision.Deltum.Transaction (Transaction)
import qualified Revision.Deltum.Transaction as Transaction

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
