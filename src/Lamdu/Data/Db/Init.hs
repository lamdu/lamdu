-- | Initialize a database, populating it with "freshdb.json" if needed
module Lamdu.Data.Db.Init
    ( initDb
    ) where

import qualified Data.Property as Property
import qualified GUI.Momentu as M
import qualified Lamdu.Data.Anchors as Anchors
import           Lamdu.Data.Db.Layout (DbM)
import qualified Lamdu.Data.Db.Layout as DbLayout
import qualified Lamdu.GUI.WidgetIdIRef as WidgetIdIRef
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Revision.Deltum.Rev.Branch (Branch)
import qualified Revision.Deltum.Rev.Branch as Branch
import           Revision.Deltum.Rev.Version (Version)
import qualified Revision.Deltum.Rev.Version as Version
import qualified Revision.Deltum.Rev.View as View
import           Revision.Deltum.Transaction (Transaction)
import qualified Revision.Deltum.Transaction as Transaction

import           Lamdu.Prelude

type T = Transaction

setName :: Monad m => Branch m -> Text -> T m ()
setName = Property.setP . Anchors.assocBranchNameRef

newBranch :: Monad m => Text -> Version m -> T m (Branch m)
newBranch name ver =
    do
        branch <- Branch.new ver
        setName branch name
        pure branch

initDb :: Transaction.Store DbM -> T DbLayout.ViewM () -> IO ()
initDb db importAct =
    DbLayout.runDbTransaction db $
    do
        emptyVersion <- Version.makeInitialVersion []
        master <- newBranch "master" emptyVersion
        view <- View.new master
        Transaction.writeIRef DbLayout.guiState (M.GUIState WidgetIds.defaultCursor mempty)
        Transaction.writeIRef DbLayout.dbSchemaVersion DbLayout.curDbSchemaVersion
        let writeRevAnchor f = Transaction.writeIRef (f DbLayout.revisionIRefs)
        writeRevAnchor DbLayout.view view
        writeRevAnchor DbLayout.branches [master]
        writeRevAnchor DbLayout.currentBranch master
        writeRevAnchor DbLayout.redos []
        let paneWId = WidgetIdIRef.fromIRef $ DbLayout.panes DbLayout.codeIRefs
        let initGuiState = M.GUIState paneWId mempty
        DbLayout.runViewTransaction view $
            do
                let writeCodeAnchor f = Transaction.writeIRef (f DbLayout.codeIRefs)
                let writeGuiAnchor f = Transaction.writeIRef (f DbLayout.guiIRefs)
                writeCodeAnchor DbLayout.globals mempty
                writeCodeAnchor DbLayout.panes mempty
                writeCodeAnchor DbLayout.tids mempty
                writeCodeAnchor DbLayout.tags mempty
                writeGuiAnchor DbLayout.preJumps []
                writeGuiAnchor DbLayout.preGuiState initGuiState
                writeGuiAnchor DbLayout.postGuiState initGuiState
                importAct
        -- Prevent undo into the invalid empty revision
        newVer <- Branch.curVersion master
        Version.preventUndo newVer
