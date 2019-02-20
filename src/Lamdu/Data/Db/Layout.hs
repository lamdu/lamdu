{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Lamdu.Data.Db.Layout
    ( DbM(DbM), runDbTransaction
    , ViewM, runViewTransaction
    , GuiAnchors, guiAnchors, guiIRefs
    , CodeAnchors, codeAnchors, codeIRefs
    , RevisionProps, revisionProps, revisionIRefs
    , guiState
    , dbSchemaVersion, curDbSchemaVersion
    , module Lamdu.Data.Anchors
    ) where

import           Control.Monad.IO.Class (MonadIO)
import           Data.ByteString.Char8 ()
import           GUI.Momentu.State (GUIState)
import           Lamdu.Data.Anchors (Gui(..), Code(..), Revision(..))
import qualified Lamdu.Data.Anchors as Anchors
import           Revision.Deltum.IRef (IRef)
import qualified Revision.Deltum.IRef as IRef
import           Revision.Deltum.Rev.View (View)
import qualified Revision.Deltum.Rev.View as View
import           Revision.Deltum.Transaction (Transaction)
import qualified Revision.Deltum.Transaction as Transaction

import           Lamdu.Prelude

type T = Transaction

newtype DbM a = DbM { dbM :: IO a }
    deriving (Functor, Applicative, Monad, MonadIO)

newtype ViewM a = ViewM { viewM :: T DbM a }
    deriving (Functor, Applicative, Monad)

runDbTransaction :: Transaction.Store DbM -> T DbM a -> IO a
runDbTransaction store = dbM . Transaction.run store

runViewTransaction :: View DbM -> T ViewM a -> T DbM a
runViewTransaction v = viewM . (Transaction.run . Transaction.onStoreM ViewM . View.store) v

codeIRefs :: Code (IRef ViewM) ViewM
codeIRefs = Code
    { repl = IRef.anchor "repl"
    , panes = IRef.anchor "panes"
    , globals = IRef.anchor "globals"
    , tags = IRef.anchor "tags"
    , tids = IRef.anchor "tids"
    }

guiIRefs :: Gui (IRef ViewM)
guiIRefs = Gui
    { preJumps = IRef.anchor "prejumps"
    , preGuiState = IRef.anchor "preguistate"
    , postGuiState = IRef.anchor "postguistate"
    }

revisionIRefs :: Revision (IRef DbM) DbM
revisionIRefs = Revision
    { branches = IRef.anchor "branches"
    , currentBranch = IRef.anchor "currentBranch"
    , redos = IRef.anchor "redos"
    , view = IRef.anchor "view"
    }

type GuiAnchors = Anchors.GuiAnchors (T ViewM) (T ViewM)
type CodeAnchors = Anchors.CodeAnchors ViewM
type RevisionProps = Anchors.RevisionProps DbM

guiAnchors :: GuiAnchors
guiAnchors = Anchors.onGui Transaction.mkPropertyFromIRef guiIRefs

codeAnchors :: CodeAnchors
codeAnchors = Anchors.onCode Transaction.mkPropertyFromIRef codeIRefs

revisionProps :: RevisionProps
revisionProps = Anchors.onRevision Transaction.mkPropertyFromIRef revisionIRefs

guiState :: IRef DbM GUIState
guiState = IRef.anchor "guiState"

dbSchemaVersion :: IRef DbM Int
dbSchemaVersion = IRef.anchor "dbSchemaVersion"

curDbSchemaVersion :: Int
curDbSchemaVersion = 8
