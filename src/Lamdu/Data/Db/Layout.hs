{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, GeneralizedNewtypeDeriving #-}
module Lamdu.Data.Db.Layout
    ( DbM, runDbTransaction
    , ViewM, runViewTransaction
    , CodeAnchors, codeAnchors, codeIRefs
    , RevisionProps, revisionProps, revisionIRefs
    , guiState
    , module Lamdu.Data.Anchors
    ) where

import           Lamdu.Prelude

import           Control.Monad.IO.Class (MonadIO)
import           Data.ByteString.Char8 ()
import           Data.Store.Db (DB)
import qualified Data.Store.Db as Db
import           Data.Store.IRef (IRef)
import qualified Data.Store.IRef as IRef
import           Data.Store.Rev.View (View)
import qualified Data.Store.Rev.View as View
import           Data.Store.Transaction (Transaction)
import qualified Data.Store.Transaction as Transaction
import           GUI.Momentu (GUIState)
import           Lamdu.Data.Anchors (Code(..), Revision(..), assocNameRef)
import qualified Lamdu.Data.Anchors as Anchors

type T = Transaction

newtype DbM a = DbM { dbM :: IO a }
    deriving (Functor, Applicative, Monad, MonadIO)

newtype ViewM a = ViewM { viewM :: T DbM a }
    deriving (Functor, Applicative, Monad)

runDbTransaction :: DB -> T DbM a -> IO a
runDbTransaction db = dbM . Transaction.run (Transaction.onStoreM DbM (Db.store db))

runViewTransaction :: View DbM -> T ViewM a -> T DbM a
runViewTransaction v = viewM . (Transaction.run . Transaction.onStoreM ViewM . View.store) v

codeIRefs :: Code (IRef ViewM) ViewM
codeIRefs = Code
    { repl = IRef.anchor "repl"
    , panes = IRef.anchor "panes"
    , globals = IRef.anchor "globals"
    , preJumps = IRef.anchor "prejumps"
    , preGuiState = IRef.anchor "preguistate"
    , postGuiState = IRef.anchor "postguistate"
    , tags = IRef.anchor "tags"
    , tids = IRef.anchor "tids"
    }

revisionIRefs :: Revision (IRef DbM) DbM
revisionIRefs = Revision
    { branches = IRef.anchor "branches"
    , currentBranch = IRef.anchor "currentBranch"
    , redos = IRef.anchor "redos"
    , view = IRef.anchor "view"
    }

type CodeAnchors = Anchors.CodeAnchors ViewM
type RevisionProps = Anchors.RevisionProps DbM

codeAnchors :: CodeAnchors
codeAnchors = Anchors.onCode Transaction.mkPropertyFromIRef codeIRefs

revisionProps :: RevisionProps
revisionProps = Anchors.onRevision Transaction.mkPropertyFromIRef revisionIRefs

guiState :: IRef DbM GUIState
guiState = IRef.anchor "guiState"
