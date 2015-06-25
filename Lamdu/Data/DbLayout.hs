{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}
module Lamdu.Data.DbLayout
    ( DbM, runDbTransaction
    , ViewM, runViewTransaction
    , CodeProps, codeProps, codeIRefs
    , RevisionProps, revisionProps, revisionIRefs
    , module Lamdu.Data.Anchors
    ) where

import           Control.Applicative (Applicative)
import           Control.Monad.IO.Class (MonadIO)
import           Data.ByteString.Char8 ()
import           Data.Store.Db (Db)
import qualified Data.Store.Db as Db
import           Data.Store.IRef (IRef)
import qualified Data.Store.IRef as IRef
import           Data.Store.Rev.View (View)
import qualified Data.Store.Rev.View as View
import           Data.Store.Transaction (Transaction)
import qualified Data.Store.Transaction as Transaction
import           Lamdu.Data.Anchors (Code(..), Revision(..), assocNameRef, SpecialFunctions(..))
import qualified Lamdu.Data.Anchors as Anchors

type T = Transaction

newtype DbM a = DbM { dbM :: IO a }
    deriving (Functor, Applicative, Monad, MonadIO)

newtype ViewM a = ViewM { viewM :: T DbM a }
    deriving (Functor, Applicative, Monad)

runDbTransaction :: Db -> T DbM a -> IO a
runDbTransaction db = dbM . Transaction.run (Transaction.onStoreM DbM (Db.store db))

runViewTransaction :: View DbM -> T ViewM a -> T DbM a
runViewTransaction v = viewM . (Transaction.run . Transaction.onStoreM ViewM . View.store) v

codeIRefs :: Code (IRef ViewM) ViewM
codeIRefs = Code
    { panes = IRef.anchor "panes"
    , globals = IRef.anchor "globals"
    , specialFunctions = IRef.anchor "specialFuncs"
    , preJumps = IRef.anchor "prejumps"
    , preCursor = IRef.anchor "precursor"
    , postCursor = IRef.anchor "postcursor"
    , tags = IRef.anchor "tags"
    , tids = IRef.anchor "tids"
    }

revisionIRefs :: Revision (IRef DbM) DbM
revisionIRefs = Revision
    { branches = IRef.anchor "branches"
    , currentBranch = IRef.anchor "currentBranch"
    , cursor = IRef.anchor "cursor"
    , redos = IRef.anchor "redos"
    , view = IRef.anchor "view"
    }

type CodeProps = Anchors.CodeProps ViewM
type RevisionProps = Anchors.RevisionProps DbM

codeProps :: CodeProps
codeProps = Anchors.onCode Transaction.mkPropertyFromIRef codeIRefs

revisionProps :: RevisionProps
revisionProps = Anchors.onRevision Transaction.mkPropertyFromIRef revisionIRefs
