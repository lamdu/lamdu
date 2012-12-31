{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, DeriveDataTypeable #-}

module Lamdu.Anchors
  ( panes, panesIRef
  , clipboards, clipboardsIRef
  , cursor, cursorIRef
  , preCursor, preCursorIRef
  , postCursor, postCursorIRef
  , preJumps, preJumpsIRef
  , branches, branchesIRef
  , view, viewIRef
  , redos, redosIRef
  , currentBranch, currentBranchIRef
  , globals, globalsIRef
  , sugarConfig, sugarConfigIRef
  , ffiEnv, ffiEnvIRef

  , Pane, makePane
  , nonEmptyAssocDataRef
  , assocNameRef, assocSearchTermRef

  , DbM, runDbTransaction
  , ViewM, runViewTransaction
  ) where

import Control.Applicative (Applicative)
import Control.Monad.IO.Class (MonadIO)
import Control.MonadA (MonadA)
import Data.Binary (Binary(..))
import Data.ByteString.Char8 ()
import Data.Store.Db (Db)
import Data.Store.Guid (Guid)
import Data.Store.IRef (IRef, Tag)
import Data.Store.Property (Property(Property))
import Data.Store.Rev.Branch (Branch)
import Data.Store.Rev.Version(Version)
import Data.Store.Rev.View (View)
import Data.Store.Transaction (Transaction, MkProperty(..))
import Data.Typeable (Typeable)
import Lamdu.CodeEdit.Sugar.Config (SugarConfig)
import Lamdu.Data.Expression.IRef (DefI)
import qualified Data.ByteString as SBS
import qualified Data.Store.Db as Db
import qualified Data.Store.IRef as IRef
import qualified Data.Store.Rev.View as View
import qualified Data.Store.Transaction as Transaction
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Lamdu.CodeEdit.FFI as FFI
import qualified Lamdu.Data.Expression.IRef as DataIRef

type T = Transaction
type Pane t = DefI t

newtype DbM a = DbM { dbM :: IO a }
  deriving (Functor, Applicative, Monad, MonadIO, Typeable)

newtype ViewM a = ViewM { viewM :: T DbM a }
  deriving (Functor, Applicative, Monad, Typeable)

runDbTransaction :: Db -> T DbM a -> IO a
runDbTransaction db = dbM . Transaction.run (Transaction.onStoreM DbM (Db.store db))

runViewTransaction :: View (Tag DbM) -> T ViewM a -> T DbM a
runViewTransaction v = viewM . (Transaction.run . Transaction.onStoreM ViewM . View.store) v

panesIRef :: IRef t [Pane t]
panesIRef = IRef.anchor "panes"

panes :: MkProperty ViewM [Pane (Tag ViewM)]
panes = Transaction.mkProperty panesIRef

clipboards :: MkProperty ViewM [DefI (Tag ViewM)]
clipboards = Transaction.mkProperty clipboardsIRef

clipboardsIRef :: IRef (Tag ViewM) [DefI (Tag ViewM)]
clipboardsIRef = IRef.anchor "clipboard"

branchesIRef :: IRef (Tag DbM) [Branch (Tag DbM)]
branchesIRef = IRef.anchor "branches"

branches :: MkProperty DbM [Branch (Tag DbM)]
branches = Transaction.mkProperty branchesIRef

currentBranch :: MkProperty DbM (Branch (Tag DbM))
currentBranch = Transaction.mkProperty currentBranchIRef

currentBranchIRef :: IRef (Tag DbM) (Branch (Tag DbM))
currentBranchIRef = IRef.anchor "currentBranch"

-- TODO: This should be an index
globals :: MkProperty ViewM [DataIRef.DefI (Tag ViewM)]
globals = Transaction.mkProperty globalsIRef

globalsIRef :: IRef (Tag ViewM) [DataIRef.DefI (Tag ViewM)]
globalsIRef = IRef.anchor "globals"

sugarConfig :: MkProperty ViewM (SugarConfig (Tag ViewM))
sugarConfig = Transaction.mkProperty sugarConfigIRef

sugarConfigIRef :: IRef (Tag ViewM) (SugarConfig (Tag ViewM))
sugarConfigIRef = IRef.anchor "sugarConfig"

ffiEnv :: MkProperty ViewM (FFI.Env (Tag ViewM))
ffiEnv = Transaction.mkProperty ffiEnvIRef

ffiEnvIRef :: IRef (Tag ViewM) (FFI.Env (Tag ViewM))
ffiEnvIRef = IRef.anchor "ffiEnv"

-- Cursor is untagged because it is both saved globally and per-revision.
-- Cursor movement without any revisioned changes are not saved per-revision.
cursor :: MkProperty DbM Widget.Id
cursor = Transaction.mkProperty cursorIRef

cursorIRef :: IRef (Tag DbM) Widget.Id
cursorIRef = IRef.anchor "cursor"

preJumps :: MkProperty ViewM [Widget.Id]
preJumps = Transaction.mkProperty preJumpsIRef

preJumpsIRef :: IRef (Tag ViewM) [Widget.Id]
preJumpsIRef = IRef.anchor "prejumps"

preCursor :: MkProperty ViewM Widget.Id
preCursor = Transaction.mkProperty preCursorIRef

preCursorIRef :: IRef (Tag ViewM) Widget.Id
preCursorIRef = IRef.anchor "precursor"

postCursor :: MkProperty ViewM Widget.Id
postCursor = Transaction.mkProperty postCursorIRef

postCursorIRef :: IRef (Tag ViewM) Widget.Id
postCursorIRef = IRef.anchor "postcursor"

redos :: MkProperty DbM [Version (Tag DbM)]
redos = Transaction.mkProperty redosIRef

redosIRef :: IRef (Tag DbM) [Version (Tag DbM)]
redosIRef = IRef.anchor "redos"

view :: MkProperty DbM (View (Tag DbM))
view = Transaction.mkProperty viewIRef

viewIRef :: IRef (Tag DbM) (View (Tag DbM))
viewIRef = IRef.anchor "HEAD"

makePane :: DataIRef.DefI (Tag ViewM) -> Pane (Tag ViewM)
makePane = id

nonEmptyAssocDataRef ::
  (MonadA m, Binary a) =>
  SBS.ByteString -> Guid -> T m a -> MkProperty m a
nonEmptyAssocDataRef str guid makeDef = MkProperty $ do
  v <- maybe noVal return =<< Transaction.getP dataRef
  return $ Property v setter
  where
    setter = Transaction.setP dataRef . Just
    noVal = do
      def <- makeDef
      setter def
      return def
    dataRef = Transaction.assocDataRef str guid

assocNameRef :: MonadA m => Guid -> MkProperty m String
assocNameRef = Transaction.assocDataRefDef "" "Name"

assocSearchTermRef :: MonadA m => Guid -> MkProperty m String
assocSearchTermRef = Transaction.assocDataRefDef "" "searchTerm"
