{-# LANGUAGE EmptyDataDecls, OverloadedStrings, GeneralizedNewtypeDeriving #-}

module Editor.Anchors
  ( panes, panesIRef, clipboards
  , cursor, preCursor, postCursor, preJumps
  , branches, branchesIRef, view, redos, currentBranch
  , globals
  , sugarConfig, ffiEnv
  , newBuiltin, newDefinition
  , Pane
  , nonEmptyAssocDataRef
  , assocNameRef, assocSearchTermRef
  , makePane, makeDefinition, newPane
  , savePreJumpPosition, jumpBack
  , MkProperty, getP, setP, modP

  , DbM, runDbTransaction
  , ViewM, runViewTransaction
  ) where

import Control.Applicative (Applicative)
import Control.Monad (liftM, liftM2, when)
import Control.Monad.IO.Class (MonadIO)
import Data.Binary (Binary(..))
import Data.ByteString.Char8 ()
import Data.List.Split (splitOn)
import Data.Store.Db (Db)
import Data.Store.Guid (Guid)
import Data.Store.IRef (IRef)
import Data.Store.Property (Property(Property))
import Data.Store.Rev.Branch (Branch)
import Data.Store.Rev.Version(Version)
import Data.Store.Rev.View (View)
import Data.Store.Transaction (Transaction)
import Editor.CodeEdit.Sugar.Config (SugarConfig)
import qualified Data.ByteString as SBS
import qualified Data.Store.Db as Db
import qualified Data.Store.IRef as IRef
import qualified Data.Store.Property as Property
import qualified Data.Store.Rev.View as View
import qualified Data.Store.Transaction as Transaction
import qualified Editor.CodeEdit.FFI as FFI
import qualified Editor.Data as Data
import qualified Editor.Data.IRef as DataIRef
import qualified Graphics.UI.Bottle.Widget as Widget

type Pane = DataIRef.DefinitionIRef

newtype DbM a = DbM { dbM :: IO a }
  deriving (Functor, Applicative, Monad, MonadIO)

newtype ViewM a = ViewM { viewM :: Transaction DbM a }
  deriving (Functor, Applicative, Monad)

runDbTransaction :: Db -> Transaction DbM a -> IO a
runDbTransaction db = dbM . Transaction.run (Transaction.onStoreM DbM (Db.store db))

runViewTransaction :: View -> Transaction ViewM a -> Transaction DbM a
runViewTransaction v = viewM . (Transaction.run . Transaction.onStoreM ViewM . View.store) v

panesIRef :: IRef [Pane]
panesIRef = IRef.anchor "panes"

type MkProperty m a = Transaction m (Transaction.Property m a)

panes :: MkProperty ViewM [Pane]
panes = Transaction.fromIRef panesIRef

clipboards :: MkProperty ViewM [DataIRef.Expression]
clipboards = Transaction.fromIRef $ IRef.anchor "clipboard"

branchesIRef :: IRef [Branch]
branchesIRef = IRef.anchor "branches"

branches :: MkProperty DbM [Branch]
branches = Transaction.fromIRef branchesIRef

currentBranch :: MkProperty DbM Branch
currentBranch = Transaction.fromIRef $ IRef.anchor "currentBranch"

-- TODO: This should be an index
globals :: MkProperty ViewM [DataIRef.DefinitionIRef]
globals = Transaction.fromIRef $ IRef.anchor "globals"

sugarConfig :: MkProperty ViewM SugarConfig
sugarConfig = Transaction.fromIRef $ IRef.anchor "sugarConfig"

ffiEnv :: MkProperty ViewM FFI.Env
ffiEnv = Transaction.fromIRef $ IRef.anchor "ffiEnv"

-- Cursor is untagged because it is both saved globally and per-revision.
-- Cursor movement without any revisioned changes are not saved per-revision.
cursor :: MkProperty DbM Widget.Id
cursor = Transaction.fromIRef $ IRef.anchor "cursor"

preJumps :: MkProperty ViewM [Widget.Id]
preJumps = Transaction.fromIRef $ IRef.anchor "prejumps"

preCursor :: MkProperty ViewM Widget.Id
preCursor = Transaction.fromIRef $ IRef.anchor "precursor"

postCursor :: MkProperty ViewM Widget.Id
postCursor = Transaction.fromIRef $ IRef.anchor "postcursor"

redos :: MkProperty DbM [Version]
redos = Transaction.fromIRef $ IRef.anchor "redos"

view :: MkProperty DbM View
view = Transaction.fromIRef $ IRef.anchor "HEAD"

makePane :: DataIRef.DefinitionIRef -> Pane
makePane = id

makeDefinition
  :: Transaction ViewM DataIRef.DefinitionIRef
makeDefinition = do
  let newHole = DataIRef.newExprBody $ Data.ExpressionLeaf Data.Hole
  defI <-
    Transaction.newIRef =<<
    liftM2 (Data.Definition . Data.DefinitionExpression) newHole newHole
  modP globals (defI :)
  return defI

nonEmptyAssocDataRef ::
  (Monad m, Binary a) =>
  SBS.ByteString -> Guid -> Transaction m a -> MkProperty m a
nonEmptyAssocDataRef str guid makeDef = do
  dataRef <- Transaction.assocDataRef str guid
  def <-
    case Property.value dataRef of
    Nothing -> do
      def <- makeDef
      Property.set dataRef $ Just def
      return def
    Just val ->
      return val
  return $ Property def (Property.set dataRef . Just)

assocNameRef :: Monad m => Guid -> MkProperty m String
assocNameRef = Transaction.assocDataRefDef "" "Name"

assocSearchTermRef :: Monad m => Guid -> MkProperty m String
assocSearchTermRef = Transaction.assocDataRefDef "" "searchTerm"

newPane :: DataIRef.DefinitionIRef -> Transaction ViewM ()
newPane defI = do
  panesP <- panes
  when (defI `notElem` Property.value panesP) $
    Property.set panesP $ makePane defI : Property.value panesP

savePreJumpPosition :: Widget.Id -> Transaction ViewM ()
savePreJumpPosition pos = modP preJumps $ (pos :) . take 19

jumpBack :: Transaction ViewM (Maybe (Transaction ViewM Widget.Id))
jumpBack = do
  preJumpsP <- preJumps
  return $
    case Property.value preJumpsP of
    [] -> Nothing
    (j:js) -> Just $ do
      Property.set preJumpsP js
      return j

newBuiltin
  :: Monad m
  => String -> DataIRef.Expression
  -> Transaction m DataIRef.DefinitionIRef
newBuiltin fullyQualifiedName typeI =
  newDefinition name . (`Data.Definition` typeI) . Data.DefinitionBuiltin .
  Data.Builtin $ Data.FFIName (init path) name
  where
    name = last path
    path = splitOn "." fullyQualifiedName

newDefinition :: Monad m => String -> DataIRef.DefinitionI -> Transaction m DataIRef.DefinitionIRef
newDefinition name defI = do
  res <- Transaction.newIRef defI
  setP (assocNameRef (IRef.guid res)) name
  return res

getP :: Monad m => MkProperty m a -> Transaction m a
getP = liftM Property.value

setP :: Monad m => MkProperty m a -> a -> Transaction m ()
setP mkProp val = do
  prop <- mkProp
  Property.set prop val

modP :: Monad m => MkProperty m a -> (a -> a) -> Transaction m ()
modP mkProp f = do
  prop <- mkProp
  Property.pureModify prop f
