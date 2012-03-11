{-# LANGUAGE TemplateHaskell, EmptyDataDecls, OverloadedStrings #-}

module Editor.Anchors(
    root, rootIRef,
    cursor, cursorIRef, preCursor, postCursor, preJumps,
    branches, view, redos,
    currentBranchIRef, currentBranch,
    globals,
    initDB,
    Pane(..), atPaneCursor, atPaneDefinition,
    dbStore, DBTag,
    viewStore, ViewTag,
    aNameRef, variableNameRef,
    makePane, makeDefinition, newPane,
    jumpTo, canJumpBack, jumpBack)
where

import Control.Monad (when, unless, (<=<), liftM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Writer (WriterT, tell, execWriterT)
import Data.Binary (Binary(..))
import Data.Derive.Binary(makeBinary)
import Data.DeriveTH(derive)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import Data.Store.Db (Db)
import Data.Store.Guid(Guid)
import Data.Store.IRef (IRef)
import Data.Store.Property(Property(Property))
import Data.Store.Rev.Branch (Branch)
import Data.Store.Rev.Change (Key, Value)
import Data.Store.Rev.Version(Version)
import Data.Store.Rev.View (View)
import Data.Store.Transaction (Transaction, Store(..))
import qualified Data.AtFieldTH as AtFieldTH
import qualified Data.Store.Db as Db
import qualified Data.Store.Guid as Guid
import qualified Data.Store.IRef as IRef
import qualified Data.Store.Property as Property
import qualified Data.Store.Rev.Branch as Branch
import qualified Data.Store.Rev.Version as Version
import qualified Data.Store.Rev.View as View
import qualified Data.Store.Transaction as Transaction
import qualified Editor.WidgetIds as WidgetIds
import qualified Editor.Data as Data
import qualified Graphics.UI.Bottle.Widget as Widget

data Pane = Pane {
  paneCursor :: Widget.Id,
  paneDefinition :: IRef Data.Definition
  }
  deriving (Eq, Ord, Read, Show)

derive makeBinary ''Pane
AtFieldTH.make ''Pane

data DBTag
dbStore :: Db -> Store DBTag IO
dbStore = Db.store

data ViewTag
viewStore :: Monad m => View -> Store ViewTag (Transaction DBTag m)
viewStore = View.store

rootIRef :: IRef [Pane]
rootIRef = IRef.anchor "root"

root :: Monad m => Transaction.Property ViewTag m [Pane]
root = Transaction.fromIRef rootIRef

branchesIRef :: IRef [(IRef String, Branch)]
branchesIRef = IRef.anchor "branches"

branches :: Monad m => Transaction.Property DBTag m [(IRef String, Branch)]
branches = Transaction.fromIRef branchesIRef

currentBranchIRef :: IRef Branch
currentBranchIRef = IRef.anchor "currentBranch"

currentBranch :: Monad m => Transaction.Property DBTag m Branch
currentBranch = Transaction.fromIRef currentBranchIRef

cursorIRef :: IRef Widget.Id
cursorIRef = IRef.anchor "cursor"

-- TODO: This should be an index
globals :: Monad m => Transaction.Property ViewTag m [Data.VariableRef]
globals = Transaction.fromIRef $ IRef.anchor "globals"

-- Cursor is untagged because it is both saved globally and per-revision.
-- Cursor movement without any revisioned changes are not saved per-revision.
cursor :: Monad m => Transaction.Property DBTag m Widget.Id
cursor = Transaction.fromIRef cursorIRef

preJumps :: Monad m => Transaction.Property ViewTag m [Widget.Id]
preJumps = Transaction.fromIRef $ IRef.anchor "prejumps"

preCursor :: Monad m => Transaction.Property ViewTag m Widget.Id
preCursor = Transaction.fromIRef $ IRef.anchor "precursor"

postCursor :: Monad m => Transaction.Property ViewTag m Widget.Id
postCursor = Transaction.fromIRef $ IRef.anchor "postcursor"

-- Initialize an IRef if it does not already exist.
initRef :: (Binary a, Monad m) => IRef a -> Transaction t m a -> Transaction t m a
initRef iref act = do
  exists <- Transaction.irefExists iref
  unless exists (Property.set p =<< act)
  Property.get p
  where
    p = Transaction.fromIRef iref

viewIRef :: IRef View
viewIRef = IRef.anchor "HEAD"

redosIRef :: IRef [Version]
redosIRef = IRef.anchor "redos"

redos :: Monad m => Transaction.Property DBTag m [Version]
redos = Transaction.fromIRef redosIRef

view :: Monad m => Transaction.Property DBTag m View
view = Transaction.fromIRef viewIRef

type WriteCollector m = WriterT [(Key, Value)] m

writeCollectorStore ::
  Monad m => m Guid -> Store t (WriteCollector m)
writeCollectorStore newKey = Store {
  storeNewKey = lift newKey,
  storeLookup = fail "Cannot lookup when collecting writes!",
  storeAtomicWrite = tell <=< mapM makeChange
  }
  where
    makeChange (key, Just value) = return (key, value)
    makeChange (_, Nothing) =
      fail "Cannot delete when collecting writes"

collectWrites ::
  Monad m =>
  m Guid -> Transaction t (WriteCollector m) () -> m [(Key, Value)]
collectWrites newGuid =
  execWriterT . Transaction.run (writeCollectorStore newGuid)

newBuiltin :: Monad m => String -> Transaction t m Data.VariableRef
newBuiltin fullyQualifiedName = do
  builtinIRef <- Transaction.newIRef Data.Builtin {
    Data.biModule = init path,
    Data.biName = name
    }
  Property.set (aNameRef builtinIRef) name
  return $ Data.BuiltinRef builtinIRef
  where
    name = last path
    path = splitOn "." fullyQualifiedName

makePane :: IRef Data.Definition -> Pane
makePane defI = Pane {
  paneCursor = WidgetIds.fromIRef defI,
  paneDefinition = defI
  }

makeDefinition :: Monad m => Transaction ViewTag m (IRef Data.Definition)
makeDefinition = do
  holeI <- Transaction.newIRef $ Data.ExpressionHole Data.emptyHoleState
  defI <- Transaction.newIRef Data.Definition {
    Data.defParameters = [],
    Data.defBody = holeI
    }
  Property.pureModify globals (Data.DefinitionRef defI :)
  return defI

initDB :: Store DBTag IO -> IO ()
initDB store =
  Transaction.run store $ do
    bs <- initRef branchesIRef $ do
      masterNameIRef <- Transaction.newIRef "master"
      changes <- collectWrites Transaction.newKey $ do
        Property.set globals =<< mapM newBuiltin temporaryBuiltinsDatabase
        defI <- makeDefinition
        Property.set root [makePane defI]
        Property.set preJumps []
        Property.set preCursor $ WidgetIds.fromIRef defI
        Property.set postCursor $ WidgetIds.fromIRef defI
      initialVersionIRef <- Version.makeInitialVersion changes
      master <- Branch.new initialVersionIRef
      return [(masterNameIRef, master)]
    let branch = snd $ head bs
    _ <- initRef viewIRef $ View.new branch
    _ <- initRef currentBranchIRef (return branch)
    _ <- initRef redosIRef $ return []
    _ <- initRef cursorIRef . return $ Widget.Id []
    return ()
  where
    temporaryBuiltinsDatabase =
      ["Data.List.sort"
      ,"Data.List.reverse"
      ,"Data.List.length"
      ,"Data.List.tail"
      ,"Data.List.zipWith"
      ,"Control.Applicative.liftA2"
      ,"Control.Applicative.pure"
      ,"Control.Applicative.shuki"
      ,"Control.Monad.join"
      ,"Prelude.fmap"
      ,"Prelude.const"
      ,"Prelude.+"
      ,"Prelude.-"
      ,"Prelude.*"
      ,"Prelude./"
      ,"Prelude.^"
      ,"Prelude.:"
      ,"Prelude.=="
      ,"Prelude./="
      ,"Prelude.<="
      ,"Prelude.>="
      ,"Prelude.<"
      ,"Prelude.>"
      ,"Prelude.if"
      ]

-- Get an associated name from the given IRef
aNameRef :: Monad m => IRef a -> Property (Transaction t m) String
aNameRef = Property.pureCompose (fromMaybe "") Just . aNameGuid . IRef.guid
  where
    aNameGuid guid =
      Property getter setter
      where
        getter = Transaction.lookup nameGuid
        setter (Just val) = Transaction.writeGuid nameGuid val
        setter Nothing = Transaction.delete nameGuid
        nameGuid = Guid.combine guid $ Guid.make "Name"

variableNameRef :: Monad m => Data.VariableRef -> Property (Transaction t m) String
variableNameRef = Data.onVariableIRef aNameRef

newPane :: Monad m => IRef Data.Definition -> Transaction ViewTag m ()
newPane defI = do
  panes <- Property.get panesRef
  when (all ((/= defI) . paneDefinition) panes) $
    Property.set panesRef $ makePane defI : panes
  where
    panesRef = Transaction.fromIRef rootIRef

jumpTo ::
  Monad m
  => Widget.Id
  -> Widget.Id
  -> Transaction ViewTag m Widget.Id
jumpTo old new = do
  Property.pureModify preJumps $ (old :) . take 19
  return new

canJumpBack :: Monad m => Transaction ViewTag m Bool
canJumpBack = liftM (not . null) $ Property.get preJumps

jumpBack :: Monad m => Transaction ViewTag m (Maybe Widget.Id)
jumpBack = do
  jumps <- Property.get preJumps
  case jumps of
    [] -> return Nothing
    (j:js) -> do
      Property.set preJumps js
      return $ Just j
