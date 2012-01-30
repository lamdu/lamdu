{-# LANGUAGE EmptyDataDecls, OverloadedStrings #-}

module Editor.Anchors(
    root, rootIRef,
    cursor, cursorIRef,
    branches, view,
    currentBranchIRef, currentBranch,
    initDB,
    dbStore, DBTag,
    viewStore, ViewTag,
    aName, aNameRef,
    maybeUpdateCursor)
where

import Control.Monad (unless, (<=<))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Writer (WriterT, tell, execWriterT)
import Data.Binary (Binary)
import Data.Maybe (fromMaybe)
import Data.Store.Db (Db)
import Data.Store.Guid(Guid)
import Data.Store.IRef (IRef)
import Data.Store.Rev.Branch (Branch)
import Data.Store.Rev.Change (Key, Value)
import Data.Store.Rev.View (View)
import Data.Store.Transaction (Transaction, Store(..))
import Data.Store.Property(Property(..))
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Data.Store.Guid as Guid
import qualified Data.Store.Db as Db
import qualified Data.Store.IRef as IRef
import qualified Data.Store.Property as Property
import qualified Data.Store.Rev.Branch as Branch
import qualified Data.Store.Rev.Version as Version
import qualified Data.Store.Rev.View as View
import qualified Data.Store.Transaction as Transaction
import qualified Editor.AnimIds as AnimIds
import qualified Editor.Data as Data

data DBTag
dbStore :: Db -> Store DBTag IO
dbStore = Db.store

data ViewTag
viewStore :: Monad m => View -> Store ViewTag (Transaction DBTag m)
viewStore = View.store

rootIRef :: IRef Data.Definition
rootIRef = IRef.anchor "root"

root :: Monad m => Transaction.Property ViewTag m Data.Definition
root = Transaction.fromIRef rootIRef

branchesIRef :: IRef [(IRef String, Branch)]
branchesIRef = IRef.anchor "branches"

branches :: Monad m => Transaction.Property DBTag m [(IRef String, Branch)]
branches = Transaction.fromIRef branchesIRef

currentBranchIRef :: IRef Branch
currentBranchIRef = IRef.anchor "currentBranch"

currentBranch :: Monad m => Transaction.Property DBTag m Branch
currentBranch = Transaction.fromIRef currentBranchIRef

cursorIRef :: IRef Widget.Cursor
cursorIRef = IRef.anchor "cursor"

-- Cursor is untagged because it is both saved globally and per-revision.
-- Cursor movement without any revisioned changes are not saved per-revision.
cursor :: Monad m => Transaction.Property t m Widget.Cursor
cursor = Transaction.fromIRef cursorIRef

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

newGetVariable :: Monad m => String -> Transaction t m (IRef Data.Expression)
newGetVariable name = do
  var <- Transaction.newIRef Data.Variable
  Property.set (aNameRef var) name
  Transaction.newIRef $ Data.ExpressionGetVariable var

initDB :: Store DBTag IO -> IO ()
initDB store =
  Transaction.run store $ do
    bs <- initRef branchesIRef $ do
      masterNameIRef <- Transaction.newIRef "master"
      changes <- collectWrites Transaction.newKey $ do
        launchMissilesI <- newGetVariable "launchMissiles"
        unitI <- newGetVariable "()"
        expr <- Transaction.newIRef . Data.ExpressionApply $ Data.Apply launchMissilesI unitI
        Property.set root Data.Definition {
          Data.defParameters = [],
          Data.defBody = expr
          }
        Property.set (aNameRef rootIRef) "awesomeFunc"
        Property.set cursor $ AnimIds.fromIRef rootIRef
      initialVersionIRef <- Version.makeInitialVersion changes
      master <- Branch.new initialVersionIRef
      return [(masterNameIRef, master)]
    let branch = snd $ head bs
    _ <- initRef viewIRef $ View.new branch
    _ <- initRef currentBranchIRef (return branch)
    _ <- initRef cursorIRef . return $ []
    return ()

-- Get an associated name from the given IRef
aName :: Monad m => IRef a -> Transaction.Property t m (Maybe String)
aName iref =
  Property getter setter
  where
    getter = Transaction.lookup nameGuid
    setter (Just val) = Transaction.writeGuid nameGuid val
    setter Nothing = Transaction.delete nameGuid
    nameGuid = Guid.combine guid $ Guid.make "Name"
    guid = IRef.guid iref

aNameRef :: Monad m => IRef a -> Property (Transaction t m) String
aNameRef = Property.pureCompose (fromMaybe "") Just . aName

maybeUpdateCursor :: Monad m => Widget.EventResult -> Transaction t m ()
maybeUpdateCursor = maybe (return ()) (Property.set cursor) . Widget.eCursor
