module Editor.ExampleDB(initDB) where

import Control.Monad (unless, (<=<))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Writer (WriterT, tell, execWriterT)
import Data.Binary (Binary(..))
import Data.Store.Guid(Guid)
import Data.Store.IRef (IRef)
import Data.Store.Rev.Change (Key, Value)
import Data.Store.Transaction (Transaction, Store(..))
import Editor.Anchors (DBTag)
import qualified Data.Store.Property as Property
import qualified Data.Store.Rev.Branch as Branch
import qualified Data.Store.Rev.Version as Version
import qualified Data.Store.Rev.View as View
import qualified Data.Store.Transaction as Transaction
import qualified Editor.Anchors as A
import qualified Editor.WidgetIds as WidgetIds
import qualified Graphics.UI.Bottle.Widget as Widget


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

-- Initialize an IRef if it does not already exist.
initRef :: (Binary a, Monad m) => IRef a -> Transaction t m a -> Transaction t m a
initRef iref act = do
  exists <- Transaction.irefExists iref
  unless exists (Property.set p =<< act)
  Property.get p
  where
    p = Transaction.fromIRef iref

builtinsDatabase :: [String]
builtinsDatabase =
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
  ,"Prelude.Set"
  ]

initDB :: Store DBTag IO -> IO ()
initDB store =
  Transaction.run store $ do
    bs <- initRef A.branchesIRef $ do
      masterNameIRef <- Transaction.newIRef "master"
      changes <- collectWrites Transaction.newKey $ do
        Property.set A.globals =<< mapM A.newBuiltin builtinsDatabase
        defI <- A.makeDefinition "foo"
        Property.set A.root [A.makePane defI]
        Property.set A.preJumps []
        Property.set A.preCursor $ WidgetIds.fromIRef defI
        Property.set A.postCursor $ WidgetIds.fromIRef defI
      initialVersionIRef <- Version.makeInitialVersion changes
      master <- Branch.new initialVersionIRef
      return [(masterNameIRef, master)]
    let branch = snd $ head bs
    _ <- initRef A.viewIRef $ View.new branch
    _ <- initRef A.currentBranchIRef (return branch)
    _ <- initRef A.redosIRef $ return []
    _ <- initRef A.cursorIRef . return $ Widget.Id []
    return ()
