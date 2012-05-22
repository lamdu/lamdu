module Editor.ExampleDB(initDB) where

import Control.Monad (liftM, unless, (<=<))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Writer (WriterT)
import Data.Binary (Binary(..))
import Data.Store.Guid(Guid)
import Data.Store.IRef (IRef)
import Data.Store.Rev.Change (Key, Value)
import Data.Store.Transaction (Transaction, Store(..))
import Editor.Anchors (DBTag)
import qualified Control.Monad.Trans.Writer as Writer
import qualified Data.Store.IRef as IRef
import qualified Data.Store.Property as Property
import qualified Data.Store.Rev.Branch as Branch
import qualified Data.Store.Rev.Version as Version
import qualified Data.Store.Rev.View as View
import qualified Data.Store.Transaction as Transaction
import qualified Editor.Anchors as A
import qualified Editor.Data as Data
import qualified Editor.WidgetIds as WidgetIds

type WriteCollector m = WriterT [(Key, Value)] m

writeCollectorStore ::
  Monad m => m Guid -> Store t (WriteCollector m)
writeCollectorStore newKey = Store {
  storeNewKey = lift newKey,
  storeLookup = fail "Cannot lookup when collecting writes!",
  storeAtomicWrite = Writer.tell <=< mapM makeChange
  }
  where
    makeChange (key, Just value) = return (key, value)
    makeChange (_, Nothing) =
      fail "Cannot delete when collecting writes"

collectWrites ::
  Monad m =>
  m Guid -> Transaction t (WriteCollector m) () -> m [(Key, Value)]
collectWrites newGuid =
  Writer.execWriterT . Transaction.run (writeCollectorStore newGuid)

-- Initialize an IRef if it does not already exist.
initRef :: (Binary a, Monad m) => IRef a -> Transaction t m a -> Transaction t m a
initRef iref act = do
  exists <- Transaction.irefExists iref
  unless exists (Property.set p =<< act)
  Property.get p
  where
    p = Transaction.fromIRef iref

newTodoIRef :: Monad m => Transaction t m (IRef a)
newTodoIRef = liftM IRef.unsafeFromGuid Transaction.newKey

fixIRef :: (Binary a, Monad m) => (IRef a -> Transaction t m a) -> Transaction t m (IRef a)
fixIRef createOuter = do
  x <- newTodoIRef
  Transaction.writeIRef x =<< createOuter x
  return x

createBuiltins :: Monad m => Transaction t m [Data.VariableRef]
createBuiltins =
  Writer.execWriterT $ do
    magicI <- lift . fixIRef $ \magicI -> do
      Property.set (A.aNameRef (IRef.guid magicI)) "Magic"
      magicE <- getVar $ Data.DefinitionRef magicI
      return $ Data.DefinitionBuiltin Data.Builtin
        { Data.biName = Data.FFIName ["Core"] "Magic"
        , Data.biType = magicE
        }

    let magic = getVar (Data.DefinitionRef magicI)

    set <- mkType . A.newBuiltin "Core.Set" =<< lift magic
    let
      forAll name f = fixIRef $ \aI -> do
        Property.set (A.aNameRef (IRef.guid aI)) name
        s <- set
        return . Data.ExpressionPi . Data.Lambda s =<< f ((getVar . Data.ParameterRef) aI)
      setToSet = mkPi set set
    list <- mkType . A.newBuiltin "Data.List.List" =<< lift setToSet
    let
      listOf a = do
        l <- list
        Transaction.newIRef . Data.ExpressionApply . Data.Apply l =<< a

    integer <- mkType . A.newBuiltin "Prelude.Integer" =<< lift set
    bool <- mkType . A.newBuiltin "Prelude.Bool" =<< lift set

    makeWithType "Prelude.if" . forAll "a" $ \a ->
      mkPi bool . mkPi a $ mkPi a a

    makeWithType "Prelude.const" . forAll "a" $ \a -> forAll "b" $ \b ->
      mkPi a $ mkPi b a

    let endoListOfA = forAll "a" $ \a -> mkPi (listOf a) (listOf a)
    makeWithType "Data.List.reverse" endoListOfA
    makeWithType "Data.List.tail" endoListOfA

    makeWithType "Data.List.length" . forAll "a" $ \a ->
      mkPi (listOf a) integer

    makeWithType "Data.List.zipWith" . forAll "a" $ \a -> forAll "b" $ \b -> forAll "c" $ \c ->
      mkPi (mkPi a (mkPi b c)) . mkPi (listOf a) . mkPi (listOf b) $ listOf c

    let intToIntToInt = mkPi integer $ mkPi integer integer
    mapM_ ((`makeWithType` intToIntToInt) . ("Prelude." ++) . (:[])) "+-*/^"

    let intToIntToBool = mkPi integer $ mkPi integer bool
    mapM_ ((`makeWithType` intToIntToBool) . ("Prelude." ++))
      ["==", "/=", "<=", ">=", "<", ">"]
  where
    tellift f = Writer.tell . (:[]) =<< lift f
    getVar = Transaction.newIRef . Data.ExpressionGetVariable
    mkPi mkArgType mkResType = do
      argType <- mkArgType
      Transaction.newIRef . Data.ExpressionPi . Data.Lambda argType =<< mkResType
    mkType f = do
      x <- lift f
      Writer.tell [x]
      return $ getVar x
    makeWithType builtinName typeMaker =
      tellift (A.newBuiltin builtinName =<< typeMaker)

initDB :: Store DBTag IO -> IO ()
initDB store =
  Transaction.run store $ do
    bs <- initRef A.branchesIRef $ do
      masterNameIRef <- Transaction.newIRef "master"
      changes <- collectWrites Transaction.newKey $ do
        builtins <- createBuiltins
        Property.set A.globals builtins
        defI <- A.makeDefinition "foo"
        Property.set A.panes [A.makePane defI]
        Property.set A.preJumps []
        Property.set A.preCursor $ WidgetIds.fromIRef defI
        Property.set A.postCursor $ WidgetIds.fromIRef defI
      initialVersionIRef <- Version.makeInitialVersion changes
      master <- Branch.new initialVersionIRef
      return [(masterNameIRef, master)]
    let branch = snd $ head bs
    view <- initRef A.viewIRef $ View.new branch
    _ <- initRef A.currentBranchIRef (return branch)
    _ <- initRef A.redosIRef $ return []
    _ <- initRef A.cursorIRef . Transaction.run (View.store view) $ do
      (A.Pane paneCursor _ : _) <- Property.get A.panes
      return paneCursor
    return ()
