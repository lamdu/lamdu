module Editor.ExampleDB(initDB) where

import Control.Monad (join, liftM, liftM2, unless, (<=<))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Writer (WriterT)
import Data.Binary (Binary(..))
import Data.Store.Guid (Guid)
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
import qualified Editor.Data.IRef as DataIRef
import qualified Editor.WidgetIds as WidgetIds

type WriteCollector m = WriterT [(Key, Value)] m

writeCollectorStore ::
  Monad m => m Guid -> Store t (WriteCollector m)
writeCollectorStore newKey = Store {
  storeNewKey = lift newKey,
  -- TODO: Eww! Hack! Remove collectWrites?
  storeLookup = \k -> return . Just . error $ "Attempt to use key: " ++ show k,
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
  unless exists (Transaction.writeIRef iref =<< act)
  Transaction.readIRef iref

newTodoIRef :: Monad m => Transaction t m (IRef a)
newTodoIRef = liftM IRef.unsafeFromGuid Transaction.newKey

fixIRef :: (Binary a, Monad m) => (IRef a -> Transaction t m a) -> Transaction t m (IRef a)
fixIRef createOuter = do
  x <- newTodoIRef
  Transaction.writeIRef x =<< createOuter x
  return x

createBuiltins :: Monad m => Transaction A.ViewTag m [Data.DefinitionIRef]
createBuiltins =
  Writer.execWriterT $ do
    list <- mkType . A.newBuiltin "Data.List.List" =<< lift setToSet
    let listOf = mkApply list
    bool <- mkType . A.newBuiltin "Prelude.Bool" =<< lift set

    makeWithType "Prelude.True" bool
    makeWithType "Prelude.False" bool

    makeWithType "Prelude.if" . forAll "a" $ \a ->
      mkPi bool . mkPi a $ endo a

    makeWithType "Prelude.id" $ forAll "a" endo

    makeWithType "Prelude.const" .
      forAll "a" $ \a -> forAll "b" $ \b -> mkPi a $ mkPi b a

    makeWithType "Data.List.reverse" $ forAll "a" (endo . listOf)
    makeWithType "Data.List.tail" $ forAll "a" (endo . listOf)

    makeWithType "Data.List.length" . forAll "a" $ \a ->
      mkPi (listOf a) integer

    makeWithType "Data.List.foldl" . forAll "a" $ \a -> forAll "b" $ \b ->
      mkPi (mkPi a (mkPi b a)) . mkPi a $ mkPi (listOf b) a

    makeWithType "Data.List.zipWith" . forAll "a" $ \a -> forAll "b" $ \b -> forAll "c" $ \c ->
      mkPi (mkPi a (mkPi b c)) . mkPi (listOf a) . mkPi (listOf b) $ listOf c

    let aToAToA = forAll "a" $ \a -> mkPi a $ endo a
    mapM_ ((`makeWithType` aToAToA) . ("Prelude." ++))
      ["+", "-", "*", "/", "^", "++"]
    makeWithType "Prelude.negate" $ forAll "a" endo

    let aToAToBool = forAll "a" $ \a -> mkPi a $ mkPi a bool
    mapM_ ((`makeWithType` aToAToBool) . ("Prelude." ++))
      ["==", "/=", "<=", ">=", "<", ">"]

    makeWithType "Prelude.enumFromTo" . mkPi integer . mkPi integer $ listOf integer

    makeWithType "Data.Functor.fmap" .
      forAll "f" $ \f ->
      forAll "a" $ \a ->
      forAll "b" $ \b ->
      mkPi (mkPi a b) . mkPi (mkApply f a) $ mkApply f b

    makeWithType "Data.List.iterate" .
      forAll "a" $ \a -> mkPi (mkPi a a) . mkPi a $ listOf a

    makeWithType "Control.Monad.return" .
      forAll "m" $ \m -> forAll "a" $ \a -> mkPi a $ mkApply m a

    makeWithType "Prelude.:" .
      forAll "a" $ \a -> mkPi a . endo $ listOf a
  where
    endo = join mkPi
    set = DataIRef.newExprBody $ Data.ExpressionLeaf Data.Set
    integer = DataIRef.newExprBody $ Data.ExpressionLeaf Data.IntegerType
    forAll name f = liftM Data.ExpressionIRef . fixIRef $ \aI -> do
      let aGuid = IRef.guid aI
      A.setP (A.assocNameRef aGuid) name
      s <- set
      return . Data.makePi s =<< f ((getVar . Data.ParameterRef) aGuid)
    setToSet = mkPi set set
    tellift f = Writer.tell . (:[]) =<< lift f
    getVar = DataIRef.newExprBody . Data.ExpressionLeaf . Data.GetVariable
    mkPi mkArgType mkResType =
      DataIRef.newExprBody =<< liftM2 Data.makePi mkArgType mkResType
    mkApply mkFunc mkArg =
      DataIRef.newExprBody =<< liftM2 Data.makeApply mkFunc mkArg
    mkType f = do
      x <- lift f
      Writer.tell [x]
      return . getVar $ Data.DefinitionRef x
    makeWithType builtinName typeMaker =
      tellift (A.newBuiltin builtinName =<< typeMaker)

setMkProp
  :: Monad m
  => m (Property.Property m a) -> a -> m ()
setMkProp mkProp val = do
  prop <- mkProp
  Property.set prop val

initDB :: Store DBTag IO -> IO ()
initDB store =
  Transaction.run store $ do
    bs <- initRef A.branchesIRef $ do
      masterNameIRef <- Transaction.newIRef "master"
      changes <- collectWrites Transaction.newKey $ do
        builtins <- createBuiltins
        setMkProp A.clipboards []
        setMkProp A.globals builtins
        setMkProp A.panes []
        setMkProp A.preJumps []
        setMkProp A.preCursor $ WidgetIds.fromIRef A.panesIRef
        setMkProp A.postCursor $ WidgetIds.fromIRef A.panesIRef
      initialVersionIRef <- Version.makeInitialVersion changes
      master <- Branch.new initialVersionIRef
      return [(masterNameIRef, master)]
    let branch = snd $ head bs
    view <- initRef A.viewIRef $ View.new branch
    _ <- initRef A.currentBranchIRef (return branch)
    _ <- initRef A.redosIRef $ return []
    _ <-
      initRef A.cursorIRef . Transaction.run (View.store view) .
      return $ WidgetIds.fromIRef A.panesIRef
    return ()
