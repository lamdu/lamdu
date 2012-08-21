module Editor.ExampleDB(initDB) where

import Control.Monad (liftM, liftM2, unless, (<=<))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Writer (WriterT)
import Data.Binary (Binary(..))
import Data.Maybe (catMaybes)
import Data.Store.Guid(Guid)
import Data.Store.IRef (IRef)
import Data.Store.Rev.Change (Key, Value)
import Data.Store.Transaction (Transaction, Store(..))
import Editor.Anchors (DBTag)
import qualified Control.Monad.Trans.Writer as Writer
import qualified Data.Map as Map
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

createBuiltins :: Monad m => Transaction A.ViewTag m [Data.VariableRef]
createBuiltins =
  Writer.execWriterT $ do
    let
      setExpr = Data.newExprIRef $ Data.ExpressionLeaf Data.Set
    set <- mkType $ A.newDefinition "Set" =<< liftM2 Data.Definition setExpr setExpr
    let
      forAll name f = liftM Data.ExpressionIRef . fixIRef $ \aI -> do
        let aGuid = IRef.guid aI
        A.setP (A.assocNameRef aGuid) name
        s <- set
        return . Data.makePi s =<< f ((getVar . Data.ParameterRef) aGuid)
      setToSet = mkPi set set
    list <- mkType . A.newBuiltin "Data.List.List" =<< lift setToSet
    let
      listOf a = do
        l <- list
        Data.newExprIRef . Data.makeApply l =<< a

    let
      integerExpr = Data.newExprIRef $ Data.ExpressionLeaf Data.IntegerType
    integer <- mkType $ A.newDefinition "Integer" =<< liftM2 Data.Definition integerExpr setExpr
    bool <- mkType . A.newBuiltin "Prelude.Bool" =<< lift set

    makeWithType "Prelude.True" bool
    makeWithType "Prelude.False" bool

    makeWithType "Prelude.if" . forAll "a" $ \a ->
      mkPi bool . mkPi a $ mkPi a a

    makeWithType "Prelude.const" . forAll "a" $ \a -> forAll "b" $ \b ->
      mkPi a $ mkPi b a

    let endoListOfA = forAll "a" $ \a -> mkPi (listOf a) (listOf a)
    makeWithType "Data.List.reverse" endoListOfA
    makeWithType "Data.List.tail" endoListOfA

    makeWithType "Data.List.length" . forAll "a" $ \a ->
      mkPi (listOf a) integer

    makeWithType "Data.List.foldl" . forAll "a" $ \a -> forAll "b" $ \b ->
      mkPi (mkPi a (mkPi b a)) . mkPi a $ mkPi (listOf b) a

    makeWithType "Data.List.zipWith" . forAll "a" $ \a -> forAll "b" $ \b -> forAll "c" $ \c ->
      mkPi (mkPi a (mkPi b c)) . mkPi (listOf a) . mkPi (listOf b) $ listOf c

    let intToIntToInt = mkPi integer $ mkPi integer integer
    mapM_ ((`makeWithType` intToIntToInt) . ("Prelude." ++) . (:[])) "+-*/^"

    let intToIntToBool = mkPi integer $ mkPi integer bool
    mapM_ ((`makeWithType` intToIntToBool) . ("Prelude." ++))
      ["==", "/=", "<=", ">=", "<", ">"]

    makeWithType "Prelude.enumFromTo" . mkPi integer . mkPi integer $ listOf integer
  where
    tellift f = Writer.tell . (:[]) =<< lift f
    getVar = Data.newExprIRef . Data.ExpressionLeaf . Data.GetVariable
    mkPi mkArgType mkResType = do
      argType <- mkArgType
      Data.newExprIRef . Data.makePi argType =<< mkResType
    mkType f = do
      x <- lift f
      Writer.tell [x]
      return $ getVar x
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
        setMkProp A.builtinsMap . Map.fromList . catMaybes =<< mapM builtinsMapEntry builtins
        defI <- A.makeDefinition
        A.setP (A.assocNameRef (IRef.guid defI)) "foo"
        setMkProp A.panes [A.makePane defI]
        setMkProp A.preJumps []
        setMkProp A.preCursor $ WidgetIds.fromIRef defI
        setMkProp A.postCursor $ WidgetIds.fromIRef defI
      initialVersionIRef <- Version.makeInitialVersion changes
      master <- Branch.new initialVersionIRef
      return [(masterNameIRef, master)]
    let branch = snd $ head bs
    view <- initRef A.viewIRef $ View.new branch
    _ <- initRef A.currentBranchIRef (return branch)
    _ <- initRef A.redosIRef $ return []
    _ <- initRef A.cursorIRef . Transaction.run (View.store view) $ do
      (defI : _) <- A.getP A.panes
      return $ WidgetIds.fromIRef defI
    return ()
  where
    builtinsMapEntry (Data.ParameterRef _) = return Nothing
    builtinsMapEntry (Data.DefinitionRef defI) = do
      expr <- Data.readExprIRef . Data.defBody =<< Transaction.readIRef defI
      return $ case expr of
        Data.ExpressionBuiltin (Data.Builtin name _) -> Just (name, Data.DefinitionRef defI)
        Data.ExpressionLeaf Data.Set -> Just (Data.FFIName ["Core"] "Set", Data.DefinitionRef defI)
        _ -> Nothing
