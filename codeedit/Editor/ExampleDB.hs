module Editor.ExampleDB(initDB) where

import Control.Monad (join, liftM, liftM2, unless)
import Control.Monad.Trans.Class (lift)
import Data.Binary (Binary(..))
import Data.Store.Db (Db)
import Data.Store.IRef (IRef)
import Data.Store.Rev.Branch (Branch)
import Data.Store.Rev.Version (Version)
import Data.Store.Transaction (Transaction)
import Editor.CodeEdit.Sugar.Config (SugarConfig(SugarConfig))
import qualified Control.Monad.Trans.Writer as Writer
import qualified Data.Store.IRef as IRef
import qualified Data.Store.Rev.Branch as Branch
import qualified Data.Store.Rev.Version as Version
import qualified Data.Store.Rev.View as View
import qualified Data.Store.Transaction as Transaction
import qualified Editor.Anchors as A
import qualified Editor.BranchGUI as BranchGUI
import qualified Editor.CodeEdit.FFI as FFI
import qualified Editor.CodeEdit.Sugar.Config as SugarConfig
import qualified Editor.Data as Data
import qualified Editor.Data.IRef as DataIRef
import qualified Editor.WidgetIds as WidgetIds

newTodoIRef :: Monad m => Transaction m (IRef a)
newTodoIRef = liftM IRef.unsafeFromGuid Transaction.newKey

fixIRef :: (Binary a, Monad m) => (IRef a -> Transaction m a) -> Transaction m (IRef a)
fixIRef createOuter = do
  x <- newTodoIRef
  Transaction.writeIRef x =<< createOuter x
  return x

createBuiltins :: Monad m => Transaction m ((FFI.Env, SugarConfig), [Data.DefinitionIRef])
createBuiltins =
  Writer.runWriterT $ do
    list <- mkType . A.newBuiltin "Data.List.List" =<< lift setToSet
    let listOf = mkApply list
    bool <- mkType . A.newBuiltin "Prelude.Bool" =<< lift set

    cons <- lift $ A.newBuiltin "Prelude.:" =<<
      forAll "a" (\a -> mkPi a . endo $ listOf a)
    nil <- lift $ A.newBuiltin "Prelude.[]" =<< forAll "a" listOf
    Writer.tell [cons, nil]

    true <- makeWithType "Prelude.True" bool
    false <- makeWithType "Prelude.False" bool

    mapM_ ((`makeWithType_` mkPi bool (endo bool)) . ("Prelude."++))
      ["&&", "||"]

    makeWithType_ "Prelude.if" . forAll "a" $ \a ->
      mkPi bool . mkPi a $ endo a

    makeWithType_ "Prelude.id" $ forAll "a" endo

    makeWithType_ "Prelude.const" .
      forAll "a" $ \a -> forAll "b" $ \b -> mkPi a $ mkPi b a

    makeWithType_ "Data.List.reverse" $ forAll "a" (endo . listOf)
    makeWithType_ "Data.List.tail" $ forAll "a" (endo . listOf)
    makeWithType_ "Data.List.head" . forAll "a" $ join (mkPi . listOf)

    makeWithType_ "Data.List.length" . forAll "a" $ \a ->
      mkPi (listOf a) integer

    makeWithType_ "Prelude.product" . forAll "a" $ \a ->
      mkPi (listOf a) a
    makeWithType_ "Prelude.sum" . forAll "a" $ \a ->
      mkPi (listOf a) a

    makeWithType_ "Data.List.filter" . forAll "a" $ \a ->
      mkPi (mkPi a bool) . endo $ listOf a

    makeWithType_ "Data.List.replicate" . forAll "a" $ \a ->
      mkPi integer . mkPi a $ listOf a

    makeWithType_ "Data.List.foldl" . forAll "a" $ \a -> forAll "b" $ \b ->
      mkPi (mkPi a (mkPi b a)) . mkPi a $ mkPi (listOf b) a

    makeWithType_ "Data.List.zipWith" . forAll "a" $ \a -> forAll "b" $ \b -> forAll "c" $ \c ->
      mkPi (mkPi a (mkPi b c)) . mkPi (listOf a) . mkPi (listOf b) $ listOf c

    let aToAToA = forAll "a" $ \a -> mkPi a $ endo a
    mapM_ ((`makeWithType_` aToAToA) . ("Prelude." ++))
      ["+", "-", "*", "/", "^", "++", "mod", "div", "quot", "rem"]
    makeWithType_ "Prelude.negate" $ forAll "a" endo
    makeWithType_ "Prelude.sqrt" $ forAll "a" endo

    let aToAToBool = forAll "a" $ \a -> mkPi a $ mkPi a bool
    mapM_ ((`makeWithType_` aToAToBool) . ("Prelude." ++))
      ["==", "/=", "<=", ">=", "<", ">"]

    makeWithType_ "Prelude.enumFromTo" . mkPi integer . mkPi integer $ listOf integer

    makeWithType_ "Data.Functor.fmap" .
      forAll "f" $ \f ->
      forAll "a" $ \a ->
      forAll "b" $ \b ->
      mkPi (mkPi a b) . mkPi (mkApply f a) $ mkApply f b

    makeWithType_ "Data.List.iterate" .
      forAll "a" $ \a -> mkPi (mkPi a a) . mkPi a $ listOf a

    makeWithType_ "Control.Monad.return" .
      forAll "m" $ \m -> forAll "a" $ \a -> mkPi a $ mkApply m a

    -- Can't use convinience path functions in case of "."
    tellift_ $ do
      typeI <-
        forAll "a" $ \a -> forAll "b" $ \b -> forAll "c" $ \c ->
        mkPi (mkPi b c) . mkPi (mkPi a b) $ mkPi a c
      A.newDefinition "." . (`Data.Definition` typeI) . Data.DefinitionBuiltin .
        Data.Builtin $ Data.FFIName ["Prelude"] "."

    let
      sugarConfig = SugarConfig
        { SugarConfig.cons = cons
        , SugarConfig.nil = nil
        }
      ffiEnv = FFI.Env
        { FFI.trueDef = true
        , FFI.falseDef = false
        }
    return (ffiEnv, sugarConfig)
  where
    endo = join mkPi
    set = DataIRef.newExprBody $ Data.ExpressionLeaf Data.Set
    integer = DataIRef.newExprBody $ Data.ExpressionLeaf Data.IntegerType
    forAll name f = liftM Data.ExpressionIRef . fixIRef $ \aI -> do
      let aGuid = IRef.guid aI
      A.setP (A.assocNameRef aGuid) name
      s <- set
      return . Data.makePi aGuid s =<< f ((getVar . Data.ParameterRef) aGuid)
    setToSet = mkPi set set
    tellift f = do
      x <- lift f
      Writer.tell [x]
      return x
    tellift_ = (fmap . liftM . const) () tellift
    getVar = DataIRef.newExprBody . Data.ExpressionLeaf . Data.GetVariable
    mkPi mkArgType mkResType = liftM snd . join $ liftM2 DataIRef.newPi mkArgType mkResType
    mkApply mkFunc mkArg =
      DataIRef.newExprBody =<< liftM2 Data.makeApply mkFunc mkArg
    mkType f = do
      x <- lift f
      Writer.tell [x]
      return . getVar $ Data.DefinitionRef x
    makeWithType_ = (fmap . fmap . liftM . const) () makeWithType
    makeWithType builtinName typeMaker =
      tellift (A.newBuiltin builtinName =<< typeMaker)

newBranch :: Monad m => String -> Version -> Transaction m Branch
newBranch name ver = do
  branch <- Branch.new ver
  A.setP (BranchGUI.branchNameProp branch) name
  return branch

initDB :: Db -> IO ()
initDB db =
  A.runDbTransaction db $ do
    exists <- Transaction.irefExists A.branchesIRef
    unless exists $ do
      emptyVersion <- Version.makeInitialVersion []
      master <- newBranch "master" emptyVersion
      view <- View.new master
      A.setP A.view view
      A.setP A.branches [master]
      A.setP A.currentBranch master
      A.setP A.redos []
      A.setP A.cursor $ WidgetIds.fromIRef A.panesIRef
      A.runViewTransaction view $ do
        ((ffiEnv, sugarConfig), builtins) <- createBuiltins
        A.setP A.clipboards []
        A.setP A.sugarConfig sugarConfig
        A.setP A.ffiEnv ffiEnv
        A.setP A.globals builtins
        A.setP A.panes []
        A.setP A.preJumps []
        A.setP A.preCursor $ WidgetIds.fromIRef A.panesIRef
        A.setP A.postCursor $ WidgetIds.fromIRef A.panesIRef
      -- Prevent undo into the invalid empty revision
      newVer <- Branch.curVersion master
      Version.preventUndo newVer
