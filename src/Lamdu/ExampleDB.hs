module Lamdu.ExampleDB(initDB) where

import Control.Applicative (liftA2)
import Control.Monad (join, unless)
import Control.Monad.Trans.Class (lift)
import Control.MonadA (MonadA)
import Data.Binary (Binary(..))
import Data.Foldable (traverse_)
import Data.Store.Db (Db)
import Data.Store.IRef (IRef, Tag)
import Data.Store.Rev.Branch (Branch)
import Data.Store.Rev.Version (Version)
import Data.Store.Transaction (Transaction)
import Lamdu.CodeEdit.Sugar.Config (SugarConfig(SugarConfig))
import qualified Control.Monad.Trans.Writer as Writer
import qualified Data.Store.IRef as IRef
import qualified Data.Store.Rev.Branch as Branch
import qualified Data.Store.Rev.Version as Version
import qualified Data.Store.Rev.View as View
import qualified Data.Store.Transaction as Transaction
import qualified Lamdu.Anchors as A
import qualified Lamdu.BranchGUI as BranchGUI
import qualified Lamdu.CodeEdit.FFI as FFI
import qualified Lamdu.CodeEdit.Sugar.Config as SugarConfig
import qualified Lamdu.Data as Data
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.Data.IRef as DataIRef
import qualified Lamdu.WidgetIds as WidgetIds

newTodoIRef :: MonadA m => Transaction m (IRef (Tag m) a)
newTodoIRef = fmap IRef.unsafeFromGuid Transaction.newKey

fixIRef ::
  (Binary a, MonadA m) =>
  (IRef (Tag m) a -> Transaction m a) ->
  Transaction m (IRef (Tag m) a)
fixIRef createOuter = do
  x <- newTodoIRef
  Transaction.writeIRef x =<< createOuter x
  return x

createBuiltins ::
  MonadA m => Transaction m ((FFI.Env (Tag m), SugarConfig (Tag m)), [DataIRef.DefI (Tag m)])
createBuiltins =
  Writer.runWriterT $ do
    list <- mkType . DataOps.newBuiltin "Data.List.List" =<< lift setToSet
    let listOf = mkApply list
    bool <- mkType . DataOps.newBuiltin "Prelude.Bool" =<< lift set

    cons <- lift $ DataOps.newBuiltin "Prelude.:" =<<
      forAll "a" (\a -> mkPi a . endo $ listOf a)
    nil <- lift $ DataOps.newBuiltin "Prelude.[]" =<< forAll "a" listOf
    Writer.tell [cons, nil]

    true <- makeWithType "Prelude.True" bool
    false <- makeWithType "Prelude.False" bool

    traverse_ ((`makeWithType_` mkPi bool (endo bool)) . ("Prelude."++))
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
    traverse_ ((`makeWithType_` aToAToA) . ("Prelude." ++))
      ["+", "-", "*", "/", "^", "++", "%", "mod", "div", "quot", "rem"]
    makeWithType_ "Prelude.negate" $ forAll "a" endo
    makeWithType_ "Prelude.sqrt" $ forAll "a" endo

    let aToAToBool = forAll "a" $ \a -> mkPi a $ mkPi a bool
    traverse_ ((`makeWithType_` aToAToBool) . ("Prelude." ++))
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

    tellift_ $ do
      typeI <-
        forAll "a" $ \a -> forAll "b" $ \b -> forAll "c" $ \c ->
        mkPi (mkPi b c) . mkPi (mkPi a b) $ mkPi a c
      -- Can't use newBuiltin in case of "."
      DataOps.newDefinition "." .
        (`Data.Definition` typeI) . Data.DefinitionBuiltin .
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
    forAll name f = fmap DataIRef.Expression . fixIRef $ \aI -> do
      let aGuid = IRef.guid aI
      A.setP (A.assocNameRef aGuid) name
      s <- set
      return . Data.makePi aGuid s =<< f ((getVar . Data.ParameterRef) aGuid)
    setToSet = mkPi set set
    tellift f = do
      x <- lift f
      Writer.tell [x]
      return x
    tellift_ = (fmap . fmap . const) () tellift
    getVar = DataIRef.newExprBody . Data.ExpressionLeaf . Data.GetVariable
    mkPi mkArgType mkResType = fmap snd . join $ liftA2 DataIRef.newPi mkArgType mkResType
    mkApply mkFunc mkArg =
      DataIRef.newExprBody =<< liftA2 Data.makeApply mkFunc mkArg
    mkType f = do
      x <- lift f
      Writer.tell [x]
      return . getVar $ Data.DefinitionRef x
    makeWithType_ = (fmap . fmap . fmap . const) () makeWithType
    makeWithType builtinName typeMaker =
      tellift (DataOps.newBuiltin builtinName =<< typeMaker)

newBranch :: MonadA m => String -> Version (Tag m) -> Transaction m (Branch (Tag m))
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
      Transaction.writeIRef A.viewIRef view
      Transaction.writeIRef A.branchesIRef [master]
      Transaction.writeIRef A.currentBranchIRef master
      Transaction.writeIRef A.redosIRef []
      Transaction.writeIRef A.cursorIRef $ WidgetIds.fromIRef A.panesIRef
      A.runViewTransaction view $ do
        ((ffiEnv, sugarConfig), builtins) <- createBuiltins
        Transaction.writeIRef A.clipboardsIRef []
        Transaction.writeIRef A.sugarConfigIRef sugarConfig
        Transaction.writeIRef A.ffiEnvIRef ffiEnv
        Transaction.writeIRef A.globalsIRef builtins
        Transaction.writeIRef A.panesIRef []
        Transaction.writeIRef A.preJumpsIRef []
        Transaction.writeIRef A.preCursorIRef $ WidgetIds.fromIRef A.panesIRef
        Transaction.writeIRef A.postCursorIRef $ WidgetIds.fromIRef A.panesIRef
      -- Prevent undo into the invalid empty revision
      newVer <- Branch.curVersion master
      Version.preventUndo newVer
