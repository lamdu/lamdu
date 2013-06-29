module Lamdu.Data.ExampleDB(initDB, createBuiltins) where

import Control.Applicative (Applicative(..), liftA2, (<$>))
import Control.Lens.Operators
import Control.Monad (join, unless, void, (<=<))
import Control.Monad.Trans.Class (lift)
import Control.MonadA (MonadA)
import Data.Binary (Binary(..))
import Data.Foldable (traverse_)
import Data.List.Split (splitOn)
import Data.Store.Db (Db)
import Data.Store.IRef (IRef, Tag)
import Data.Store.Rev.Branch (Branch)
import Data.Store.Rev.Version (Version)
import Data.Store.Transaction (Transaction, setP)
import Data.Traversable (traverse)
import Lamdu.Data.Anchors (PresentationMode(..))
import qualified Control.Lens as Lens
import qualified Control.Monad.Trans.Writer as Writer
import qualified Data.Store.Guid as Guid
import qualified Data.Store.IRef as IRef
import qualified Data.Store.Rev.Branch as Branch
import qualified Data.Store.Rev.Version as Version
import qualified Data.Store.Rev.View as View
import qualified Data.Store.Transaction as Transaction
import qualified Lamdu.Data.DbLayout as Db
import qualified Lamdu.Data.Definition as Definition
import qualified Lamdu.Data.Expression as Expr
import qualified Lamdu.Data.Expression.IRef as ExprIRef
import qualified Lamdu.Data.Expression.Lens as ExprLens
import qualified Lamdu.Data.Expression.Utils as ExprUtil
import qualified Lamdu.Data.FFI as FFI
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.GUI.WidgetIdIRef as WidgetIdIRef

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
  MonadA m => Transaction m ((FFI.Env (Tag m), Db.SpecialFunctions (Tag m)), [ExprIRef.DefIM m])
createBuiltins =
  Writer.runWriterT $ do
    list <- mkDefinitionRef $ publicBuiltin "Data.List.List" setToSet
    let listOf = mkApply list
    bool <- mkDefinitionRef $ publicBuiltin "Prelude.Bool" set

    _ <-
      publicBuiltin "Data.Map.Map" $ mkPiRecord
      [ ("Key", set)
      , ("Value", set)
      ] set

    headTag <- lift $ namedTag "head" headTagGuid
    tailTag <- lift $ namedTag "tail" tailTagGuid
    cons <-
      publicBuiltin "Prelude.:" $ forAll "a" $ \a ->
      mkPi (mkRecordType pure [(headTag, a), (tailTag, listOf a)]) $ listOf a
    nil <- publicBuiltin "Prelude.[]" $ forAll "a" listOf

    true <- publicBuiltin "Prelude.True" bool
    false <- publicBuiltin "Prelude.False" bool

    traverse_ ((`publicBuiltin_` mkInfixType bool bool bool) . ("Prelude."++))
      ["&&", "||"]

    publicBuiltin_ "Prelude.if" . forAll "a" $ \a ->
      mkPiRecord
      [ ("condition", bool)
      , ("then", a)
      , ("else", a)
      ] a

    publicBuiltin_ "Prelude.id" $ forAll "a" endo

    publicBuiltin_ "Prelude.const" .
      forAll "a" $ \a ->
      forAll "b" $ \b ->
      mkPi a $ mkPi b a

    publicBuiltin_ "Data.Function.fix" . forAll "a" $ \a -> mkPi (mkPi a a) a

    publicBuiltin_ "Data.List.reverse" $ forAll "a" (endo . listOf)
    publicBuiltin_ "Data.List.tail" $ forAll "a" (endo . listOf)
    publicBuiltin_ "Data.List.head" . forAll "a" $ join (mkPi . listOf)
    publicBuiltin_ "Data.List.last" . forAll "a" $ join (mkPi . listOf)
    publicBuiltin_ "Data.List.null" . forAll "a" $ \a -> mkPi (listOf a) bool

    publicBuiltin_ "Data.List.length" . forAll "a" $ \a ->
      mkPi (listOf a) integer

    publicBuiltin_ "Prelude.product" . forAll "a" $ \a ->
      mkPi (listOf a) a
    publicBuiltin_ "Prelude.sum" . forAll "a" $ \a ->
      mkPi (listOf a) a
    publicBuiltin_ "Prelude.maximum" . forAll "a" $ \a ->
      mkPi (listOf a) a
    publicBuiltin_ "Prelude.minimum" . forAll "a" $ \a ->
      mkPi (listOf a) a

    let
      filterType predName =
        forAll "a" $ \a ->
        mkPiRecord
        [ ("from", listOf a)
        , (predName, mkPi a bool)
        ] $ listOf a
    publicDef_ "filter" Verbose ["Data", "List"] "filter" $ filterType "predicate"
    publicDef_ "take" Verbose ["Data", "List"] "takeWhile" $ filterType "while"

    publicDef_ "take" Verbose ["Data", "List"] "take" . forAll "a" $ \a ->
      mkPiRecord
      [ ("from", listOf a)
      , ("count", integer)
      ] $ listOf a

    publicBuiltin_ "Data.List.map" .
      forAll "a" $ \a ->
      forAll "b" $ \b ->
      mkPiRecord
      [ ("list", listOf a)
      , ("mapping", mkPi a b)
      ] $ listOf b

    publicBuiltin_ "Data.List.replicate" . forAll "a" $ \a ->
      mkPiRecord
      [ ("item", a)
      , ("count", integer)
      ] $ listOf a

    publicBuiltin_ "Data.List.foldl" . forAll "a" $ \a -> forAll "b" $ \b ->
      mkPiRecord
      [ ( "list", listOf b )
      , ( "initial", a )
      , ( "next"
        , mkPiRecord
          [ ("accumulator", a)
          , ("item", b)
          ] a
        )
      ] a

    publicBuiltin_ "Data.List.foldr" . forAll "a" $ \a -> forAll "b" $ \b ->
      mkPiRecord
      [ ( "list", listOf a )
      , ( "initial", b )
      , ( "next"
        , mkPiRecord
          [ ("item", a)
          , ("rest", b)
          ] b
        )
      ] b

    publicBuiltin_ "Data.List.listcase" . forAll "a" $ \a -> forAll "b" $ \b ->
      mkPiRecord
      [ ( "list", listOf a )
      , ( "empty", b )
      , ( "cons"
        , mkPiRecord
          [ ("item", a)
          , ("rest", listOf a)
          ] b
        )
      ] b

    publicBuiltin_ "Data.List.zipWith" . forAll "a" $ \a -> forAll "b" $ \b -> forAll "c" $ \c ->
      mkPiRecord
      [ ( "func", mkPiRecord [("x", a), ("y", b)] c)
      , ( "xs", listOf a )
      , ( "ys", listOf b )
      ] $ listOf c

    let aToAToA = forAll "a" $ \a -> mkInfixType a a a
    traverse_ ((`publicBuiltin_` aToAToA) . ("Prelude." ++))
      ["+", "-", "*", "/", "^", "++", "div"]
    publicDef_ "%" Infix ["Prelude"] "mod" aToAToA
    publicBuiltin_ "Prelude.negate" $ forAll "a" endo
    publicBuiltin_ "Prelude.sqrt" $ forAll "a" endo
    publicBuiltin_ "Prelude.floor" $ forAll "a" $ \a -> forAll "b" $ \b -> mkPi a b

    let aToAToBool = forAll "a" $ \a -> mkInfixType a a bool
    traverse_ ((`publicBuiltin_` aToAToBool) . ("Prelude." ++))
      ["==", "/=", "<=", ">=", "<", ">"]

    publicDef_ ".." Infix ["Prelude"] "enumFromTo" . mkInfixType integer integer $ listOf integer
    publicBuiltin_ "Prelude.enumFrom" . mkPi integer $ listOf integer

    publicDef_ "iterate" Verbose ["Data", "List"] "iterate" .
      forAll "a" $ \a ->
      mkPiRecord [("initial", a), ("step", endo a)] $ listOf a

    let
      specialFunctions = Db.SpecialFunctions
        { Db.sfNil = nil
        , Db.sfCons = cons
        , Db.sfHeadTag = headTagGuid
        , Db.sfTailTag = tailTagGuid
        }
      ffiEnv = FFI.Env
        { FFI.trueDef = true
        , FFI.falseDef = false
        }
    return (ffiEnv, specialFunctions)
  where
    publicDef_ name presentationMode ffiPath ffiName mkType =
      void $ publicDef name presentationMode ffiPath ffiName mkType
    publicDef name presentationMode ffiPath ffiName mkType = publicize $ do
      typeI <- mkType
      DataOps.newDefinition name presentationMode .
        (`Definition.Body` typeI) . Definition.ContentBuiltin .
        Definition.Builtin $ Definition.FFIName ffiPath ffiName
    publicBuiltin fullyQualifiedName =
      publicDef name (DataOps.presentationModeOfName name) path name
      where
        path = init fqPath
        name = last fqPath
        fqPath = splitOn "." fullyQualifiedName
    publicBuiltin_ builtinName typeMaker =
      void $ publicBuiltin builtinName typeMaker
    endo = join mkPi
    set = ExprIRef.newExprBody $ ExprLens.bodyType # ()
    integer = ExprIRef.newExprBody $ Expr.BodyLeaf Expr.IntegerType
    forAll name f = fmap ExprIRef.ExpressionI . fixIRef $ \aI -> do
      let aGuid = IRef.guid aI
      setP (Db.assocNameRef aGuid) name
      s <- set
      return . ExprUtil.makePi aGuid s =<<
        f ((ExprIRef.newExprBody . Lens.review ExprLens.bodyParameterRef) aGuid)
    setToSet = mkPi set set
    mkPi mkArgType mkResType = fmap snd . join $ liftA2 ExprIRef.newPi mkArgType mkResType
    mkApply mkFunc mkArg =
      ExprIRef.newExprBody =<< liftA2 ExprUtil.makeApply mkFunc mkArg
    newTag name = namedTag name . Guid.augment "ExampleDB" $ Guid.fromString name
    namedTag name tagGuid = do
      setP (Db.assocNameRef tagGuid) name
      ExprIRef.newExprBody $ ExprLens.bodyTag # tagGuid
    mkRecordType mkTag fields = do
      tagFields <- traverse (Lens._1 mkTag <=< Lens.sequenceOf Lens._2) fields
      ExprIRef.newExprBody $ Expr.BodyRecord Expr.Record
        { Expr._recordKind = Expr.KType
        , Expr._recordFields = tagFields
        }
    publicize f = do
      x <- lift f
      Writer.tell [x]
      return x
    mkPiRecord = mkPi . mkRecordType newTag
    mkInfixRecordType lType rType = do
      l <- namedTag "l" $ Guid.fromString "infixlarg"
      r <- namedTag "r" $ Guid.fromString "infixrarg"
      mkRecordType pure [(l, lType), (r, rType)]
    mkInfixType lType rType =
      mkPi $ mkInfixRecordType lType rType
    mkDefinitionRef f =
      ExprIRef.newExprBody . (ExprLens.bodyDefinitionRef # ) <$> f
    headTagGuid = Guid.fromString "headTag"
    tailTagGuid = Guid.fromString "tailTag"

newBranch :: MonadA m => String -> Version (Tag m) -> Transaction m (Branch (Tag m))
newBranch name ver = do
  branch <- Branch.new ver
  setP (Db.assocNameRef (Branch.guid branch)) name
  return branch

initDB :: Db -> IO ()
initDB db =
  Db.runDbTransaction db $ do
    exists <- Transaction.irefExists $ Db.branches Db.revisionIRefs
    unless exists $ do
      emptyVersion <- Version.makeInitialVersion []
      master <- newBranch "master" emptyVersion
      view <- View.new master
      let writeRevAnchor f = Transaction.writeIRef (f Db.revisionIRefs)
      writeRevAnchor Db.view view
      writeRevAnchor Db.branches [master]
      writeRevAnchor Db.currentBranch master
      writeRevAnchor Db.redos []
      let paneWId = WidgetIdIRef.fromIRef $ Db.panes Db.codeIRefs
      writeRevAnchor Db.cursor paneWId
      Db.runViewTransaction view $ do
        ((ffiEnv, specialFunctions), builtins) <- createBuiltins
        let writeCodeAnchor f = Transaction.writeIRef (f Db.codeIRefs)
        writeCodeAnchor Db.clipboards []
        writeCodeAnchor Db.specialFunctions specialFunctions
        writeCodeAnchor Db.ffiEnv ffiEnv
        writeCodeAnchor Db.globals builtins
        writeCodeAnchor Db.panes []
        writeCodeAnchor Db.preJumps []
        writeCodeAnchor Db.preCursor paneWId
        writeCodeAnchor Db.postCursor paneWId
        writeCodeAnchor Db.tags []
      -- Prevent undo into the invalid empty revision
      newVer <- Branch.curVersion master
      Version.preventUndo newVer
