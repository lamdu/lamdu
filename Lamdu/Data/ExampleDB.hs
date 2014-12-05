{-# LANGUAGE OverloadedStrings #-}
module Lamdu.Data.ExampleDB(initDB, createBuiltins) where

import Control.Monad (unless, void)
import Control.Monad.Trans.Class (lift)
import Control.MonadA (MonadA)
import Data.Foldable (traverse_)
import Data.List.Split (splitOn)
import Data.Monoid (Monoid(..))
import Data.Store.Db (Db)
import Data.Store.Rev.Branch (Branch)
import Data.Store.Rev.Version (Version)
import Data.Store.Transaction (Transaction, setP)
import Data.String (IsString(..))
import Lamdu.Data.Anchors (PresentationMode(..))
import Lamdu.Expr.Scheme (Scheme(..))
import Lamdu.Expr.Type (Type, (~>))
import qualified Control.Monad.Trans.Writer as Writer
import qualified Data.Map as Map
import qualified Data.Store.Rev.Branch as Branch
import qualified Data.Store.Rev.Version as Version
import qualified Data.Store.Rev.View as View
import qualified Data.Store.Transaction as Transaction
import qualified Lamdu.Data.DbLayout as Db
import qualified Lamdu.Data.Definition as Definition
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.Scheme as Scheme
import qualified Lamdu.Expr.Type as T
import qualified Lamdu.Expr.TypeVars as TypeVars
import qualified Lamdu.Expr.UniqueId as UniqueId
import qualified Lamdu.GUI.WidgetIdIRef as WidgetIdIRef

type T = Transaction

namedId :: (MonadA m, IsString a, UniqueId.ToGuid a) => String -> T m a
namedId name = do
  setP (Db.assocNameRef tag) name
  return tag
  where
    tag = fromString name

forAll :: TypeVars.VarKind a => Int -> ([a] -> Type) -> Scheme
forAll count f =
  Scheme
  { schemeForAll = mconcat $ map TypeVars.singleton typeVars
  , schemeConstraints = mempty
  , schemeType = f $ map T.liftVar typeVars
  }
  where
    typeVars = take count $ map (fromString . (:[])) ['a'..'z']

recordType :: [(T.Tag, Type)] -> Type
recordType = T.TRecord . foldr (uncurry T.CExtend) T.CEmpty

createBuiltins ::
  MonadA m => T m (Db.SpecialFunctions m, [ExprIRef.DefI m])
createBuiltins =
  Writer.runWriterT $ do
    let newTag x = lift $ namedId x

    listTag <- newTag "List"
    valTag <- newTag "val"
    let list x = T.TInst listTag $ Map.singleton valTag x

    headTag <- newTag "head"
    tailTag <- newTag "tail"
    nonEmpty <-
      publicBuiltin "Prelude.:" $ forAll 1 $ \[a] ->
      recordType [(headTag, a), (tailTag, list a)] ~> list a
    nil <- publicBuiltin "Prelude.[]" $ forAll 1 $ \[a] -> list a
    publicBuiltin_ "Data.List.tail" $ forAll 1 $ \[a] -> list a ~> list a
    publicBuiltin_ "Data.List.head" . forAll 1 $ \[a] -> list a ~> a

    maybeTag <- newTag "Maybe"
    let maybe_ x = T.TInst maybeTag $ Map.singleton valTag x
    publicBuiltin_ "Prelude.Just" $ forAll 1 $ \[a] -> a ~> maybe_ a
    publicBuiltin_ "Prelude.Nothing" $ forAll 1 $ \[a] -> maybe_ a

    objTag <- newTag "object" -- OO hides this

    nothingTag <- newTag "Nothing"
    justTag <- newTag "Just"
    publicBuiltin_ "Data.Maybe.caseMaybe" . forAll 2 $ \[a, b] ->
      recordType
      [ ( objTag, maybe_ a )
      , ( nothingTag, b )
      , ( justTag, a ~> b )
      ] ~> b

    intTag <- newTag "Int"
    let integer = T.TInst intTag Map.empty

    boolTag <- newTag "Bool"
    let bool = T.TInst boolTag Map.empty

    true <- publicBuiltin "Prelude.True" $ Scheme.mono bool
    false <- publicBuiltin "Prelude.False" $ Scheme.mono bool

    publicBuiltin_ "Prelude.not" $ Scheme.mono $ bool ~> bool

    infixlTag <- newTag "infixl"
    infixrTag <- newTag "infixr"
    let
      infixType lType rType resType =
        recordType [(infixlTag, lType), (infixrTag, rType)] ~> resType

    traverse_ ((`publicBuiltin_` Scheme.mono (infixType bool bool bool)) . ("Prelude."++))
      ["&&", "||"]

    trueTag <- newTag "True"
    falseTag <- newTag "False"
    publicBuiltin_ "Prelude.if" . forAll 1 $ \[a] ->
      recordType
      [ (objTag, bool)
      , (trueTag, a)
      , (falseTag, a)
      ] ~> a

    publicBuiltin_ "Prelude.id" $ forAll 1 $ \[a] -> a ~> a

    publicBuiltin_ "Prelude.const" .
      forAll 2 $ \[a, b] ->
      a ~> b ~> a

    publicBuiltin_ "Data.Function.fix" . forAll 1 $ \[a] ->
      (a ~> a) ~> a

    publicBuiltin_ "Data.List.reverse" $ forAll 1 $ \[a] -> list a ~> list a
    publicBuiltin_ "Data.List.last" $ forAll 1 $ \[a] -> list a ~> a
    publicBuiltin_ "Data.List.null" $ forAll 1 $ \[a] -> list a ~> bool

    publicBuiltin_ "Data.List.length" . forAll 1 $ \[a] -> list a ~> integer

    traverse_ ((`publicBuiltin_` Scheme.mono (list integer ~> integer)) . ("Prelude."++))
      ["product", "sum", "maximum", "minimum"]

    fromTag <- newTag "from"

    predicateTag <- newTag "predicate"
    publicDef_ "filter" Verbose ["Data", "List"] "filter" $
      forAll 1 $ \[a] ->
      recordType
      [ (fromTag, list a)
      , (predicateTag, a ~> bool)
      ] ~> list a

    whileTag <- newTag "while"
    publicDef_ "take" Verbose ["Data", "List"] "takeWhile" $
      forAll 1 $ \[a] ->
      recordType
      [ (fromTag, list a)
      , (whileTag, a ~> bool)
      ] ~> list a

    countTag <- newTag "count"
    publicDef_ "take" Verbose ["Data", "List"] "take" . forAll 1 $ \[a] ->
      recordType
      [ (fromTag, list a)
      , (countTag, integer)
      ] ~> list a

    mappingTag <- newTag "mapping"
    publicBuiltin_ "Data.List.map" .
      forAll 2 $ \[a, b] ->
      recordType
      [ (objTag, list a)
      , (mappingTag, a ~> b)
      ] ~> list b

    publicBuiltin_ "Data.List.concat" . forAll 1 $ \[a] -> list (list a) ~> list a

    publicBuiltin_ "Data.List.replicate" . forAll 1 $ \[a] ->
      recordType
      [ (objTag, a)
      , (countTag, integer)
      ] ~> list a

    initialTag     <- newTag "initial"
    stepTag        <- newTag "step"
    accumulatorTag <- newTag "accumulator"
    itemTag        <- newTag "item"
    publicBuiltin_ "Data.List.foldl" . forAll 2 $ \[a, b] ->
      recordType
      [ ( objTag, list b )
      , ( initialTag, a )
      , ( stepTag
        , recordType
          [ (accumulatorTag, a)
          , (itemTag, b)
          ] ~> a
        )
      ] ~> a

    emptyTag <- newTag "empty"
    prependTag <- newTag "prepend"
    publicBuiltin_ "Data.List.foldr" . forAll 2 $ \[a, b] ->
      recordType
      [ ( objTag, list a )
      , ( emptyTag, b )
      , ( prependTag
        , recordType
          [ (headTag, a)
          , (tailTag, b)
          ] ~> b
        )
      ] ~> b

    publicBuiltin_ "Data.List.caseList" . forAll 2 $ \[a, b] ->
      recordType
      [ ( objTag, list a )
      , ( emptyTag, b )
      , ( prependTag
        , recordType
          [ (headTag, a)
          , (tailTag, list a)
          ] ~> b
        )
      ] ~> b

    funcTag <- newTag "func"
    xTag <- newTag "x"
    yTag <- newTag "y"
    publicBuiltin_ "Data.List.zipWith" . forAll 3 $ \[a, b, c] ->
      recordType
      [ ( funcTag, recordType [(xTag, a), (yTag, b)] ~> c)
      , ( xTag, list a )
      , ( yTag, list b )
      ] ~> list c

    traverse_
      ((`publicBuiltin_` Scheme.mono (infixType integer integer integer)) .
       ("Prelude." ++))
      ["+", "-", "*", "/", "^", "div"]
    publicBuiltin_ "Prelude.++" $ forAll 1 $ \[a] -> infixType (list a) (list a) (list a)
    publicDef_ "%" Infix ["Prelude"] "mod" $ Scheme.mono $ infixType integer integer integer
    publicBuiltin_ "Prelude.negate" $ Scheme.mono $ integer ~> integer
    publicBuiltin_ "Prelude.sqrt" $ Scheme.mono $ integer ~> integer

    let aToAToBool = forAll 1 $ \[a] -> infixType a a bool
    traverse_ ((`publicBuiltin_` aToAToBool) . ("Prelude." ++))
      ["==", "/=", "<=", ">=", "<", ">"]

    publicDef_ ".." Infix ["Prelude"] "enumFromTo" .
      Scheme.mono . infixType integer integer $ list integer
    publicBuiltin_ "Prelude.enumFrom" $ Scheme.mono $ integer ~> list integer

    publicDef_ "iterate" Verbose ["Data", "List"] "iterate" .
      forAll 1 $ \[a] ->
      recordType [(initialTag, a), (stepTag, a ~> a)] ~> list a

    return
      Db.SpecialFunctions
        { Db.sfNil = nil
        , Db.sfCons = nonEmpty
        , Db.sfHeadTag = headTag
        , Db.sfTailTag = tailTag
        , Db.sfFalse = false
        , Db.sfTrue = true
        }
  where
    publicDef_ name presentationMode ffiPath ffiName typ =
      void $ publicDef name presentationMode ffiPath ffiName typ
    publicDef name presentationMode ffiPath ffiName typ =
      publicize $
      DataOps.newDefinition name presentationMode .
      (`Definition.Body` Definition.ExportedType typ) . Definition.ContentBuiltin .
      Definition.Builtin $ Definition.FFIName ffiPath ffiName
    publicBuiltin fullyQualifiedName =
      publicDef name (DataOps.presentationModeOfName name) path name
      where
        path = init fqPath
        name = last fqPath
        fqPath = splitOn "." fullyQualifiedName
    publicBuiltin_ builtinName typ =
      void $ publicBuiltin builtinName typ
    publicize f = do
      x <- lift f
      Writer.tell [x]
      return x

newBranch :: MonadA m => String -> Version m -> T m (Branch m)
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
        (specialFunctions, builtins) <- createBuiltins
        let writeCodeAnchor f = Transaction.writeIRef (f Db.codeIRefs)
        writeCodeAnchor Db.clipboards []
        writeCodeAnchor Db.specialFunctions specialFunctions
        writeCodeAnchor Db.globals builtins
        writeCodeAnchor Db.panes []
        writeCodeAnchor Db.preJumps []
        writeCodeAnchor Db.preCursor paneWId
        writeCodeAnchor Db.postCursor paneWId
        writeCodeAnchor Db.tags []
      -- Prevent undo into the invalid empty revision
      newVer <- Branch.curVersion master
      Version.preventUndo newVer
