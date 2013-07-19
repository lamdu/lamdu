{-# OPTIONS -Wall -Werror #-}
module InferCombinators where

import Control.Arrow ((***))
import Control.Lens (Lens')
import Control.Lens.Operators
import Control.Monad (void)
import Data.Map ((!))
import Data.Maybe (fromMaybe)
import Data.Store.Guid (Guid)
import InferWrappers
import Lamdu.Data.Arbitrary () -- Arbitrary instance
import Lamdu.Data.Expression (Kind(..))
import Lamdu.Data.Expression.Utils (pureHole, pureSet, pureIntegerType)
import Utils
import qualified Control.Lens as Lens
import qualified Data.Store.Guid as Guid
import qualified Lamdu.Data.Expression as Expr
import qualified Lamdu.Data.Expression.Lens as ExprLens
import qualified Lamdu.Data.Expression.Utils as ExprUtil

iexpr ::
  Expr ->
  Expr ->
  Expr.Body Def ExprInferred -> ExprInferred
iexpr val typ body =
  Expr.Expression body (val, typ)

five :: Expr
five = pureLiteralInt # 5

iVal :: Lens' ExprInferred Expr
iVal = Expr.ePayload . Lens._1

iType :: Lens' ExprInferred Expr
iType = Expr.ePayload . Lens._2

bodyToPureExpr :: Expr.Body Def ExprInferred -> Expr
bodyToPureExpr exprBody = ExprLens.pureExpr # fmap (^. iVal) exprBody

-- inferred-val is simply equal to the expr. Type is given
simple :: Expr.Body Def ExprInferred -> Expr -> ExprInferred
simple body typ = iexpr (bodyToPureExpr body) typ body

getParamPure :: String -> Expr -> ExprInferred
getParamPure name = simple $ ExprLens.bodyParameterRef # Guid.fromString name

getRecursiveDef :: Expr
getRecursiveDef =
  ExprLens.pureExpr . ExprLens.bodyDefinitionRef # recursiveDefI

-- New-style:
recurse :: ExprInferred -> ExprInferred
recurse typ = simple (ExprLens.bodyDefinitionRef # recursiveDefI) $ typ ^. iVal

literalInteger :: Integer -> ExprInferred
literalInteger x = simple (ExprLens.bodyLiteralInteger # x) pureIntegerType

piType ::
  String -> ExprInferred ->
  (ExprInferred -> ExprInferred) -> ExprInferred
piType name paramType mkResultType =
  simple (ExprUtil.makePi (Guid.fromString name) paramType result) pureSet
  where
    result = mkResultType $ getParam name paramType

infixr 4 ~>
(~>) :: ExprInferred -> ExprInferred -> ExprInferred
(~>) src dest =
  simple (ExprUtil.makePi (Guid.fromString "") src dest) pureSet

lambda ::
  String -> ExprInferred ->
  (ExprInferred -> ExprInferred) ->
  ExprInferred
lambda name paramType mkResult =
  simple (ExprUtil.makeLambda guid paramType result) $
  ExprUtil.pureLam KType guid (paramType ^. iVal) (result ^. iType)
  where
    guid = Guid.fromString name
    result = mkResult $ getParam name paramType

-- Sometimes we have an inferred type that comes outside-in but cannot
-- be inferred inside-out:
setInferredType :: ExprInferred -> ExprInferred -> ExprInferred
setInferredType typ val = val & iType .~ typ ^. iVal

getField :: ExprInferred -> ExprInferred -> ExprInferred
getField recordVal tagVal
  | allFieldsMismatch = error "getField on record with only mismatching field tags"
  | otherwise = simple (Expr._BodyGetField # Expr.GetField recordVal tagVal) pureFieldType
  where
    mFields = recordVal ^? iType . ExprLens.exprKindedRecordFields KType
    allFieldsMismatch =
      case mFields of
      Nothing -> False
      Just fields -> all (tagMismatch . fst) fields
    tagMismatch fieldTag =
      Lens.nullOf ExprLens.exprHole fieldTag &&
      Lens.nullOf ExprLens.exprHole tagVal &&
      (fieldTag ^?! ExprLens.exprTag /= tagVal ^?! ExprLens.exprTag)
    mPureFieldType tagGuid =
      mFields ^?
      Lens._Just . Lens.traversed .
      Lens.filtered
      (Lens.has (Lens._1 . ExprLens.exprTag . Lens.filtered (tagGuid ==))) .
      Lens._2
    pureFieldType =
      fromMaybe pureHole $
      mPureFieldType =<< tagVal ^? ExprLens.exprTag

lambdaRecord ::
  String -> [(String, ExprInferred)] ->
  ([ExprInferred] -> ExprInferred) -> ExprInferred
lambdaRecord paramsName strFields mkResult =
  lambda paramsName (record KType fields) $ \params ->
  mkResult $ map (getField params) fieldTags
  where
    fields = strFields & Lens.traversed . Lens._1 %~ tagStr
    fieldTags = map fst fields

whereItem ::
  String -> ExprInferred -> (ExprInferred -> ExprInferred) -> ExprInferred
whereItem name val mkBody =
  lambda name (iexpr (val ^. iType) pureSet bodyHole) mkBody $$ val

typedWhereItem ::
  String -> ExprInferred -> ExprInferred -> (ExprInferred -> ExprInferred) -> ExprInferred
typedWhereItem name typ val mkBody =
  lambda name typ mkBody $$ val

holeWithInferredType :: ExprInferred -> ExprInferred
holeWithInferredType = simple bodyHole . (^. iVal)

hole :: ExprInferred
hole = simple bodyHole pureHole

getGuidParam :: Guid -> ExprInferred -> ExprInferred
getGuidParam guid typ =
  simple (ExprLens.bodyParameterRef # guid) $
  typ ^. iVal

getParam :: String -> ExprInferred -> ExprInferred
getParam = getGuidParam . Guid.fromString

listOf :: ExprInferred -> ExprInferred
listOf = (getDef "List" $$)

-- Uses inferred holes for cons type
list :: [ExprInferred] -> ExprInferred
list [] = getDef "[]" $$ hole
list items@(x:_) =
  foldr cons nil items
  where
    cons h t = getDef ":" $$ typ $$: [h, t]
    nil = getDef "[]" $$ typ
    typ = iexpr (x ^. iType) pureSet (ExprLens.bodyHole # ())

maybeOf :: ExprInferred -> ExprInferred
maybeOf = (getDef "Maybe" $$)

getDef :: String -> ExprInferred
getDef name =
  simple
  (ExprLens.bodyDefinitionRef # Def name)
  (void (definitionTypes ! Def name))

tag :: Guid -> ExprInferred
tag guid =
  simple (ExprLens.bodyTag # guid) $
  ExprLens.pureExpr . ExprLens.bodyTagType # ()

tagStr :: String -> ExprInferred
tagStr = tag . Guid.fromString

set :: ExprInferred
set = simple bodySet pureSet

tagType :: ExprInferred
tagType = simple (ExprLens.bodyTagType # ()) pureSet

integerType :: ExprInferred
integerType = simple bodyIntegerType pureSet

infixl 4 $$
infixl 3 $$:

($$:) :: ExprInferred -> [ExprInferred] -> ExprInferred
($$:) f args =
  f $$ record KVal (zip tags args)
  where
    tags = recType ^.. Lens.traversed . Lens._1 . ExprLens.exprTag . Lens.to tag
    recType =
      fromMaybe (error msg) $
      f ^? iType . ExprLens.exprKindedLam KType . Lens._2 . ExprLens.exprKindedRecordFields KType
    msg = "$$: must be applied on a func of record type, not: " ++ show (f ^. iType)

($$) :: ExprInferred -> ExprInferred -> ExprInferred
($$) func@(Expr.Expression funcBody (funcVal, funcType)) nextArg =
  iexpr applyVal applyType application
  where
    application = ExprUtil.makeApply func nextArg
    handleLam e k seeHole seeOther =
      case e ^. Expr.eBody of
      Expr.BodyLeaf Expr.Hole -> seeHole
      Expr.BodyLam (Expr.Lam k1 paramGuid _ result)
        | k == k1 -> ExprUtil.substGetPar paramGuid (nextArg ^. iVal) result
      _ -> seeOther
    applyVal =
      handleLam funcVal KVal pureHole $
      if Lens.has ExprLens.bodyDefinitionRef funcBody
      then bodyToPureExpr application
      else pureHole
    applyType = handleLam funcType KType piErr piErr
    piErr = error "Apply of non-Pi type!"

record :: Kind -> [(ExprInferred, ExprInferred)] -> ExprInferred
record k fields =
  simple (ExprLens.bodyKindedRecordFields k # fields) typ
  where
    typ = case k of
      KVal ->
        ExprLens.pureExpr . ExprLens.bodyKindedRecordFields KType #
        map (void *** (^. iType)) fields
      KType -> pureSet

asHole :: ExprInferred -> ExprInferred
asHole expr =
  iexpr val typ $ ExprLens.bodyHole # ()
  where
    (val, typ) = expr ^. Expr.ePayload
