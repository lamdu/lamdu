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
import Lamdu.Data.Expression.IRef (DefI)
import Lamdu.Data.Expression.Utils (pureHole, pureSet, pureIntegerType)
import Utils
import qualified Control.Lens as Lens
import qualified Data.Store.Guid as Guid
import qualified Data.Store.IRef as IRef
import qualified Lamdu.Data.Expression as Expr
import qualified Lamdu.Data.Expression.Lens as ExprLens
import qualified Lamdu.Data.Expression.Utils as ExprUtil

iexpr ::
  PureExprDefI t ->
  PureExprDefI t ->
  Expr.Body (DefI t) (InferResults t) -> InferResults t
iexpr val typ body =
  Expr.Expression body (val, typ)

five :: PureExprDefI t
five = pureLiteralInt # 5

iVal :: Lens' (InferResults t) (PureExprDefI t)
iVal = Expr.ePayload . Lens._1

iType :: Lens' (InferResults t) (PureExprDefI t)
iType = Expr.ePayload . Lens._2

bodyToPureExpr :: Expr.Body (DefI t) (InferResults t) -> PureExprDefI t
bodyToPureExpr exprBody = ExprLens.pureExpr # fmap (^. iVal) exprBody

-- inferred-val is simply equal to the expr. Type is given
simple :: Expr.Body (DefI t) (InferResults t) -> PureExprDefI t -> InferResults t
simple body typ = iexpr (bodyToPureExpr body) typ body

getParamPure :: String -> PureExprDefI t -> InferResults t
getParamPure name = simple $ ExprLens.bodyParameterRef # Guid.fromString name

getRecursiveDef :: PureExprDefI t
getRecursiveDef =
  ExprLens.pureExpr . ExprLens.bodyDefinitionRef # recursiveDefI

-- New-style:
recurse :: InferResults t -> InferResults t
recurse typ = simple (ExprLens.bodyDefinitionRef # recursiveDefI) $ typ ^. iVal

literalInteger :: Integer -> InferResults t
literalInteger x = simple (ExprLens.bodyLiteralInteger # x) pureIntegerType

piType ::
  String -> InferResults t ->
  (InferResults t -> InferResults t) -> InferResults t
piType name paramType mkResultType =
  simple (ExprUtil.makePi (Guid.fromString name) paramType result) pureSet
  where
    result = mkResultType $ getParam name paramType

infixr 4 ~>
(~>) :: InferResults t -> InferResults t -> InferResults t
(~>) src dest =
  simple (ExprUtil.makePi (Guid.fromString "") src dest) pureSet

lambda ::
  String -> InferResults t ->
  (InferResults t -> InferResults t) ->
  InferResults t
lambda name paramType mkResult =
  simple (ExprUtil.makeLambda guid paramType result) $
  ExprUtil.pureLam KType guid (paramType ^. iVal) (result ^. iType)
  where
    guid = Guid.fromString name
    result = mkResult $ getParam name paramType

-- Sometimes we have an inferred type that comes outside-in but cannot
-- be inferred inside-out:
setInferredType :: InferResults t -> InferResults t -> InferResults t
setInferredType typ val = val & iType .~ typ ^. iVal

getField :: InferResults t -> InferResults t -> InferResults t
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
  String -> [(String, InferResults t)] ->
  ([InferResults t] -> InferResults t) -> InferResults t
lambdaRecord paramsName strFields mkResult =
  lambda paramsName (record KType fields) $ \params ->
  mkResult $ map (getField params) fieldTags
  where
    fields = strFields & Lens.traversed . Lens._1 %~ tagStr
    fieldTags = map fst fields

whereItem ::
  String -> InferResults t -> (InferResults t -> InferResults t) -> InferResults t
whereItem name val mkBody =
  lambda name (iexpr (val ^. iType) pureSet bodyHole) mkBody $$ val

holeWithInferredType :: InferResults t -> InferResults t
holeWithInferredType = simple bodyHole . (^. iVal)

hole :: InferResults t
hole = simple bodyHole pureHole

getGuidParam :: Guid -> InferResults t -> InferResults t
getGuidParam guid typ =
  simple (ExprLens.bodyParameterRef # guid) $
  typ ^. iVal

getParam :: String -> InferResults t -> InferResults t
getParam = getGuidParam . Guid.fromString

listOf :: InferResults t -> InferResults t
listOf = (getDef "List" $$)

getDef :: String -> InferResults t
getDef name =
  simple
  (ExprLens.bodyDefinitionRef # IRef.unsafeFromGuid g)
  (void (definitionTypes ! g))
  where
    g = Guid.fromString name

tag :: Guid -> InferResults t
tag guid =
  simple (ExprLens.bodyTag # guid) $
  ExprLens.pureExpr . ExprLens.bodyTagType # ()

tagStr :: String -> InferResults t
tagStr = tag . Guid.fromString

set :: InferResults t
set = simple bodySet pureSet

tagType :: InferResults t
tagType = simple (ExprLens.bodyTagType # ()) pureSet

integerType :: InferResults t
integerType = simple bodyIntegerType pureSet

infixl 4 $$
infixl 3 $$:

($$:) :: InferResults t -> [InferResults t] -> InferResults t
($$:) f args =
  f $$ record KVal (zip tags args)
  where
    tags = recType ^.. Lens.traversed . Lens._1 . ExprLens.exprTag . Lens.to tag
    recType =
      fromMaybe (error "$$: must be applied on a func of record type") $
      f ^? iType . ExprLens.exprKindedLam KType . Lens._2 . ExprLens.exprKindedRecordFields KType

($$) :: InferResults t -> InferResults t -> InferResults t
($$) func@(Expr.Expression _ (funcVal, funcType)) nextArg =
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
      if ExprUtil.isTypeConstructorType applyType
      then bodyToPureExpr application
      else pureHole
    applyType = handleLam funcType KType piErr piErr
    piErr = error "Apply of non-Pi type!"

record :: Kind -> [(InferResults t, InferResults t)] -> InferResults t
record k fields =
  simple (ExprLens.bodyKindedRecordFields k # fields) typ
  where
    typ = case k of
      KVal ->
        ExprLens.pureExpr . ExprLens.bodyKindedRecordFields KType #
        map (void *** (^. iType)) fields
      KType -> pureSet

asHole :: InferResults t -> InferResults t
asHole expr =
  iexpr val typ $ ExprLens.bodyHole # ()
  where
    (val, typ) = expr ^. Expr.ePayload
