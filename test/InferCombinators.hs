{-# OPTIONS -Wall -Werror #-}
module InferCombinators where

import Control.Arrow ((&&&), (***))
import Control.Lens (Lens')
import Control.Lens.Operators
import Control.Monad (void)
import Data.Map ((!))
import Data.Maybe (fromMaybe)
import Data.Store.Guid (Guid)
import Lamdu.Data.Arbitrary () -- Arbitrary instance
import Lamdu.Data.Expression (Kind(..))
import Lamdu.Data.Expression.IRef (DefI)
import Lamdu.Data.Expression.Utils (pureHole, pureSet, pureIntegerType)
import Utils
import qualified Control.Lens as Lens
import qualified Data.Store.Guid as Guid
import qualified Data.Store.IRef as IRef
import qualified Lamdu.Data.Expression as Expr
import qualified Lamdu.Data.Expression.IRef as ExprIRef
import qualified Lamdu.Data.Expression.Infer as Infer
import qualified Lamdu.Data.Expression.Lens as ExprLens
import qualified Lamdu.Data.Expression.Utils as ExprUtil

type InferResults t =
  ExprIRef.Expression t
  ( PureExprDefI t
  , PureExprDefI t
  )

inferResults :: ExprIRef.Expression t (Infer.Inferred (DefI t)) -> InferResults t
inferResults = fmap (void . Infer.iValue &&& void . Infer.iType)

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

-- New-style:

integer :: Integer -> InferResults t
integer x = simple (ExprLens.bodyLiteralInteger # x) pureIntegerType

piType :: String -> InferResults t -> InferResults t -> InferResults t
piType name src dest =
  simple (ExprUtil.makePi (Guid.fromString name) src dest) pureSet

lambda :: String -> InferResults t -> InferResults t -> InferResults t
lambda name paramType result =
  simple (ExprUtil.makeLambda (Guid.fromString name) paramType result) $
  purePi name (paramType ^. iVal) (result ^. iType)

holeWithInferredType :: InferResults t -> InferResults t
holeWithInferredType = simple bodyHole . (^. iVal)

hole :: InferResults t
hole = simple bodyHole pureHole

-- TODO: Get type from scope?
getParam :: String -> InferResults t -> InferResults t
getParam name typ =
  simple (ExprLens.bodyParameterRef # Guid.fromString name) $
  typ ^. iVal

listOf :: InferResults t -> InferResults t
listOf x = apply [getDef "List", x]

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

setType :: InferResults t
setType = simple bodySet pureSet

integerType :: InferResults t
integerType = simple bodyIntegerType pureSet

-- Convenience wrapper for common case of applying dependent params
-- first:
applyRecord :: [InferResults t] -> [InferResults t] -> InferResults t
applyRecord = applyRec . apply

applyRec :: InferResults t -> [InferResults t] -> InferResults t
applyRec f args =
  apply [f, record Val (zip tags args)]
  where
    tags = recType ^.. Lens.traversed . Lens._1 . ExprLens.exprTag . Lens.to tag
    recType =
      fromMaybe (error "applyRec must be applied on a func of record type") $
      f ^? iType . ExprLens.exprKindedLam Type . Lens._2 . ExprLens.exprKindedRecordFields Type

apply :: [InferResults t] -> InferResults t
apply = foldl1 step
  where
    step func@(Expr.Expression _ (funcVal, funcType)) nextArg =
      iexpr applyVal applyType application
      where
        application = ExprUtil.makeApply func nextArg
        handleLam e k seeHole seeOther =
          case e ^. Expr.eBody of
          Expr.BodyLeaf Expr.Hole -> seeHole
          Expr.BodyLam (Expr.Lambda k1 paramGuid _ result)
            | k == k1 -> ExprUtil.subst paramGuid (nextArg ^. iVal) result
          _ -> seeOther
        applyVal = handleLam funcVal Val pureHole $ bodyToPureExpr application
        applyType = handleLam funcType Type piErr piErr
        piErr = error "Apply of non-Pi type!"

record :: Kind -> [(InferResults t, InferResults t)] -> InferResults t
record k fields =
  simple (ExprLens.bodyKindedRecordFields k # fields) typ
  where
    typ = case k of
      Val ->
        ExprLens.pureExpr . ExprLens.bodyKindedRecordFields Type #
        map (void *** (^. iType)) fields
      Type -> pureSet

inferredVal :: InferResults t -> InferResults t
inferredVal expr =
  iexpr val typ $ ExprLens.bodyHole # ()
  where
    (val, typ) = expr ^. Expr.ePayload
