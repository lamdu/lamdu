{-# OPTIONS -Wall -Werror #-}
module InferCombinators where

import Control.Arrow ((&&&))
import Control.Lens.Operators
import Control.Monad (void)
import Data.Map ((!))
import Data.Store.Guid (Guid)
import Lamdu.Data.Arbitrary () -- Arbitrary instance
import Lamdu.Data.Expression (Expression(..), Kind(..))
import Lamdu.Data.Expression.IRef (DefI)
import Lamdu.Data.Expression.Utils (pureHole, pureSet, pureIntegerType)
import Utils
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
iexpr iVal iType body =
  Expr.Expression body (iVal, iType)

five :: PureExprDefI t
five = pureLiteralInt # 5

bodyToPureExpr :: Expr.Body def (Expression def a) -> PureExpr def
bodyToPureExpr exprBody = ExprLens.pureExpr # fmap void exprBody

-- inferred-val is simply equal to the expr. Type is given
simple :: Expr.Body (DefI t) (InferResults t) -> PureExprDefI t -> InferResults t
simple body iType = iexpr (bodyToPureExpr body) iType body

-- New-style:

integer :: Integer -> InferResults t
integer x = simple (ExprLens.bodyLiteralInteger # x) pureIntegerType

piType :: String -> InferResults t -> InferResults t -> InferResults t
piType name src dest =
  simple (ExprUtil.makePi (Guid.fromString name) src dest) pureSet

holeWithInferredType :: PureExprDefI t -> InferResults t
holeWithInferredType = simple bodyHole

-- TODO: Get type from scope
getParam :: String -> PureExprDefI t -> InferResults t
getParam name = simple $ ExprLens.bodyParameterRef # Guid.fromString name

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

inferredSetType :: InferResults t
inferredSetType = simple bodySet pureSet

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
            | k == k1 -> ExprUtil.subst paramGuid (void nextArg) result
          _ -> seeOther
        applyVal = handleLam funcVal Val pureHole $ bodyToPureExpr application
        applyType = handleLam funcType Type piErr piErr
        piErr = error "Apply of non-Pi type!"

record :: Kind -> [(InferResults t, InferResults t)] -> InferResults t
record k fields =
  simple (recBody k) typ
  where
    typ = case k of
      Val -> bodyToPureExpr $ recBody Type
      Type -> pureSet
    recBody k1 = ExprLens.bodyKindedRecordFields k1 # fields

inferredVal :: InferResults t -> InferResults t
inferredVal expr =
  iexpr val typ $ ExprLens.bodyHole # ()
  where
    (val, typ) = expr ^. Expr.ePayload
