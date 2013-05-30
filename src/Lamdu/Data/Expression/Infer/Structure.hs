module Lamdu.Data.Expression.Infer.Structure
	(add) where

import Control.Applicative ((<$))
import Control.Lens.Operators
import Lamdu.Data.Expression (Expression(..))
import qualified Control.Lens as Lens
import qualified Lamdu.Data.Expression as Expr
import qualified Lamdu.Data.Expression.IRef as ExprIRef
import qualified Lamdu.Data.Expression.Infer as Infer
import qualified Lamdu.Data.Expression.Lens as ExprLens

add ::
  ExprIRef.ExpressionM m (Infer.Inferred def, a) ->
  ExprIRef.ExpressionM m (Infer.Inferred def, a)
add = Lens.traversed . Lens._1 %~ addToNode

addToNode :: Infer.Inferred def -> Infer.Inferred def
addToNode node
  | Lens.has ExprLens.exprHole (Infer.iValue node) =
    node
    { Infer.iValue =
        (Infer.iValue node ^. Expr.ePayload) <$
        structureForType (() <$ Infer.iType node)
    }
  | otherwise = node

structureForType ::
  Expression def () ->
  Expression def ()
structureForType expr =
  case expr ^. Expr.eBody of
  Expr.BodyRecord (Expr.Record Expr.Type fields) ->
    ExprLens.pureExpr . ExprLens.bodyKindedRecordFields Expr.Val #
    (fields & Lens.traversed . Lens._2 %~ structureForType)
  _ -> ExprLens.pureExpr . ExprLens.bodyHole # ()
