module Lamdu.Sugar.InputExpr (makePure) where

import Lamdu.Sugar.Types (InputExpr, InputPayloadP(..))
import System.Random (RandomGen)
import qualified Lamdu.Data.Expression.IRef as ExprIRef
import qualified Lamdu.Data.Expression.Utils as ExprUtil

-- Not inferred, not stored
makePure ::
  RandomGen g => g -> ExprIRef.ExpressionM m a -> InputExpr m a
makePure gen =
  ExprUtil.randomizeExprAndParams gen . fmap f
  where
    f a guid = InputPayload guid Nothing Nothing a
