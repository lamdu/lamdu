module Lamdu.Sugar.InputExpr (makePure) where

import Lamdu.Data.Expr (Expression)
import Lamdu.Sugar.Types (InputPayloadP(..), InputPayload)
import System.Random (RandomGen)
import qualified Lamdu.Data.Expr.Utils as ExprUtil

makePure ::
  RandomGen gen => gen -> Expression def a -> Expression def (InputPayload m a)
makePure gen =
  ExprUtil.randomizeExprAndParams gen . fmap f
  where
    f a guid = InputPayload guid Nothing Nothing a
