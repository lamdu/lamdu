module Lamdu.Sugar.InputExpr (makePure) where

import Lamdu.Data.Expr (Expr)
import Lamdu.Sugar.Types (InputPayloadP(..), InputPayload)
import System.Random (Random, RandomGen)
import qualified Lamdu.Data.Expr.Utils as ExprUtil

makePure ::
  (Ord par, Random par, RandomGen gen) => gen -> Expr def par a -> Expr def par (InputPayload m a)
makePure gen =
  ExprUtil.randomizeExprAndParams gen . fmap f
  where
    f a par = InputPayload par Nothing Nothing a
