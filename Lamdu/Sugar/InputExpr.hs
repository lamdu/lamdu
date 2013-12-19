module Lamdu.Sugar.InputExpr (makePure) where

import Lamdu.Data.Expr (Expr)
import Lamdu.Sugar.Types (InputPayloadP(..), InputPayload)
import System.Random (RandomGen)
import qualified Lamdu.Data.Expr.Utils as ExprUtil

makePure ::
  RandomGen gen => gen -> Expr def a -> Expr def (InputPayload m a)
makePure gen =
  ExprUtil.randomizeExprAndParams gen . fmap f
  where
    f a guid = InputPayload guid Nothing Nothing a
