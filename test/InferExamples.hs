module InferExamples where

import InferWrappers
import InferCombinators

factorialExpr :: InferResults t
factorialExpr =
  lambda "x" iInt $ \x ->
  getDef "if" $$ iInt $$:
  [ getDef "==" $$ iInt $$:
    [x, literalInteger 0]
  , literalInteger 1
  , getDef "*" $$ iInt $$:
    [ x
    , recurse (integerType ~> integerType) $$
      (getDef "-" $$ iInt $$: [x, literalInteger 1])
    ]
  ]
  where
    iInt = asHole integerType

euler1Expr :: InferResults t
euler1Expr =
  getDef "sum" $$ iInt $$
  ( getDef "filter" $$ iInt $$:
    [ getDef ".." $$: [literalInteger 1, literalInteger 1000]
    , lambda "x" iInt $ \x ->
      getDef "||" $$:
      [ getDef "==" $$ iInt $$:
        [ literalInteger 0, getDef "%" $$ iInt $$: [x, literalInteger 3] ]
      , getDef "==" $$ iInt $$:
        [ literalInteger 0, getDef "%" $$ iInt $$: [x, literalInteger 5] ]
      ]
    ]
  )
  where
    iInt = asHole integerType

factorial :: Int -> Int
factorial = inferAndEncode factorialExpr

euler1 :: Int -> Int
euler1 = inferAndEncode euler1Expr

