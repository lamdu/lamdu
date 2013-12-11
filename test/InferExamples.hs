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

-- Solve depressed quartic polynomial
solveDepressedQuarticExpr :: InferResults t
solveDepressedQuarticExpr =
  lambdaRecord "params"
  [ ("e", iInt)
  , ("d", iInt)
  , ("c", iInt)
  ] $ \[e, d, c] ->
  whereItem "solvePoly" ( getDef "id" $$ iListInt )
  $ \solvePoly ->
  whereItem "sqrts"
  ( lambda "x" iInt $ \x ->
    whereItem "r"
    ( getDef "sqrt" $$ iInt $$ x
    ) $ \r ->
    list [r, getDef "negate" $$ iInt $$ r]
  )
  $ \sqrts ->
  getDef "if" $$ iListInt $$:
  [ getDef "==" $$ iInt $$: [d, literalInteger 0]
  , getDef "concat" $$ iInt $$
    ( getDef "map" $$ iInt $$ iListInt $$:
      [ solvePoly $$ list [e, c, literalInteger 1]
      , sqrts
      ]
    )
  , getDef "concat" $$ iInt $$
    ( getDef "map" $$ iInt $$ iListInt $$:
      [ sqrts $$ (getDef "head" $$ iInt $$ (solvePoly $$ list
        [ getDef "negate" $$ iInt $$ (d %* d)
        , (c %* c) %- (literalInteger 4 %* e)
        , literalInteger 2 %* c
        , literalInteger 1
        ]))
      , lambda "x" iInt $ \x ->
        solvePoly $$ list
        [ (c %+ (x %* x)) %- (d %/ x)
        , literalInteger 2 %* x
        , literalInteger 2
        ]
      ]
    )
  ]
  where
    iInt = asHole integerType
    iListInt = asHole $ listOf integerType
    x %+ y = getDef "+" $$ iInt $$: [x, y]
    x %- y = getDef "-" $$ iInt $$: [x, y]
    x %* y = getDef "*" $$ iInt $$: [x, y]
    x %/ y = getDef "/" $$ iInt $$: [x, y]

factorialEncode :: Int -> Int
factorialEncode = inferAndEncode factorialExpr

euler1Encode :: Int -> Int
euler1Encode = inferAndEncode euler1Expr

solveDepressedQuarticEncode :: Int -> Int
solveDepressedQuarticEncode = inferAndEncode solveDepressedQuarticExpr
