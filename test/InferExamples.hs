module InferExamples where

import Control.Lens.Operators
import Lamdu.Data.Expression.Utils (pureHole)
import Utils
import InferWrappers

factorialExpr :: PureExprDefI t
factorialExpr =
  pureLambda "x" pureHole $
  pureApplyPoly1 "if"
  [ pureApplyPoly1 "==" [pureGetParam "x", pureLiteralInt # 0]
  , pureLiteralInt # 1
  , pureApplyPoly1 "*"
    [ pureGetParam "x"
    , pureApply
      [ pureGetRecursiveDefI
      , pureApplyPoly1 "-" [pureGetParam "x", pureLiteralInt # 1]
      ]
    ]
  ]

euler1Expr :: PureExprDefI t
euler1Expr =
  pureApplyPoly1 "sum"
  [ pureApplyPoly1 "filter"
    [ pureLambda "x" pureHole $
      pureApply
      [ pureGetDef "||"
      , pureApplyPoly1 "=="
        [ pureLiteralInt # 0, pureApplyPoly1 "%" [pureGetParam "x", pureLiteralInt # 3] ]
      , pureApplyPoly1 "=="
        [ pureLiteralInt # 0, pureApplyPoly1 "%" [pureGetParam "x", pureLiteralInt # 5] ]
      ]
    , pureApply [pureGetDef "..", pureLiteralInt # 1, pureLiteralInt # 1000]
    ]
  ]

factorial :: Int -> Int
factorial = inferAndEncode factorialExpr

euler1 :: Int -> Int
euler1 = inferAndEncode euler1Expr

