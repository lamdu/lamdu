module InferExamples where

import InferWrappers
import InferCombinators

factorial :: Int -> Int
factorial = inferAndEncode factorialExpr

euler1 :: Int -> Int
euler1 = inferAndEncode euler1Expr

