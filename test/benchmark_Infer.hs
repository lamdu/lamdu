import Control.Monad
import Criterion.Main
import InferPerf
import InferTests

benches :: [(String, Int -> Int)]
benches =
  [ ("factorial", inferAndEncode factorialExpr)
  , ("euler1", inferAndEncode euler1Expr)
  , ("solveDepressedQuartic", inferAndEncode solveDepressedQuarticExpr)
  ]

main :: IO ()
main = do
  putStrLn "============================================="
  putStrLn "Binary encode size:"
  forM_ benches $ \(name, encodeSize) ->
    putStrLn $ unwords ["==", name, "inferred:", show (encodeSize 0), "bytes"]
  putStrLn "============================================="

  defaultMain $ map makeBench benches
  where
    makeBench (name, encodeSize) =
      bench name $ whnf encodeSize 0
