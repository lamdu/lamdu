import Control.Applicative
import Control.Monad
import Criterion.Main
import InferPerf
import InferTests

main :: IO ()
main = do
  putStrLn "============================================="
  putStrLn "Binary encode size:"
  forM_ [ ("factorial", inferAndEncode factorialExpr 0)
        , ("euler1", inferAndEncode euler1Expr 0)
        ] $ \(name, size) ->
    putStrLn $ unwords ["==", name, "inferred:", show size, "bytes"]
  putStrLn "============================================="

  defaultMain
    [ bench "factorial" $ whnf (inferAndEncode factorialExpr) 0
    , bench "euler1" $ whnf (inferAndEncode euler1Expr) 0
    ]
