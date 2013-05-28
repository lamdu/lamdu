import Control.Monad
import Criterion.Main
import InferExamples

main :: IO ()
main = do
  putStrLn "============================================="
  putStrLn "Binary encode size:"
  forM_ [("factorial", factorial 0), ("euler1", euler1 0)] $ \(name, size) ->
    putStrLn $ unwords ["==", name, "inferred:", show size, "bytes"]
  putStrLn "============================================="

  defaultMain
    [ bench "factorial" $ whnf factorial 0
    , bench "euler1" $ whnf euler1 0
    ]
