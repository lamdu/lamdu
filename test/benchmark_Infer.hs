import Control.Monad
import Criterion.Main
import InferExamples

benches :: [(String, Int -> Int)]
benches =
  [ ("factorial", factorialEncode)
  , ("euler1", euler1Encode)
  , ("solveDepressedQuartic", solveDepressedQuarticEncode)
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
