import Control.Exception (evaluate)
import Control.Monad
import Data.Foldable (traverse_)
import InferPerf
import InferTests

run :: Int -> String -> (Int -> a) -> IO ()
run count name f = do
  putStrLn $ unwords ["Running", name, "inference", show count, "times"]
  traverse_ (void . evaluate . f) [1..count]

main :: IO ()
main = do
  bench "factorial" factorialExpr
  bench "euler1" euler1Expr
  bench "solveDepressedQuarticExpr" solveDepressedQuarticExpr
  where
    bench name expr = run 100 name $ inferAndEncode expr
