import Control.Exception (evaluate)
import Control.Monad
import Data.Foldable (traverse_)
import InferExamples

run :: Int -> String -> (Int -> a) -> IO ()
run count name f = do
  putStrLn $ unwords ["Running", name, "inference", show count, "times"]
  traverse_ (void . evaluate . f) [1..count]

main :: IO ()
main = do
  run 100 "factorial" factorial
  run 100 "euler1" euler1
