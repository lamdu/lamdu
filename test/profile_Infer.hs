import Control.Exception (evaluate)
import Control.Monad
import Data.Foldable (traverse_)
import Utils

count :: Int
count = 100

main :: IO ()
main = do
  putStrLn $ unwords ["Running factorial inference ", show count, "times"]
  traverse_ (void . evaluate . factorial) [1..count]
