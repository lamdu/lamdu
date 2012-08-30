import Control.Exception (evaluate)
import Control.Monad
import Utils

count :: Int
count = 100

main :: IO ()
main = do
  putStrLn $ unwords ["Running factorial inference ", show count, "times"]
  mapM_ (void . evaluate . factorial) [1..count]
