import Criterion.Main
import Utils

main :: IO ()
main =
  defaultMain
  [ bench "factorial" (factorial 0)
  , bench "euler1" (euler1 0)
  ]
