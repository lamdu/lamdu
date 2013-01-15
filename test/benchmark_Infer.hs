import Criterion.Main
import Utils

main :: IO ()
main =
  defaultMain
  [ bgroup "test"
    [ bench "factorial" (nf factorial 0)
    ]
  ]
