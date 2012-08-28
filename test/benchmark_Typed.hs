import Criterion.Main
import TypedTests (allTests)
import qualified Test.HUnit as HUnit

dontPutText :: HUnit.PutText ()
dontPutText = HUnit.PutText ((const . const) return) ()

main :: IO ()
main =
  defaultMain
  [ bgroup "test"
    [ bench "test" $ HUnit.runTestText dontPutText allTests
    ]
  ]
