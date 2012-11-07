import InferTests (allTests)
import qualified Test.Framework as TestFramework

main :: IO ()
main = TestFramework.defaultMain allTests
