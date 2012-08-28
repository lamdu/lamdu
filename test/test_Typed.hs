import Test.Framework.Providers.HUnit (hUnitTestToTests)
import TypedTests (allTests)
import qualified Test.Framework as TestFramework

main :: IO ()
main = TestFramework.defaultMain $ hUnitTestToTests allTests

