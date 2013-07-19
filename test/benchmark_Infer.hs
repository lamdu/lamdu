import Control.Applicative
import Control.Monad
import Criterion.Main
import Data.Binary.Utils (encodeS)
import InferTests
import InferWrappers
import Utils
import qualified Data.ByteString as SBS
import qualified Lamdu.Data.Expression as Expr

inferAndEncode :: Expr.Expression Def a -> Int -> Int
inferAndEncode expr par =
  SBS.length $ encodeS result
  where
    result = assertSuccess . loadInferDef . void $ par <$ expr

main :: IO ()
main = do
  putStrLn "============================================="
  putStrLn "Binary encode size:"
  forM_ [ ("factorial", inferAndEncode factorialExpr 0)
        , ("euler1", inferAndEncode euler1Expr 0)
        ] $ \(name, size) ->
    putStrLn $ unwords ["==", name, "inferred:", show size, "bytes"]
  putStrLn "============================================="

  defaultMain
    [ bench "factorial" $ whnf (inferAndEncode factorialExpr) 0
    , bench "euler1" $ whnf (inferAndEncode euler1Expr) 0
    ]
