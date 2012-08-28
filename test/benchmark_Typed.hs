import Criterion.Main
import Utils
import qualified Data.Store.Guid as Guid
import qualified Data.Store.IRef as IRef
import qualified Editor.Data as Data
import qualified Editor.Data.Typed as Typed
import qualified System.Random as Random

factorial :: Int -> (Typed.Expression (), Typed.RefMap)
factorial gen =
  uncurry doInferM Typed.initial (Just factorialDefI) Nothing .
  Data.randomizeGuids (Random.mkStdGen gen) .
  makeLambda "x" hole $
  makeApply
  [ getDefExpr "if"
  , hole
  , makeApply [getDefExpr "==", getParamExpr "x", literalInt 0]
  , literalInt 1
  , makeApply
    [ getDefExpr "*"
    , getParamExpr "x"
    , makeApply
      [ mkExpr "recurse" $ Data.makeDefinitionRef factorialDefI
      , makeApply [getDefExpr "-", getParamExpr "x", literalInt 1]
      ]
    ]
  ]
  where
    factorialDefI = IRef.unsafeFromGuid $ Guid.fromString "factorial"

main :: IO ()
main =
  defaultMain
  [ bgroup "test"
    [ bench "factorial" (whnf factorial 0)
    ]
  ]
