module InferPerf where

import Control.Applicative
import Control.Monad
import Data.Binary.Utils (encodeS)
import Utils
import InferWrappers
import qualified Data.ByteString as SBS
import qualified Lamdu.Data.Expression as Expr

inferAndEncode :: Expr.Expression Def a -> Int -> Int
inferAndEncode expr par =
  SBS.length $ encodeS result
  where
    result = assertSuccess . runLoadInferDerefDef . void $ par <$ expr
