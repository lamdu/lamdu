module InferPerf where

import Control.Applicative
import Control.Monad
import Data.Binary.Utils (encodeS)
import Data.Store.Guid (Guid)
import InferWrappers
import Utils
import qualified Data.ByteString as SBS
import qualified Lamdu.Data.Expr as Expr

inferAndEncode :: Expr.Expr Def Guid a -> Int -> Int
inferAndEncode expr par =
  SBS.length $ encodeS result
  where
    result = either (error . show) id . runLoadInferDerefDef . void $ par <$ expr
