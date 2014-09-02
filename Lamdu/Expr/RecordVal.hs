module Lamdu.Expr.RecordVal
  ( unpack
  ) where

import Control.Lens.Operators
import Control.Lens.Tuple
import Data.Map (Map)
import Lamdu.Expr.Val (Val(..))
import qualified Data.Map as Map
import qualified Lamdu.Expr.Type as T
import qualified Lamdu.Expr.Val as V

unpack :: Val a -> (Map T.Tag (a, Val a), Val a)
unpack (Val pl (V.BRecExtend (V.RecExtend tag val rest))) =
  unpack rest
  & _1 %~ Map.insert tag (pl, val)
unpack rest = (Map.empty, rest)
