{-# LANGUAGE NoImplicitPrelude #-}
module Lamdu.Expr.RecordVal
    ( unpack
    ) where

import qualified Data.Map as Map
import qualified Lamdu.Calc.Type as T
import           Lamdu.Calc.Val.Annotated (Val(..))
import qualified Lamdu.Calc.Val as V

import           Lamdu.Prelude

unpack :: Val a -> (Map T.Tag (a, Val a), Val a)
unpack (Val pl (V.BRecExtend (V.RecExtend tag val rest))) =
    unpack rest
    & _1 %~ Map.insert tag (pl, val)
unpack rest = (Map.empty, rest)
