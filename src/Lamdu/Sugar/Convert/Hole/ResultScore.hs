{-# LANGUAGE NoImplicitPrelude #-}
module Lamdu.Sugar.Convert.Hole.ResultScore
    ( resultScore
    ) where

import qualified Control.Lens as Lens
import qualified Data.Map as Map
import           Lamdu.Calc.Type (Type(..), Composite(..))
import           Lamdu.Calc.Val.Annotated (Val(..))
import qualified Lamdu.Calc.Val.Annotated as Val
import qualified Lamdu.Calc.Val as V
import qualified Lamdu.Expr.Lens as ExprLens
import qualified Lamdu.Infer as Infer

import           Lamdu.Prelude

resultTypeScore :: Type -> [Int]
resultTypeScore (TVar _) = [0]
resultTypeScore (TInst _ p) = 1 : maximum ([] : map resultTypeScore (Map.elems p))
resultTypeScore (TFun a r) = 2 : max (resultTypeScore a) (resultTypeScore r)
resultTypeScore (TSum c) = 2 : compositeTypeScore c
resultTypeScore (TRecord c) = 2 : compositeTypeScore c

compositeTypeScore :: Composite t -> [Int]
compositeTypeScore CEmpty = []
compositeTypeScore (CVar _) = [1]
compositeTypeScore (CExtend _ t r) =
    max (resultTypeScore t) (compositeTypeScore r)

resultScore :: Val Infer.Payload -> [Int]
resultScore val@(Val pl body) =
    numWrappers val :
    (if Lens.has ExprLens.valBodyHole body then 1 else 0) :
    resultScopeScore :
    resultTypeScore (pl ^. Infer.plType) ++
    (body ^.. Lens.traversed >>= resultScore)
    where
        resultScopeScore =
            case body ^? ExprLens.valBodyVar <&> (`Map.member` Infer.scopeToTypeMap (pl ^. Infer.plScope)) of
            Just False -> 1
            _ -> 0

numWrappers :: Val a -> Int
numWrappers val =
    sum (val ^.. Val.body . traverse <&> numWrappers) +
    if Lens.has (ExprLens.valApply . V.applyFunc . ExprLens.valHole) val
    then 1
    else 0
