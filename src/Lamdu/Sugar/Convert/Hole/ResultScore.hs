{-# LANGUAGE NoImplicitPrelude #-}
module Lamdu.Sugar.Convert.Hole.ResultScore
    ( resultScore
    ) where

import qualified Control.Lens as Lens
import qualified Data.Map as Map
import           Lamdu.Calc.Type (Type(..), Composite(..))
import qualified Lamdu.Calc.Val as V
import           Lamdu.Calc.Val.Annotated (Val(..))
import qualified Lamdu.Calc.Val.Annotated as Val
import qualified Lamdu.Expr.Lens as ExprLens
import qualified Lamdu.Infer as Infer
import           Lamdu.Sugar.Types.Hole (HoleResultScore(..))

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

score :: Val Infer.Payload -> [Int]
score (Val pl body) =
    (if Lens.has ExprLens.valBodyHole body then 1 else 0) :
    resultScopeScore :
    resultTypeScore (pl ^. Infer.plType) ++
    (body ^.. Lens.traversed >>= score)
    where
        resultScopeScore =
            case body ^? ExprLens.valBodyVar <&> (`Map.member` Infer.scopeToTypeMap (pl ^. Infer.plScope)) of
            Just False -> 1
            _ -> 0

resultScore :: Val Infer.Payload -> HoleResultScore
resultScore val =
    HoleResultScore
    { _hrsNumHoleWrappers = numWrappers val
    , _hrsScore = score val
    }

numWrappers :: Val a -> Int
numWrappers val =
    sum (val ^.. Val.body . traverse <&> numWrappers) +
    if Lens.has appliedHole val
    then 1
    else 0

appliedHole :: Lens.Traversal' (Val a) ()
appliedHole = ExprLens.valApply . V.applyFunc . ExprLens.valHole
