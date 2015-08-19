module Lamdu.Sugar.Convert.Hole.ResultScore
    ( resultScore
    ) where

import qualified Control.Lens as Lens
import           Control.Lens.Operators
import qualified Data.Map as Map
import qualified Lamdu.Expr.Lens as ExprLens
import           Lamdu.Expr.Type (Type(..), Composite(..))
import           Lamdu.Expr.Val (Val(..))
import qualified Lamdu.Expr.Val as V
import qualified Lamdu.Infer as Infer

resultTypeScore :: Type -> [Int]
resultTypeScore (TVar _) = [0]
resultTypeScore TInt = [1]
resultTypeScore (TInst _ p) = 2 : maximum ([] : map resultTypeScore (Map.elems p))
resultTypeScore (TFun a r) = 2 : max (resultTypeScore a) (resultTypeScore r)
resultTypeScore (TSum c) = 2 : compositeTypeScore c
resultTypeScore (TRecord c) = 2 : compositeTypeScore c

compositeTypeScore :: Composite t -> [Int]
compositeTypeScore (CEmpty) = []
compositeTypeScore (CVar _) = [1]
compositeTypeScore (CExtend _ t r) =
    max (resultTypeScore t) (compositeTypeScore r)

resultScore :: Val Infer.Payload -> [Int]
resultScore val@(Val pl body) =
    bodyTopLevelScore body :
    resultTypeScore (pl ^. Infer.plType) ++
    [length (val ^.. ExprLens.payloadsOf ExprLens.valBodyHole)] ++
    (bodyParts body >>= resultScore)

bodyParts :: V.Body a -> [a]
bodyParts (V.BApp (V.Apply f a)) = [a, f]
bodyParts x = x ^.. Lens.traversed

bodyTopLevelScore :: V.Body (Val a) -> Int
bodyTopLevelScore body =
    case body of
    V.BApp (V.Apply (Val _ (V.BLeaf V.LHole)) _) -> 10
    V.BLeaf V.LHole -> 2
    V.BLeaf V.LGlobal{} -> 0
    _ -> 1
