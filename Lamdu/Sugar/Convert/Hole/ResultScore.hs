module Lamdu.Sugar.Convert.Hole.ResultScore
    ( resultScore
    ) where

import qualified Control.Lens as Lens
import           Control.Lens.Operators
import qualified Data.Map as Map
import           Lamdu.Expr.Type (Type(..), Composite(..))
import           Lamdu.Expr.Val (Val(..))
import qualified Lamdu.Expr.Val as V
import qualified Lamdu.Infer as Infer

resultTypeScore :: Type -> [Int]
resultTypeScore (TVar _) = [0]
resultTypeScore TInt = [1]
resultTypeScore (TInst _ p) = 2 : maximum ([] : map resultTypeScore (Map.elems p))
resultTypeScore (TFun a r) = 2 : max (resultTypeScore a) (resultTypeScore r)
resultTypeScore (TSum c) =
    compositeTypeScore c
resultTypeScore (TRecord c) =
    compositeTypeScore c

compositeTypeScore :: Composite t -> [Int]
compositeTypeScore (CEmpty) = [2]
compositeTypeScore (CVar _) = [1]
compositeTypeScore (CExtend _ t r) = 2 : max (resultTypeScore t) (compositeTypeScore r)

resultScore :: Val Infer.Payload -> [Int]
resultScore (Val pl body) =
    bodyTopLevelScore : resultTypeScore (pl ^. Infer.plType) ++
    (body ^.. Lens.traversed <&> resultScore & ([]:) & maximum)
    where
        bodyTopLevelScore =
            case body of
            V.BApp (V.Apply (Val _ (V.BLeaf V.LHole)) _) -> 10
            V.BLeaf V.LHole -> 1
            _ -> 0
