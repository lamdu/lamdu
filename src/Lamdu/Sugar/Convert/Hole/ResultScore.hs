module Lamdu.Sugar.Convert.Hole.ResultScore
    ( resultScore
    ) where

import qualified Control.Lens as Lens
import qualified Data.Map as Map
import           Hyper (Tree, Pure, _Pure, traverseK1)
import           Hyper.Type.AST.FuncType (FuncType(..))
import           Hyper.Type.AST.Nominal (NominalInst(..))
import           Hyper.Type.AST.Row (RowExtend(..))
import           Hyper.Type.AST.Scheme (QVarInstances(..))
import           Hyper.Type.Ann (val, ann)
import qualified Lamdu.Calc.Lens as ExprLens
import           Lamdu.Calc.Term (Val)
import qualified Lamdu.Calc.Term as V
import           Lamdu.Calc.Type (Type(..), Row(..), Types(..))
import           Lamdu.Sugar.Types.Parts (HoleResultScore(..))

import           Lamdu.Prelude

resultTypeScore :: Tree Pure Type -> [Int]
resultTypeScore x =
    case x ^. _Pure of
    TVar{} -> [0]
    TFun (FuncType a r) -> 2 : max (resultTypeScore a) (resultTypeScore r)
    TVariant c -> 2 : compositeTypeScore c
    TRecord c -> 2 : compositeTypeScore c
    TInst (NominalInst _ (Types (QVarInstances t) (QVarInstances r))) ->
        1 : maximum ([] : map resultTypeScore (Map.elems t) <> map compositeTypeScore (Map.elems r))

compositeTypeScore :: Tree Pure Row -> [Int]
compositeTypeScore x =
    case x ^. _Pure of
    REmpty -> []
    RVar{} -> [1]
    RExtend (RowExtend _ t r) ->
        max (resultTypeScore t) (compositeTypeScore r)

score :: Val (Tree Pure Type) -> [Int]
score x =
    (if Lens.has ExprLens.valBodyHole (x ^. val) then 1 else 0) :
    resultTypeScore (x ^. ann) ++
    (x ^.. val . traverseK1 >>= score)

resultScore :: Val (Tree Pure Type) -> HoleResultScore
resultScore x =
    HoleResultScore
    { _hrsNumFragments = numFragments x
    , _hrsScore = score x
    }

numFragments :: Val a -> Int
numFragments x =
    sum (x ^.. val . traverseK1 <&> numFragments) +
    if Lens.has appliedHole x
    then 1
    else 0

appliedHole :: Lens.Traversal' (Val a) ()
appliedHole = val . V._BApp . V.appFunc . val . V._BLeaf . V._LHole
