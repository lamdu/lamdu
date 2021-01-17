{-# LANGUAGE GADTs, TypeApplications #-}
module Lamdu.Sugar.Convert.Hole.ResultScore
    ( resultScore
    ) where

import qualified Control.Lens as Lens
import qualified Data.Map as Map
import           Hyper
import           Hyper.Infer (InferResult, inferResult)
import           Hyper.Type.AST.FuncType (FuncType(..))
import           Hyper.Type.AST.Nominal (NominalInst(..))
import           Hyper.Type.AST.Row (RowExtend(..))
import           Hyper.Type.AST.Scheme (QVarInstances(..))
import qualified Lamdu.Calc.Lens as ExprLens
import qualified Lamdu.Calc.Term as V
import           Lamdu.Calc.Type (Type(..), Row(..), Types(..))
import           Lamdu.Sugar.Types.Parts (HoleResultScore(..))

import           Lamdu.Prelude

resultTypeScore :: Pure # Type -> [Int]
resultTypeScore x =
    case x ^. _Pure of
    TVar{} -> [0]
    TFun (FuncType a r) -> 2 : max (resultTypeScore a) (resultTypeScore r)
    TVariant c -> 2 : compositeTypeScore c
    TRecord c -> 2 : compositeTypeScore c
    TInst (NominalInst _ (Types (QVarInstances t) (QVarInstances r))) ->
        1 : maximum ([] : map resultTypeScore (Map.elems t) <> map compositeTypeScore (Map.elems r))

compositeTypeScore :: Pure # Row -> [Int]
compositeTypeScore x =
    case x ^. _Pure of
    REmpty -> []
    RVar{} -> [1]
    RExtend (RowExtend _ t r) ->
        max (resultTypeScore t) (compositeTypeScore r)

score :: Ann (InferResult Pure) # V.Term -> [Int]
score x =
    (if Lens.has (hVal . ExprLens.valBodyHole) x then 1 else 0) :
    resultTypeScore (x ^. hAnn . inferResult) ++
    hfoldMap
    ( \case
        HWitness V.W_Term_Term -> score
        HWitness V.W_Term_HCompose_Prune_Type -> const []
    ) (x ^. hVal)

resultScore :: Ann (InferResult Pure) # V.Term -> HoleResultScore
resultScore x =
    HoleResultScore
    { _hrsNumFragments = numFragments x
    , _hrsScore = score x
    }

numFragments :: Ann a # V.Term -> Int
numFragments x =
    inner +
    if Lens.has appliedHole x
    then 1
    else 0
    where
        inner =
            hfoldMap @_ @[Int]
            ( \case
                HWitness V.W_Term_Term -> (:[]) . numFragments
                HWitness V.W_Term_HCompose_Prune_Type -> const []
            ) (x ^. hVal)
            & sum

appliedHole :: Lens.Traversal' (Ann a # V.Term) ()
appliedHole = hVal . V._BApp . V.appFunc . hVal . V._BLeaf . V._LHole
