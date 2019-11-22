{-# LANGUAGE TemplateHaskell, TypeOperators, GADTs, TypeFamilies #-}

module Lamdu.Sugar.Convert.Binder.Redex
    ( Redex(..) , bodyScope, lam, lamPl, paramRefs, arg
    , check
    ) where

import qualified Control.Lens as Lens
import           Hyper
import           Hyper.TH.Nodes (makeHNodes)
import qualified Lamdu.Calc.Term as V
import           Lamdu.Eval.Results (ScopeId)
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Types

import           Lamdu.Prelude

data Redex a = Redex
    { _bodyScope :: EvalScopes ScopeId
    , _lam :: Tree (V.Lam V.Var V.Term) (Ann (GetHyperType a))
    , _lamPl :: a # V.Term
    , _paramRefs :: [EntityId]
    , _arg :: Tree (Ann (GetHyperType a)) V.Term
    }
Lens.makeLenses ''Redex

makeHNodes ''Redex

instance HFunctor Redex where
    hmap f r =
        r
        { _lamPl = f (HWitness W_Redex_Term) (r ^. lamPl)
        , _lam = r ^. lam & hmapped1 . hflipped . hmapped1 %~ f (HWitness W_Redex_Term)
        , _arg = r ^. arg & hflipped . hmapped1 %~ f (HWitness W_Redex_Term)
        }

check ::
    Tree V.Term (Ann (Const (Input.Payload m a))) ->
    Maybe (Tree Redex (Const (Input.Payload m a)))
check term =
    do
        V.App func a <- term ^? V._BApp
        l <- func ^? hVal . V._BLam
        Just Redex
            { _lam = l
            , _lamPl = func ^. hAnn
            , _bodyScope =
                func ^. annotation . Input.evalResults
                <&> (^. Input.eAppliesOfLam)
                <&> Lens.traversed %~ getRedexApplies
            , _arg = a
            , _paramRefs = func ^. annotation . Input.varRefsOfLambda
            }
    where
        getRedexApplies [(scopeId, _)] = scopeId
        getRedexApplies _ =
            error "redex should only be applied once per parent scope"
