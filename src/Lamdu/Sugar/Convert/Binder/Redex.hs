{-# LANGUAGE TemplateHaskell, TypeOperators, RankNTypes, GADTs #-}

module Lamdu.Sugar.Convert.Binder.Redex
    ( Redex(..), bodyScope, lam, lamPl, paramRefs, arg
    , hmapRedex
    , check
    ) where

import qualified Control.Lens as Lens
import           Hyper
import           Hyper.Recurse
import           Hyper.Type.AST.Lam
import qualified Lamdu.Calc.Term as V
import           Lamdu.Eval.Results (ScopeId)
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Types

import           Lamdu.Prelude

data Redex a = Redex
    { _bodyScope :: EvalScopes ScopeId
    , _lam :: V.Lam V.Var V.Term # Ann (GetHyperType a)
    , _lamPl :: a :# V.Term
    , _paramRefs :: [EntityId]
    , _arg :: Ann (GetHyperType a) # V.Term
    }
Lens.makeLenses ''Redex

hmapRedex ::
    (forall n. HRecWitness V.Term n -> p # n -> q # n) ->
    Redex # p -> Redex # q
hmapRedex f r =
    r
    { _lamPl = f HRecSelf (r ^. lamPl)
    , _lam =
        hmap
        ( \case
            HWitness W_Lam_expr -> hflipped %~ hmap (\(HWitness w) -> f w)
        ) (r ^. lam)
    , _arg = r ^. arg & hflipped %~ hmap (\(HWitness w) -> f w)
    }

check ::
    V.Term # Ann (Input.Payload m a) ->
    Maybe (Redex # Input.Payload m a)
check term =
    do
        V.App func a <- term ^? V._BApp
        l <- func ^? hVal . V._BLam
        Just Redex
            { _lam = l
            , _lamPl = func ^. hAnn
            , _bodyScope =
                func ^. hAnn . Input.evalResults
                <&> (^. Input.eAppliesOfLam)
                <&> Lens.traversed %~ getRedexApplies
            , _arg = a
            , _paramRefs = func ^. hAnn . Input.varRefsOfLambda
            }
    where
        getRedexApplies [(scopeId, _)] = scopeId
        getRedexApplies _ =
            error "redex should only be applied once per parent scope"
