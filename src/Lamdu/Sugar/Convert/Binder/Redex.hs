{-# LANGUAGE TemplateHaskell, KindSignatures, DataKinds #-}

module Lamdu.Sugar.Convert.Binder.Redex
    ( Redex(..) , bodyScope, lam, lamPl, paramRefs, arg
    , check
    ) where

import qualified Control.Lens as Lens
import           Hyper
import           Lamdu.Calc.Term (Val)
import qualified Lamdu.Calc.Term as V
import           Lamdu.Eval.Results (ScopeId)
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Types

import           Lamdu.Prelude

data Redex a = Redex
    { _bodyScope :: EvalScopes ScopeId
    , _lam :: Tree (V.Lam V.Var V.Term) (Ann (Const a))
    , _lamPl :: a
    , _paramRefs :: [EntityId]
    , _arg :: Val a
    }
Lens.makeLenses ''Redex

instance Functor Redex where
    fmap f r =
        r
        { _lam = r ^. lam & V.lamOut . Lens.from _HFlip . hmapped1 . Lens._Wrapped %~ f
        , _lamPl = r ^. lamPl & f
        , _arg = r ^. arg & Lens.from _HFlip . hmapped1 . Lens._Wrapped %~ f
        }

check :: Tree V.Term (Ann (Const (Input.Payload m a))) -> Maybe (Redex (Input.Payload m a))
check term =
    do
        V.App func a <- term ^? V._BApp
        l <- func ^? hVal . V._BLam
        Just Redex
            { _lam = l
            , _lamPl = func ^. hAnn . Lens._Wrapped
            , _bodyScope =
                func ^. hAnn . Lens._Wrapped . Input.evalResults
                <&> (^. Input.eAppliesOfLam)
                <&> Lens.traversed %~ getRedexApplies
            , _arg = a
            , _paramRefs = func ^. hAnn . Lens._Wrapped . Input.varRefsOfLambda
            }
    where
        getRedexApplies [(scopeId, _)] = scopeId
        getRedexApplies _ =
            error "redex should only be applied once per parent scope"
