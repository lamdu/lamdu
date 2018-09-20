{-# LANGUAGE TemplateHaskell #-}

module Lamdu.Sugar.Convert.Binder.Redex
    ( Redex(..) , bodyScope, lam, lamPl, paramRefs, arg
    , check
    ) where

import qualified Control.Lens as Lens
import           Data.Tree.Diverse (annotations, ann, val)
import qualified Lamdu.Calc.Lens as ExprLens
import           Lamdu.Calc.Term (Val)
import qualified Lamdu.Calc.Term as V
import           Lamdu.Eval.Results (ScopeId)
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Types

import           Lamdu.Prelude

data Redex a = Redex
    { _bodyScope :: EvalScopes ScopeId
    , _lam :: V.Lam (Val a)
    , _lamPl :: a
    , _paramRefs :: [EntityId]
    , _arg :: Val a
    }
Lens.makeLenses ''Redex

instance Functor Redex where
    fmap f r =
        r
        { _lam = r ^. lam & V.lamResult . annotations %~ f
        , _lamPl = r ^. lamPl & f
        , _arg = r ^. arg & annotations %~ f
        }

check :: Val (Input.Payload m a) -> Maybe (Redex (Input.Payload m a))
check expr =
    do
        V.Apply func a <- expr ^? ExprLens.valApply
        l <- func ^? val . V._BLam
        Just Redex
            { _lam = l
            , _lamPl = func ^. ann
            , _bodyScope =
                func ^. ann . Input.evalResults
                <&> (^. Input.eAppliesOfLam)
                <&> Lens.traversed %~ getRedexApplies
            , _arg = a
            , _paramRefs = func ^. ann . Input.varRefsOfLambda
            }
    where
        getRedexApplies [(scopeId, _)] = scopeId
        getRedexApplies _ =
            error "redex should only be applied once per parent scope"
