{-# LANGUAGE TemplateHaskell #-}

module Lamdu.Sugar.Convert.Binder.Redex
    ( Redex(..)
      , bodyScope
      , lam
      , paramRefs
      , arg
      , hiddenPayloads
    , check
    ) where

import qualified Control.Lens as Lens
import           Data.CurAndPrev (CurAndPrev)
import qualified Lamdu.Calc.Val as V
import           Lamdu.Calc.Val.Annotated (Val(..))
import qualified Lamdu.Calc.Val.Annotated as Val
import           Lamdu.Eval.Results (ScopeId)
import qualified Lamdu.Expr.Lens as ExprLens
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Types

import           Lamdu.Prelude

data Redex a = Redex
    { _bodyScope :: CurAndPrev (Map ScopeId ScopeId)
    , _lam :: V.Lam (Val a)
    , _paramRefs :: [EntityId]
    , _arg :: Val a
    , _hiddenPayloads :: [a]
    } deriving (Functor, Foldable, Traversable)
Lens.makeLenses ''Redex

check :: Val (Input.Payload m a) -> Maybe (Redex (Input.Payload m a))
check expr = do
    V.Apply func a <- expr ^? ExprLens.valApply
    l <- func ^? Val.body . V._BLam
    Just Redex
        { _lam = l
        , _bodyScope =
            func ^. Val.payload . Input.evalResults
            <&> (^. Input.eAppliesOfLam)
            <&> Lens.traversed %~ getRedexApplies
        , _arg = a
        , _hiddenPayloads = (^. Val.payload) <$> [expr, func]
        , _paramRefs = func ^. Val.payload . Input.varRefsOfLambda
        }
    where
        getRedexApplies [(scopeId, _)] = scopeId
        getRedexApplies _ =
            error "redex should only be applied once per parent scope"
