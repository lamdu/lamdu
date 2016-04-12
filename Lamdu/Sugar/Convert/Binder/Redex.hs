{-# LANGUAGE NoImplicitPrelude, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Lamdu.Sugar.Convert.Binder.Redex
    ( Redex(..)
    , checkForRedex
    ) where

import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Data.CurAndPrev (CurAndPrev)
import           Data.Map (Map)
import           Lamdu.Eval.Results (ScopeId)
import qualified Lamdu.Expr.Lens as ExprLens
import           Lamdu.Expr.Type (Type)
import qualified Lamdu.Expr.Val as V
import           Lamdu.Expr.Val.Annotated (Val(..))
import qualified Lamdu.Expr.Val.Annotated as Val
import qualified Lamdu.Infer as Infer
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Types

import           Prelude.Compat

data Redex a = Redex
    { redexBodyScope :: CurAndPrev (Map ScopeId ScopeId)
    , redexLam :: V.Lam (Val a)
    , redexParamRefs :: [EntityId]
    , redexArg :: Val a
    , redexArgType :: Type
    , redexHiddenPayloads :: [a]
    } deriving (Functor, Foldable, Traversable)

checkForRedex :: Val (Input.Payload m a) -> Maybe (Redex (Input.Payload m a))
checkForRedex expr = do
    V.Apply func arg <- expr ^? ExprLens.valApply
    lam <- func ^? Val.body . ExprLens._BLam
    Just Redex
        { redexLam = lam
        , redexBodyScope =
            func ^. Val.payload . Input.evalResults
            <&> (^. Input.eAppliesOfLam)
            <&> Lens.traversed %~ getRedexApplies
        , redexArg = arg
        , redexArgType =
            arg ^. Val.payload . Input.inferred . Infer.plType
        , redexHiddenPayloads = (^. Val.payload) <$> [expr, func]
        , redexParamRefs = func ^. Val.payload . Input.varRefsOfLambda
        }
    where
        getRedexApplies [(scopeId, _)] = scopeId
        getRedexApplies _ =
            error "redex should only be applied once per parent scope"
