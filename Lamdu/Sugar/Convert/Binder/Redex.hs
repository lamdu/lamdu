{-# LANGUAGE NoImplicitPrelude, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Lamdu.Sugar.Convert.Binder.Redex
    ( Redex(..)
    , checkForRedex
    ) where

import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Data.CurAndPrev (CurAndPrev)
import           Data.Map (Map)
import           Lamdu.Eval.Val (ScopeId)
import qualified Lamdu.Expr.Lens as ExprLens
import           Lamdu.Expr.Val (Val(..))
import qualified Lamdu.Expr.Val as V
import           Lamdu.Sugar.Convert.Expression.Actions (makeAnnotation)
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Types

import           Prelude.Compat

data Redex a = Redex
    { redexBody :: Val a
    , redexBodyScope :: CurAndPrev (Map ScopeId ScopeId)
    , redexParam :: V.Var
    , redexParamRefs :: [EntityId]
    , redexArg :: Val a
    , redexHiddenPayloads :: [a]
    , redexArgAnnotation :: Annotation
    } deriving (Functor, Foldable, Traversable)

checkForRedex :: Val (Input.Payload m a) -> Maybe (Redex (Input.Payload m a))
checkForRedex expr = do
    V.Apply func arg <- expr ^? ExprLens.valApply
    V.Lam param body <- func ^? V.body . ExprLens._BAbs
    Just Redex
        { redexBody = body
        , redexBodyScope =
            func ^. V.payload . Input.evalResults
            <&> (^. Input.eAppliesOfLam)
            <&> Lens.traversed %~ getRedexApplies
        , redexParam = param
        , redexArg = arg
        , redexHiddenPayloads = (^. V.payload) <$> [expr, func]
        , redexArgAnnotation = makeAnnotation (arg ^. V.payload)
        , redexParamRefs = func ^. V.payload . Input.varRefsOfLambda
        }
    where
        getRedexApplies [(scopeId, _)] = scopeId
        getRedexApplies _ =
            error "redex should only be applied once per parent scope"
