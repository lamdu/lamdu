-- | Functions memoized with Data.Cache.Fenced

module Lamdu.Cache
    ( Functions(..)
    , infer
    , make, FencedCache.Cache, FencedCache.fence
    ) where

import           AST (Tree, Pure, annotations)
import           AST.Infer (ITerm, iAnnotations)
import           AST.Unify.Binding (UVar)
import qualified Control.Lens as Lens
import           Control.Monad.RWS (RWST(..))
import           Data.Cache.Fenced (Decl, function)
import qualified Data.Cache.Fenced as FencedCache
import           Lamdu.Calc.Infer (InferState, PureInfer(..), runPureInfer)
import           Lamdu.Calc.Term (Val)
import qualified Lamdu.Calc.Term as V
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Data.Definition as Definition
import           Lamdu.Sugar.Convert.Load (unmemoizedInfer, InferFunc)

import           Lamdu.Prelude

-- Like InferFunc but has the form of (a -> b) where *entire* input is
-- inside "a" and *entire* output is inside "b"
type MemoableInferFunc =
    ( Definition.Expr (Val ())
    , Tree V.Scope UVar
    , InferState
    ) ->
    Either (Tree Pure T.TypeError)
    (Tree (ITerm () UVar) V.Term, InferState)

newtype Functions = Functions
    { inferMemoized :: MemoableInferFunc
    }

-- | We know that inferMemoized retains the shape, so we strip the
-- payload and cover it after
infer :: Functions -> InferFunc a
infer funcs defExpr =
    fmap unvoid . PureInfer . RWST $
    \env s ->
    inferMemoized funcs (defExpr <&> annotations .~ (), env, s)
    <&> \(iterm, s') -> (iterm, s', ())
    where
        origExpr = defExpr ^. Definition.expr
        unvoid resExpr =
            resExpr
            & Lens.unsafePartsOf iAnnotations .~ origExpr ^.. annotations

memoableInfer :: MemoableInferFunc
memoableInfer (expr, env, state) = unmemoizedInfer expr & runPureInfer env state

decl :: Decl Functions
decl =
    Functions
    <$> function memoableInfer

make :: IO (FencedCache.Cache, Functions)
make = FencedCache.make decl
