-- | Functions memoized with Data.Cache.Fenced

module Lamdu.Cache
    ( Functions(..)
    , HasFunctions(..)
    , infer
    , make, FencedCache.Cache, FencedCache.fence
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.State (StateT(..))
import           Data.Cache.Fenced (Decl, function)
import qualified Data.Cache.Fenced as FencedCache
import           Lamdu.Calc.Val.Annotated (Val)
import qualified Lamdu.Data.Definition as Definition
import qualified Lamdu.Infer as Infer
import           Lamdu.Infer (InferCtx(..))
import qualified Lamdu.Infer.Error as Infer
import           Lamdu.Sugar.Convert.Load (unmemoizedInfer, InferFunc)

import           Lamdu.Prelude

-- Like InferFunc but has the form of (a -> b) where *entire* input is
-- inside "a" and *entire* output is inside "b"
type MemoableInferFunc =
    ( Definition.Expr (Val ())
    , Infer.Scope
    , Infer.Context
    ) -> Either Infer.Error (Val Infer.Payload, Infer.Context)

newtype Functions = Functions
    { inferMemoized :: MemoableInferFunc
    }

class HasFunctions env where
    functions :: Lens' env Functions

instance HasFunctions Functions where
    functions = id

-- | We know that inferMemoized retains the shape, so we strip the
-- payload and cover it after
infer :: Functions -> InferFunc a
infer funcs defExpr scope =
    fmap unvoid . Infer . StateT $
    \ctx ->
    inferMemoized funcs (defExpr <&> void, scope, ctx)
    where
        unvoid resExpr =
            defExpr ^. Definition.expr
            & Lens.unsafePartsOf traverse %~ zip (resExpr ^.. Lens.folded)

memoableInfer :: MemoableInferFunc
memoableInfer (expr, scope, ctx) =
    unmemoizedInfer expr scope
    & Infer.run
    & (`runStateT` ctx)
    & Lens._Right . _1 . Lens.mapped %~ \(x, ~()) -> x

decl :: Decl Functions
decl =
    Functions
    <$> function memoableInfer

make :: IO (FencedCache.Cache, Functions)
make = FencedCache.make decl
