module Lamdu.Data.Infer.Load
  ( Loader(..)
  , load
  ) where

import Control.Lens.Operators
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.MonadA (MonadA)
import Data.UnionFind (Ref)
import Lamdu.Data.Infer.Monad (InferT)
import qualified Lamdu.Data.Expression as Expr
import qualified Lamdu.Data.Expression.Lens as ExprLens
import qualified Lamdu.Data.Infer.ExprRefs as ExprRefs

data Loader def m = Loader
  { loadDefType :: def -> m (Expr.Expression def ())
    -- TODO: For synonyms we'll need loadDefVal
  }

-- Error includes untyped def use
loadDef :: MonadA m => Loader def m -> def -> InferT def m (Ref, def)
loadDef (Loader loader) def =
  loader def
  & lift
  >>= ExprRefs.exprIntoContext
  <&> flip (,) def

load ::
  MonadA m =>
  Loader def m -> Expr.Expression def a ->
  InferT def m (Expr.Expression (Ref, def) a)
load loader expr = expr & ExprLens.exprDef %%~ loadDef loader
