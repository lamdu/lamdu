module Lamdu.Data.Infer.Load
  ( Loader(..)
  , LoadError(..)
  , LoadedDef(..), ldDef, ldType -- re-export
  , load
  ) where

import Control.Lens.Operators
import Control.Monad (when)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Either (EitherT(..))
import Control.Monad.Trans.State (StateT)
import Control.MonadA (MonadA)
import Lamdu.Data.Infer.Internal
import qualified Control.Lens as Lens
import qualified Control.Monad.Trans.Either as Either
import qualified Lamdu.Data.Expression as Expr
import qualified Lamdu.Data.Expression.Lens as ExprLens
import qualified Lamdu.Data.Infer.ExprRefs as ExprRefs

data Loader def m = Loader
  { loadDefType :: def -> m (Expr.Expression def ())
    -- TODO: For synonyms we'll need loadDefVal
  }

newtype LoadError def = LoadUntypedDef def

-- Error includes untyped def use
loadDef ::
  MonadA m =>
  Loader def m -> def ->
  StateT (Context def) (EitherT (LoadError def) m) (LoadedDef def)
loadDef (Loader loader) def = do
  loadedDefType <- lift . lift $ loader def
  when (Lens.has ExprLens.holePayloads loadedDefType) .
    lift . Either.left $ LoadUntypedDef def
  ExprRefs.exprIntoContext loadedDefType
    <&> LoadedDef def

load ::
  MonadA m =>
  Loader def m -> Expr.Expression def a ->
  StateT (Context def) (EitherT (LoadError def) m)
    (Expr.Expression (LoadedDef def) a)
load loader = ExprLens.exprDef %%~ loadDef loader
