-- | Infer expressions where GlobalId's are known to be DefI's
module Lamdu.Expr.IRef.Infer
  ( ExpressionSetter

  , loadInferScope
  , loadInferInto
  , loadInfer
  ) where

import Control.Lens.Operators
import Control.Lens.Tuple
import Control.Monad (mzero)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Trans.State (StateT(..), mapStateT)
import Control.MonadA (MonadA)
import Data.Store.Transaction (Transaction)
import Lamdu.Expr.Val (Val(..))
import Lamdu.Infer (Infer)
import Lamdu.Infer.Load (Loader(..))
import Lamdu.Infer.Unify (unify)
import Lamdu.Infer.Update (update, updateInferredVal)
import qualified Data.Store.Transaction as Transaction
import qualified Lamdu.Data.Definition as Definition
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.Val as V
import qualified Lamdu.Infer as Infer
import qualified Lamdu.Infer.Load as InferLoad
import qualified Lamdu.Infer.Recursive as Recursive

type T = Transaction

type ExpressionSetter def = Val () -> Val ()

loader :: MonadA m => Loader (MaybeT (T m))
loader =
  Loader loadType
  where
    loadType globalId = do
      defBody <- lift $ Transaction.readIRef $ ExprIRef.defI globalId
      case defBody of
        Definition.BodyExpr (Definition.Expr _ (Definition.ExportedType scheme)) ->
          return scheme
        Definition.BodyBuiltin (Definition.Builtin _ scheme) -> return scheme
        _ -> mzero -- Reference to global with non-exported type!

eitherToMaybeT :: Monad m => Either l a -> MaybeT m a
eitherToMaybeT (Left _) = MaybeT $ return Nothing
eitherToMaybeT (Right x) = MaybeT $ return $ Just x

type M m = StateT Infer.Context (MaybeT (T m))

liftInfer :: Monad m => Infer a -> M m a
liftInfer = mapStateT eitherToMaybeT . Infer.run

loadInferScope ::
  MonadA m => Infer.Scope -> Val a -> M m (Val (Infer.Payload, a))
loadInferScope scope val = do
  inferAction <- lift $ InferLoad.loadInfer loader scope val
  liftInfer inferAction

loadInferInto ::
  MonadA m => Infer.Payload -> Val a -> M m (Val (Infer.Payload, a))
loadInferInto pl val = do
  inferredVal <- loadInferScope (pl ^. Infer.plScope) val
  let inferredType = inferredVal ^. V.payload . _1 . Infer.plType
  liftInfer $ do
    update (pl ^. Infer.plType)
      >>= unify inferredType
    updateInferredVal inferredVal

loadInfer ::
  MonadA m => V.Var -> Val a ->
  MaybeT (T m) (Val (Infer.Payload, a), Infer.Context)
loadInfer recurseGetVar val =
  liftInfer (Recursive.inferEnv recurseGetVar Infer.emptyScope)
  >>= (`loadInferInto` val)
  & (`runStateT` Infer.initialContext)

