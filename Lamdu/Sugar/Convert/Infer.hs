{-# LANGUAGE ConstraintKinds #-}
module Lamdu.Sugar.Convert.Infer
  ( ExpressionSetter

  , RecursionEnv, reDefI

  , loadInferScope
  , loadInferInto
  , loadInfer
  ) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Lens.Operators
import Control.Lens.Tuple
import Control.Monad (mzero)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Trans.State (StateT(..), mapStateT)
import Control.MonadA (MonadA)
import Data.Store.Transaction (Transaction)
import Lamdu.Expr.Identifier (Identifier(..))
import Lamdu.Expr.Val (Val(..))
import Lamdu.Infer (Infer)
import Lamdu.Infer.Load (Loader(..))
import Lamdu.Infer.Unify (unify)
import Lamdu.Infer.Update (updateInferredVal, update)
import qualified Control.Lens as Lens
import qualified Data.Store.Guid as Guid
import qualified Data.Store.IRef as IRef
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Lamdu.Data.Definition as Definition
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.Scheme as Scheme
import qualified Lamdu.Expr.Type as T
import qualified Lamdu.Expr.UniqueId as UniqueId
import qualified Lamdu.Expr.Val as V
import qualified Lamdu.Infer as Infer
import qualified Lamdu.Infer.Load as InferLoad
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import qualified Lamdu.Sugar.Types as Sugar

type T = Transaction

type ExpressionSetter def = Val () -> Val ()

data RecursionEnv m = RecursionEnv
  { _reDefI :: ExprIRef.DefI m
  , reType :: T.Type
  }

reDefI :: RecursionEnv m -> ExprIRef.DefI m
reDefI = _reDefI

makeRecursionEnv :: ExprIRef.DefI m -> RecursionEnv m
makeRecursionEnv defI =
  RecursionEnv defI $ T.liftVar defTV
  where
    defTV = T.Var $ Identifier $ Guid.bs $ UniqueId.toGuid defI

loader :: MonadA m => RecursionEnv m -> Loader (MaybeT (T m))
loader (RecursionEnv recursiveDefI recursiveType) =
  Loader loadType
  where
    loadType globalId
      | defI == recursiveDefI = return $ Scheme.mono $ recursiveType
      | otherwise = do
        defBody <- lift $ Transaction.readIRef defI
        case defBody of
          Definition.BodyExpr (Definition.Expr _ (Definition.ExportedType scheme)) ->
            return scheme
          Definition.BodyBuiltin (Definition.Builtin _ scheme) -> return scheme
          _ -> mzero -- Reference to global with non-exported type!
      where
        defI = ExprIRef.defI globalId

eitherToMaybeT :: Monad m => Either l a -> MaybeT m a
eitherToMaybeT (Left _) = MaybeT $ return Nothing
eitherToMaybeT (Right x) = MaybeT $ return $ Just x

type M m = StateT Infer.Context (MaybeT (T m))

liftInfer :: Monad m => Infer a -> M m a
liftInfer = mapStateT eitherToMaybeT . Infer.run

loadInferScope ::
  MonadA m => RecursionEnv m -> Infer.Scope -> Val a -> M m (Val (Infer.Payload, a))
loadInferScope recursionEnv scope val = do
  inferAction <- lift $ InferLoad.loadInfer (loader recursionEnv) scope val
  liftInfer inferAction

loadInferInto ::
  MonadA m => RecursionEnv m -> Infer.Payload -> Val a -> M m (Val (Infer.Payload, a))
loadInferInto recursionEnv pl val = do
  inferredVal <- loadInferScope recursionEnv (pl ^. Infer.plScope) val
  let inferredType = inferredVal ^. V.payload . _1 . Infer.plType
  liftInfer $ do
    unify inferredType (pl ^. Infer.plType)
    updateInferredVal inferredVal

loadInfer ::
  MonadA m => ExprIRef.DefI m -> Val (ExprIRef.ValIProperty m) ->
  MaybeT (T m) ((Val (Sugar.InputPayload m ()), RecursionEnv m), Infer.Context)
loadInfer defI val =
  loadInferScope initialRecursionEnv Infer.emptyScope val
  >>= unifyWithRecursionEnv
  & (`runStateT` Infer.initialContext)
  <&> _1 . _1 . Lens.mapped %~ mkInputPayload
  where
    initialRecursionEnv = makeRecursionEnv defI
    unifyWithRecursionEnv inferredVal =
      liftInfer $ do
        let resType = inferredVal ^. V.payload . _1 . Infer.plType
        unify (reType initialRecursionEnv) resType
        (,) <$> updateInferredVal inferredVal
            <*> (RecursionEnv defI <$> update (reType initialRecursionEnv))

    mkInputPayload (inferPl, stored) = Sugar.InputPayload
      { Sugar._ipEntityId = EntityId.ofValI $ Property.value stored
      , Sugar._ipInferred = inferPl
      , Sugar._ipStored = Just stored
      , Sugar._ipData = ()
      , Sugar._ipGuid = IRef.guid $ ExprIRef.unValI $ Property.value stored
      }
