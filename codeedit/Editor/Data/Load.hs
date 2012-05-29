{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module Editor.Data.Load
  ( Entity(..), EntityType(..), WritableEntityData(..)
  , EntityT
  , guid
  , loadDefinition
  , replacer, iref
  , writeIRef, writeIRefVia
  )
where

import Control.Monad (liftM, liftM2, (<=<))
import Data.Binary (Binary)
import Data.Store.Guid (Guid)
import Data.Store.IRef (IRef)
import Data.Store.Transaction (Transaction)
import Editor.Anchors (ViewTag)
import Editor.Data (Definition(..), Builtin(..), Expression(..), Apply(..), Lambda(..))
import qualified Data.Store.IRef as IRef
import qualified Data.Store.Transaction as Transaction
import qualified Editor.Data.Ops as DataOps

{-# ANN module "HLint: ignore Use camelCase" #-}

-- Have to use type families to avoid infinite kinds.
type family ReplaceArg_1_0 (i :: * -> *) (a :: *)
type instance ReplaceArg_1_0 i (f k) = f i

data WritableEntityData m a = WritableEntityData
  { wedIRef :: IRef a
  , wedReplace :: Maybe (IRef a -> m ())
  }

data EntityType m a = ReadOnly Guid | Writable (WritableEntityData m a)

-- Pure alternative for IRef
data Entity m a = Entity
  { entityType :: EntityType m (ReplaceArg_1_0 IRef a)
  , entityValue :: a
  }

type EntityM m f = Entity m (f (Entity m))
type EntityT m f = EntityM (Transaction ViewTag m) f

guidT :: EntityType m a -> Guid
guidT (ReadOnly g) = g
guidT (Writable i) = IRef.guid $ wedIRef i

guid :: Entity m a -> Guid
guid = guidT . entityType

writableEntityData
  :: Entity m a
  -> Maybe (WritableEntityData m (ReplaceArg_1_0 IRef a))
writableEntityData =
  f . entityType
  where
    f (ReadOnly _) = Nothing
    f (Writable i) = Just i

replacer
  :: Entity m a
  -> Maybe (IRef (ReplaceArg_1_0 IRef a) -> m ())
replacer = wedReplace <=< writableEntityData

iref :: Entity m a -> Maybe (IRef (ReplaceArg_1_0 IRef a))
iref = fmap wedIRef . writableEntityData

writeIRef
  :: (Monad m, Binary (ReplaceArg_1_0 IRef a))
  => Entity (Transaction ViewTag m) a
  -> Maybe (ReplaceArg_1_0 IRef a -> Transaction t m ())
writeIRef = fmap Transaction.writeIRef . iref

argument :: (a -> b) -> (b -> c) -> a -> c
argument = flip (.)

writeIRefVia
  :: (Monad m, Binary (ReplaceArg_1_0 IRef b))
  => (a -> ReplaceArg_1_0 IRef b)
  -> Entity (Transaction ViewTag m) b
  -> Maybe (a -> Transaction t m ())
writeIRefVia f = (fmap . fmap . argument) f writeIRef

load
  :: (Monad m, Binary (f IRef))
  => IRef (f IRef)
  -> (f IRef -> Transaction t m (f (Entity (Transaction t m))))
  -> Maybe (IRef (f IRef) -> Transaction t m ())
  -> Transaction t m (EntityM (Transaction t m) f)
load exprI f mSetter = do
  expr <- Transaction.readIRef exprI
  liftM (Entity (Writable (WritableEntityData exprI mSetter))) $ f expr

loadExpression
  :: Monad m
  => IRef (Expression IRef)
  -> Maybe (IRef (Expression IRef) -> Transaction ViewTag m ())
  -> Transaction ViewTag m (EntityT m Expression)
loadExpression exprI = load exprI $ \expr -> case expr of
  ExpressionLambda lambda ->
    liftM ExpressionLambda $ loadLambda ExpressionLambda lambda
  ExpressionPi lambda ->
    liftM ExpressionPi $ loadLambda ExpressionPi lambda
  ExpressionApply apply@(Apply funcI argI) ->
    liftM ExpressionApply $
    liftM2 Apply
    (loadExpression funcI (Just (DataOps.applyFuncSetter exprI apply)))
    (loadExpression argI (Just (DataOps.applyArgSetter exprI apply)))
  ExpressionGetVariable x -> return $ ExpressionGetVariable x
  ExpressionHole -> return ExpressionHole
  ExpressionLiteralInteger x -> return $ ExpressionLiteralInteger x
  where
    loadLambda cons lambda@(Lambda argType body) =
      liftM2 Lambda
      (loadExpression argType
       (Just (DataOps.lambdaTypeSetter cons exprI lambda)))
      (loadExpression body
       (Just (DataOps.lambdaBodySetter cons exprI lambda)))

loadDefinition
  :: Monad m
  => IRef (Definition IRef)
  -> Transaction ViewTag m (EntityT m Definition)
loadDefinition defI = flip (load defI) Nothing $ \def -> case def of
    DefinitionBuiltin (Builtin ffiName bType) ->
      liftM (DefinitionBuiltin . Builtin ffiName) .
      loadExpression bType $ Just
      (Transaction.writeIRef defI .
       DefinitionBuiltin .
       Builtin ffiName)
    DefinitionExpression expr ->
      liftM DefinitionExpression .
      loadExpression expr $ Just
      (Transaction.writeIRef defI .
       DefinitionExpression)
