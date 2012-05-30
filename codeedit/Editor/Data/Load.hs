{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module Editor.Data.Load
  ( Entity(..)
  , EntityT
  , guid
  , loadDefinition
  , loadExpression
  )
where

import Control.Monad (liftM, liftM2)
import Data.Binary (Binary)
import Data.Store.Guid (Guid)
import Data.Store.IRef (IRef)
import Data.Store.Transaction (Transaction)
import Editor.Anchors (ViewTag)
import Editor.Data (Definition(..), DefinitionBody(..), Expression(..), Apply(..), Lambda(..))
import qualified Data.Store.IRef as IRef
import qualified Data.Store.Transaction as Transaction
import qualified Editor.Data.Ops as DataOps

{-# ANN module "HLint: ignore Use camelCase" #-}
type family ReplaceArg_1_0 (i :: * -> *) (a :: *)
type instance ReplaceArg_1_0 i (f k) = f i

-- Pure alternative for IRef
data Entity m a = Entity
  { entityIRef :: IRef (ReplaceArg_1_0 IRef a)
  , entityReplace :: Maybe (IRef (ReplaceArg_1_0 IRef a) -> m ())
  , entityValue :: a
  }

type EntityM m f = Entity m (f (Entity m))
type EntityT m f = EntityM (Transaction ViewTag m) f

guid :: Entity m a -> Guid
guid = IRef.guid . entityIRef

load
  :: (Monad m, Binary (f IRef))
  => IRef (f IRef)
  -> (f IRef -> Transaction t m (f (Entity (Transaction t m))))
  -> Maybe (IRef (f IRef) -> Transaction t m ())
  -> Transaction t m (EntityM (Transaction t m) f)
load exprI f mSetter = do
  expr <- Transaction.readIRef exprI
  liftM (Entity exprI mSetter) $ f expr

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
loadDefinition defI =
  flip (load defI) Nothing $ \def ->
  case def of
  Definition typeI body -> do
    loadedType <-
      loadExpression typeI . Just $
      Transaction.writeIRef defI . flip Definition body
    liftM (Definition loadedType) $
      case body of
      DefinitionMagic -> return DefinitionMagic
      DefinitionBuiltin ffiName -> return $ DefinitionBuiltin ffiName
      DefinitionExpression expr -> do
        loadedExpr <-
          loadExpression expr . Just $
          Transaction.writeIRef defI . Definition typeI . DefinitionExpression
        return $ DefinitionExpression loadedExpr
