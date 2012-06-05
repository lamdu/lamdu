{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module Editor.Data.Load
  ( Entity(..), EntityT
  , Stored(..), EntityStored, ReplaceArg_1_0
  , guid, esGuid
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
import Editor.Data (Definition(..), Expression(..), Apply(..), Lambda(..))
import qualified Data.Store.IRef as IRef
import qualified Data.Store.Transaction as Transaction
import qualified Editor.Data.Ops as DataOps

{-# ANN module "HLint: ignore Use camelCase" #-}
type family ReplaceArg_1_0 (i :: * -> *) (a :: *)
type instance ReplaceArg_1_0 i (f k) = f i

data Stored m a = Stored
  { esIRef :: IRef a
  , esReplace :: Maybe (IRef a -> m ())
  }

type EntityStored m a = Stored m (ReplaceArg_1_0 IRef a)

-- TODO: explain..
-- How could we compare the esReplace field?
-- Do we really need this instance?
instance Eq (Stored m a) where
  Stored x _ == Stored y _ = x == y

-- Pure alternative for IRef
data Entity m a = Entity
  { entityStored :: EntityStored m a
  , entityValue :: a
  }

type EntityM m f = Entity m (f (Entity m))
type EntityT m f = EntityM (Transaction ViewTag m) f

esGuid :: Stored m a -> Guid
esGuid = IRef.guid . esIRef

guid :: Entity m a -> Guid
guid = esGuid . entityStored

load
  :: (Monad m, Binary (f IRef))
  => IRef (f IRef)
  -> (f IRef -> Transaction t m (f (Entity (Transaction t m))))
  -> Maybe (IRef (f IRef) -> Transaction t m ())
  -> Transaction t m (EntityM (Transaction t m) f)
load exprI f mSetter = do
  expr <- Transaction.readIRef exprI
  liftM (Entity (Stored exprI mSetter)) $ f expr

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
  ExpressionLiteralInteger x -> return $ ExpressionLiteralInteger x
  ExpressionHole -> return ExpressionHole
  ExpressionMagic -> return ExpressionMagic
  ExpressionBuiltin bi -> return $ ExpressionBuiltin bi
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
  Definition typeI bodyI -> do
    loadedType <-
      loadExpression typeI . Just $
      Transaction.writeIRef defI . flip Definition bodyI
    loadedExpr <-
      loadExpression bodyI . Just $
      Transaction.writeIRef defI . Definition typeI
    return $ Definition loadedType loadedExpr
