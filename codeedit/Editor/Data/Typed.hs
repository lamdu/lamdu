{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module Editor.Data.Typed
  ( Entity(..), EntityM, EntityT
  , Stored(..), EntityOrigin(..)
  , entityReplace
  , inferTypes
  , entityGuid, entityIRef
  , writeIRef, writeIRefVia
  )
where

import Control.Monad (liftM, liftM2, (<=<))
import Data.Binary (Binary)
import Data.Store.Guid (Guid)
import Data.Store.IRef (IRef)
import Data.Store.Transaction (Transaction)
import Editor.Anchors (ViewTag)
import Editor.Data (Definition(..), Expression(..), Apply(..), Lambda(..), Builtin(..))
import qualified Data.Store.IRef as IRef
import qualified Data.Store.Transaction as Transaction
import qualified Editor.Data.Load as DataLoad

{-# ANN module "HLint: ignore Use camelCase" #-}
type family ReplaceArg_1_0 (i :: * -> *) (a :: *)
type instance ReplaceArg_1_0 i (f k) = f i

data Stored m a = Stored
  { esIRef :: IRef a
  , esReplace :: Maybe (IRef a -> m ())
  }

data EntityOrigin m a = OriginGenerated Guid | OriginStored (Stored m (ReplaceArg_1_0 IRef a))

entityOriginGuid :: EntityOrigin m a -> Guid
entityOriginGuid (OriginGenerated guid) = guid
entityOriginGuid (OriginStored stored) = IRef.guid $ esIRef stored

entityGuid :: Entity m a -> Guid
entityGuid = entityOriginGuid . entityOrigin

data Entity m a = Entity
  { entityOrigin :: EntityOrigin m a
  , entityType :: Maybe (Entity m (Expression (Entity m))) -- Inferred type
  , entityValue :: a
  }

type TypedEntity m = Entity (Transaction ViewTag m)

entityOriginStored
  :: EntityOrigin m a
  -> Maybe (Stored m (ReplaceArg_1_0 IRef a))
entityOriginStored (OriginGenerated _) = Nothing
entityOriginStored (OriginStored x) = Just x

entityStored :: Entity m a -> Maybe (Stored m (ReplaceArg_1_0 IRef a))
entityStored = entityOriginStored . entityOrigin

entityReplace :: Entity m a -> Maybe (IRef (ReplaceArg_1_0 IRef a) -> m ())
entityReplace = esReplace <=< entityStored

entityIRef :: Entity m a -> Maybe (IRef (ReplaceArg_1_0 IRef a))
entityIRef = fmap esIRef . entityStored

writeIRef
  :: (Monad m, Binary (ReplaceArg_1_0 IRef a))
  => TypedEntity m a
  -> Maybe (ReplaceArg_1_0 IRef a -> Transaction t m ())
writeIRef = fmap Transaction.writeIRef . entityIRef

argument :: (a -> b) -> (b -> c) -> a -> c
argument = flip (.)

writeIRefVia
  :: (Monad m, Binary (ReplaceArg_1_0 IRef b))
  => (a -> ReplaceArg_1_0 IRef b)
  -> TypedEntity m b
  -> Maybe (a -> Transaction t m ())
writeIRefVia f = (fmap . fmap . argument) f writeIRef

type EntityM m f = Entity m (f (Entity m))
type EntityT m f = EntityM (Transaction ViewTag m) f

inferExpression
  :: Monad m
  => DataLoad.EntityT m Expression
  -> Transaction ViewTag m (EntityT m Expression)
inferExpression (DataLoad.Entity iref mReplace value) =
  liftM (Entity (OriginStored (Stored iref mReplace)) Nothing) $
  case value of
  ExpressionLambda lambda -> liftM ExpressionLambda $ inferLambda lambda
  ExpressionPi lambda -> liftM ExpressionPi $ inferLambda lambda
  ExpressionApply apply -> liftM ExpressionApply $ inferApply apply
  ExpressionGetVariable varRef -> return $ ExpressionGetVariable varRef
  ExpressionHole -> return ExpressionHole
  ExpressionLiteralInteger int -> return $ ExpressionLiteralInteger int
  where
    inferApply (Apply func arg) =
      liftM2 Apply (inferExpression func) (inferExpression arg)
    inferLambda (Lambda paramType body) =
      liftM2 Lambda (inferExpression paramType) (inferExpression body)

inferTypes
  :: Monad m
  => DataLoad.EntityT m Definition
  -> Transaction ViewTag m (EntityT m Definition)
inferTypes (DataLoad.Entity iref mReplace value) =
  liftM (Entity (OriginStored (Stored iref mReplace)) Nothing) $
  case value of
  DefinitionExpression expr ->
    liftM DefinitionExpression $ inferExpression expr
  DefinitionBuiltin builtin ->
    liftM DefinitionBuiltin $ inferBuiltin builtin
  where
    inferBuiltin (Builtin name t) = liftM (Builtin name) $ inferExpression t
