{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module Editor.Data.Typed
  ( Entity(..), EntityM, EntityT
  , Stored(..), EntityOrigin(..)
  , entityReplace
  , inferDefinition
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
import Editor.Data (Definition(..), Expression(..), Apply(..), Lambda(..), Builtin(..), VariableRef(..))
import qualified Data.Binary.Utils as BinaryUtils
import qualified Data.Store.Guid as Guid
import qualified Data.Store.IRef as IRef
import qualified Data.Store.Transaction as Transaction
import qualified Editor.Data.Load as DataLoad
import qualified System.Random as Random

{-# ANN module "HLint: ignore Use camelCase" #-}
type family ReplaceArg_1_0 (i :: * -> *) (a :: *)
type instance ReplaceArg_1_0 i (f k) = f i

data Stored m a = Stored
  { esIRef :: IRef a
  , esReplace :: Maybe (IRef a -> m ())
  }

data EntityOrigin m a
  = OriginGenerated Guid
  | OriginStored (Stored m (ReplaceArg_1_0 IRef a))

type Scope m = [(Guid, EntityT m Expression)]

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
  => Scope m
  -> DataLoad.EntityT m Expression
  -> Transaction ViewTag m (EntityT m Expression)
inferExpression scope (DataLoad.Entity iref mReplace value) =
  liftM makeEntity $
  case value of
  ExpressionLambda lambda -> liftM ((,) Nothing . ExpressionLambda) $ inferLambda lambda
  ExpressionPi lambda -> liftM ((,) Nothing . ExpressionPi) $ inferLambda lambda
  ExpressionApply apply -> liftM ((,) Nothing . ExpressionApply) $ inferApply apply
  ExpressionGetVariable varRef ->
    return
    ( case varRef of
      ParameterRef guid -> lookup guid scope
      DefinitionRef _ -> Nothing
    , ExpressionGetVariable varRef
    )
  ExpressionHole -> return (Nothing, ExpressionHole)
  ExpressionLiteralInteger int -> return (Nothing, ExpressionLiteralInteger int)
  where
    makeEntity (t, expr) =
      Entity (OriginStored (Stored iref mReplace))
      (fmap (uniqify (guidToStdGen (IRef.guid iref))) t) expr
    inferApply (Apply func arg) =
      liftM2 Apply (inferExpression scope func) (inferExpression scope arg)
    inferLambda (Lambda paramType body) = do
      inferredParamType <- inferExpression scope paramType
      liftM (Lambda inferredParamType) $
        inferExpression ((IRef.guid iref, inferredParamType) : scope) body

guidToStdGen :: Guid -> Random.StdGen
guidToStdGen = Random.mkStdGen . BinaryUtils.decodeS . Guid.bs

uniqify :: Random.StdGen -> EntityT m Expression -> EntityT m Expression
uniqify gen (Entity _ t v) =
  Entity (OriginGenerated newGuid) (fmap (uniqify genT) t) $
  case v of
  ExpressionLambda lambda -> ExpressionLambda $ onLambda lambda
  ExpressionPi lambda -> ExpressionPi $ onLambda lambda
  ExpressionApply (Apply func arg) ->
    ExpressionApply $ Apply (uniqify genV0 func) (uniqify genV1 arg)
  x -> x
  where
    (genV0, genV1) = Random.split genV
    onLambda (Lambda paramType body) =
      Lambda (uniqify genV0 paramType) (uniqify genV1 body)
    (newGuid, newGen) = Random.random gen
    (genT, genV) = Random.split newGen

inferDefinition
  :: Monad m
  => DataLoad.EntityT m Definition
  -> Transaction ViewTag m (EntityT m Definition)
inferDefinition (DataLoad.Entity iref mReplace value) =
  liftM (Entity (OriginStored (Stored iref mReplace)) Nothing) $
  case value of
  DefinitionExpression expr ->
    liftM DefinitionExpression $ inferExpression [] expr
  DefinitionBuiltin builtin ->
    liftM DefinitionBuiltin $ inferBuiltin builtin
  where
    inferBuiltin (Builtin name t) = liftM (Builtin name) $ inferExpression [] t
