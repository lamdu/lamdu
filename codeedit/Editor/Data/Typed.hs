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

import Control.Monad (liftM, (<=<))
import Data.Binary (Binary)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Store.Guid (Guid)
import Data.Store.IRef (IRef)
import Data.Store.Transaction (Transaction)
import Editor.Anchors (ViewTag)
import Editor.Data (Definition(..), DefinitionBody(..), Expression(..), Apply(..), Lambda(..), VariableRef(..))
import qualified Data.Binary.Utils as BinaryUtils
import qualified Data.Map as Map
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
  ExpressionApply (Apply func arg) -> do
    inferredFunc <- inferExpression scope func
    let
      applyType = case entityType inferredFunc of
        Just (Entity _ _ (ExpressionPi (Lambda _ resultType))) ->
          Just resultType
        _ -> Nothing -- TODO: Split to "bad type" and "missing type"
    liftM ((,) applyType . ExpressionApply . Apply inferredFunc) $
      inferExpression scope arg
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
      (fmap (uniqify Map.empty (guidToStdGen (IRef.guid iref))) t) expr
    inferLambda (Lambda paramType body) = do
      inferredParamType <- inferExpression scope paramType
      liftM (Lambda inferredParamType) $
        inferExpression ((IRef.guid iref, inferredParamType) : scope) body

guidToStdGen :: Guid -> Random.StdGen
guidToStdGen = Random.mkStdGen . BinaryUtils.decodeS . Guid.bs

uniqify
  :: Map Guid Guid
  -> Random.StdGen -> EntityT m Expression -> EntityT m Expression
uniqify symbolMap gen (Entity oldOrigin t v) =
  Entity (OriginGenerated newGuid) (fmap (u genT) t) $
  case v of
  ExpressionLambda lambda -> ExpressionLambda $ onLambda lambda
  ExpressionPi lambda -> ExpressionPi $ onLambda lambda
  ExpressionApply (Apply func arg) ->
    ExpressionApply $ Apply (u genV0 func) (u genV1 arg)
  ExpressionGetVariable (ParameterRef guid) ->
    ExpressionGetVariable . ParameterRef . fromMaybe guid $
    Map.lookup guid symbolMap
  x -> x
  where
    oldGuid = entityOriginGuid oldOrigin
    u = uniqify symbolMap
    (genV0, genV1) = Random.split genV
    onLambda (Lambda paramType body) =
      Lambda (u genV0 paramType)
      (uniqify (Map.insert oldGuid newGuid symbolMap) genV1 body)
    (newGuid, newGen) = Random.random gen
    (genT, genV) = Random.split newGen

inferDefinition
  :: Monad m
  => DataLoad.EntityT m Definition
  -> Transaction ViewTag m (EntityT m Definition)
inferDefinition (DataLoad.Entity iref mReplace value) =
  liftM (Entity (OriginStored (Stored iref mReplace)) Nothing) $
  case value of
  Definition typeI body -> do
    inferredType <- inferExpression [] typeI
    liftM (Definition inferredType) $
      case body of
      DefinitionExpression expr -> liftM DefinitionExpression $ inferExpression [] expr
      DefinitionBuiltin ffiName -> return $ DefinitionBuiltin ffiName
