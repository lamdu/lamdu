{-# LANGUAGE TemplateHaskell, TypeFamilies, FlexibleContexts #-}
module Editor.Data.Typed
  ( Entity(..), atEntityType, atEntityValue
  , EntityM, EntityT
  , Stored(..), EntityOrigin(..)
  , entityReplace
  , loadInferDefinition
  , entityGuid, entityIRef
  , writeIRef, writeIRefVia
  , foldValues, mapTypes
  )
where

import Control.Monad (liftM, (<=<))
import Data.Binary (Binary)
import Data.Map (Map)
import Data.Maybe (fromMaybe, maybeToList)
import Data.Store.Guid (Guid)
import Data.Store.IRef (IRef)
import Data.Store.Transaction (Transaction)
import Editor.Anchors (ViewTag)
import Editor.Data (Definition(..), Expression(..), Apply(..), Lambda(..), VariableRef(..))
import qualified Data.AtFieldTH as AtFieldTH
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
  , entityType :: [Entity m (Expression (Entity m))] -- Inferred types
  , entityValue :: a
  }

type EntityM m f = Entity m (f (Entity m))
type EntityT m f = EntityM (Transaction ViewTag m) f

AtFieldTH.make ''Entity

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

subst
  :: Guid
  -> Expression (Entity m)
  -> EntityM m Expression
  -> EntityM m Expression
subst guid newExpr (Entity origin mType value) =
  Entity origin mType $
  case value of
  ExpressionLambda lambda -> ExpressionLambda $ onLambda lambda
  ExpressionPi lambda -> ExpressionPi $ onLambda lambda
  ExpressionApply (Apply func arg) -> ExpressionApply $ Apply (s func) (s arg)
  var@(ExpressionGetVariable (ParameterRef guidRef))
    | guidRef == guid -> newExpr
    | otherwise -> var
  x -> x
  where
    s = subst guid newExpr
    onLambda (Lambda paramType body) = Lambda (s paramType) (s body)

alphaEq :: Map Guid Guid -> EntityT m Expression -> EntityT m Expression -> Bool
alphaEq guidMap (Entity origin0 _ value0) (Entity origin1 _ value1) =
  case (value0, value1) of
  (ExpressionLambda lambda0,
   ExpressionLambda lambda1) -> onLambda lambda0 lambda1
  (ExpressionPi lambda0,
   ExpressionPi lambda1) -> onLambda lambda0 lambda1
  (ExpressionApply (Apply func0 arg0),
   ExpressionApply (Apply func1 arg1)) ->
    alphaEq guidMap func0 func1 &&
    alphaEq guidMap arg0 arg1
  (ExpressionGetVariable (DefinitionRef iref0),
   ExpressionGetVariable (DefinitionRef iref1)) -> iref0 == iref1
  (ExpressionGetVariable (ParameterRef guidRef0),
   ExpressionGetVariable (ParameterRef guidRef1)) -> fromMaybe guidRef0 (Map.lookup guidRef0 guidMap) == guidRef1
  (ExpressionHole, ExpressionHole) -> True
  (ExpressionLiteralInteger int0,
   ExpressionLiteralInteger int1) -> int0 == int1
  _ -> False
  where
    onLambda
      (Lambda paramType0 body0)
      (Lambda paramType1 body1)
      = alphaEq guidMap paramType0 paramType1 &&
        alphaEq (Map.insert guid0 guid1 guidMap) body0 body1
    guid0 = entityOriginGuid origin0
    guid1 = entityOriginGuid origin1

unify
  :: EntityT m Expression
  -> [EntityT m Expression] -> [EntityT m Expression]
unify x xs
  | any (alphaEq Map.empty x) xs = xs
  | otherwise = (x : xs)

inferExpression
  :: Monad m
  => Scope m
  -> DataLoad.EntityT m Expression
  -> Transaction ViewTag m (EntityT m Expression)
inferExpression scope (DataLoad.Entity iref mReplace value) =
  liftM makeEntity $
  case value of
  ExpressionLambda lambda -> liftM ((,) [] . ExpressionLambda) $ inferLambda lambda
  ExpressionPi lambda -> liftM ((,) [] . ExpressionPi) $ inferLambda lambda
  ExpressionApply (Apply func arg) -> do
    inferredFunc <- inferExpression scope func
    inferredArg <- inferExpression scope arg
    let
      (applyType, modArg) = case entityType inferredFunc of
        Entity origin _ (ExpressionPi (Lambda paramType resultType)) : _ ->
          ( [subst (entityOriginGuid origin) (entityValue inferredArg) resultType]
          , atEntityType (unify paramType) inferredArg)
        _ -> ([], inferredArg) -- TODO: Split to "bad type" and "missing type"
    return (applyType, ExpressionApply (Apply inferredFunc modArg))
  ExpressionGetVariable varRef -> do
    types <- case varRef of
      ParameterRef guid -> return . maybeToList $ lookup guid scope
      DefinitionRef defI -> do
        dType <- liftM defType $ Transaction.readIRef defI
        inferredDType <-
          inferExpression [] =<< DataLoad.loadExpression dType Nothing
        return [inferredDType]
    return (types, ExpressionGetVariable varRef)
  ExpressionLiteralInteger int -> return ([], ExpressionLiteralInteger int)
  ExpressionBuiltin bi -> return ([], ExpressionBuiltin bi)
  ExpressionHole -> return ([], ExpressionHole)
  ExpressionMagic -> return ([], ExpressionMagic)
  where
    makeEntity (t, expr) =
      Entity (OriginStored (Stored iref mReplace)) t expr
    inferLambda (Lambda paramType body) = do
      inferredParamType <- inferExpression scope paramType
      liftM (Lambda inferredParamType) $
        inferExpression ((IRef.guid iref, inferredParamType) : scope) body

guidToStdGen :: Guid -> Random.StdGen
guidToStdGen = Random.mkStdGen . BinaryUtils.decodeS . Guid.bs

uniqify
  :: Map Guid Guid
  -> Random.StdGen -> EntityT m Expression -> EntityT m Expression
uniqify symbolMap gen (Entity oldOrigin ts v) =
  Entity (OriginGenerated newGuid) (uniqifyList symbolMap genT ts) $
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

uniqifyTypes :: EntityT m Expression -> EntityT m Expression
uniqifyTypes =
  foldValues f
  where
    f (Entity origin ts val) =
      Entity origin (uniqifyList Map.empty (guidToStdGen (entityOriginGuid origin)) ts) val

foldValues
  :: (EntityT m Expression -> EntityT m Expression)
  -> EntityT m Expression -> EntityT m Expression
foldValues f (Entity origin ts v) =
  f . Entity origin ts $
  case v of
  ExpressionLambda lambda -> ExpressionLambda $ onLambda lambda
  ExpressionPi lambda -> ExpressionPi $ onLambda lambda
  ExpressionApply (Apply func arg) ->
    ExpressionApply $ Apply (foldValues f func) (foldValues f arg)
  x -> x
  where
    onLambda (Lambda paramType body) =
      Lambda (foldValues f paramType) (foldValues f body)

-- | Execute on types in the Expression tree
mapTypes
  :: (EntityT m Expression -> EntityT m Expression)
  -> EntityT m Expression -> EntityT m Expression
mapTypes f =
  foldValues g
  where
    recurse = f . mapTypes f
    g (Entity origin ts v) =
      Entity origin (map recurse ts) $
      case v of
      ExpressionLambda lambda -> ExpressionLambda $ onLambda lambda
      ExpressionPi lambda -> ExpressionPi $ onLambda lambda
      x -> x
    onLambda (Lambda paramType body) = Lambda (recurse paramType) body

uniqifyList
  :: Map Guid Guid -> Random.StdGen
  -> [EntityT m Expression] -> [EntityT m Expression]
uniqifyList symbolMap =
  zipWith (uniqify symbolMap) . map Random.mkStdGen . Random.randoms

inferDefinition
  :: Monad m
  => DataLoad.EntityT m Definition
  -> Transaction ViewTag m (EntityT m Definition)
inferDefinition (DataLoad.Entity iref mReplace value) =
  liftM (Entity (OriginStored (Stored iref mReplace)) []) $
  case value of
  Definition typeI bodyI -> do
    inferredType <- liftM uniqifyTypes $ inferExpression [] typeI
    inferredBody <- liftM uniqifyTypes $ inferExpression [] bodyI
    return $ Definition inferredType inferredBody

loadInferDefinition
  :: Monad m => IRef (Definition IRef)
  -> Transaction ViewTag m (EntityT m Definition)
loadInferDefinition = inferDefinition <=< DataLoad.loadDefinition
