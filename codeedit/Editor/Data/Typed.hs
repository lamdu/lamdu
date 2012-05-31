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

import Control.Monad (liftM, liftM2, (<=<))
import Data.Binary (Binary)
import Data.Map (Map)
import Data.Maybe (fromMaybe, maybeToList)
import Data.Monoid (mempty)
import Data.Store.Guid (Guid)
import Data.Store.IRef (IRef)
import Data.Store.Transaction (Transaction)
import Editor.Anchors (ViewTag)
import Editor.Data (Definition(..), Expression(..), FFIName(..), Apply(..), Lambda(..), VariableRef(..))
import qualified Data.AtFieldTH as AtFieldTH
import qualified Data.Binary.Utils as BinaryUtils
import qualified Data.Functor.Identity as Identity
import qualified Data.Map as Map
import qualified Data.Store.Guid as Guid
import qualified Data.Store.IRef as IRef
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Editor.Anchors as Anchors
import qualified Editor.Data.Load as DataLoad
import qualified System.Random as Random

{-# ANN module "HLint: ignore Use camelCase" #-}
type family ReplaceArg_1_0 (i :: * -> *) (a :: *)
type instance ReplaceArg_1_0 i (f k) = f i

data Stored m a = Stored
  { esIRef :: IRef a
  , esReplace :: Maybe (IRef a -> m ())
  }

-- TODO: explain..
-- How could we compare the esReplace field?
instance Eq (Stored m a) where
  Stored x _ == Stored y _ = x == y

data EntityOrigin m a
  = OriginGenerated Guid
  | OriginStored (Stored m (ReplaceArg_1_0 IRef a))
  deriving Eq

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
  } deriving Eq

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

alphaEq :: EntityT m Expression -> EntityT m Expression -> Bool
alphaEq e0 e1 =
  uniqify Map.empty gen e0 == uniqify Map.empty gen e1
  where
    gen = Random.mkStdGen 0

unify
  :: EntityT m Expression
  -> [EntityT m Expression] -> [EntityT m Expression]
unify x xs
  | any (alphaEq x) xs = xs
  | otherwise = (x : xs)

expand :: Monad m => EntityT m Expression -> Transaction ViewTag m (EntityT m Expression)
expand =
  foldValues f
  where
    f (Entity _ _ (ExpressionGetVariable (DefinitionRef defI))) = do
      def <- Transaction.readIRef defI
      liftM convertExpression $ DataLoad.loadExpression (defBody def) Nothing
    f (Entity _ _ (ExpressionApply (Apply (Entity lambdaOrigin _ (ExpressionLambda (Lambda _ body))) val))) =
      return $ subst (entityOriginGuid lambdaOrigin) (entityValue val) body
    f x = return x

convertExpression :: DataLoad.EntityT m Expression -> EntityT m Expression
convertExpression (DataLoad.Entity iref mReplace value) =
  Entity (OriginStored (Stored iref mReplace)) [] $
  case value of
  ExpressionLambda lambda -> ExpressionLambda $ convertLambda lambda
  ExpressionPi lambda -> ExpressionPi $ convertLambda lambda
  ExpressionApply (Apply func arg) ->
    ExpressionApply $ Apply (convertExpression func) (convertExpression arg)
  ExpressionGetVariable varRef -> ExpressionGetVariable varRef
  ExpressionLiteralInteger int -> ExpressionLiteralInteger int
  ExpressionBuiltin bi -> ExpressionBuiltin bi
  ExpressionHole -> ExpressionHole
  ExpressionMagic -> ExpressionMagic
  where
    convertLambda (Lambda paramType body) = Lambda (convertExpression paramType) (convertExpression body)

inferExpression
  :: Monad m
  => Scope m
  -> EntityT m Expression
  -> Transaction ViewTag m (EntityT m Expression)
inferExpression scope (Entity origin prevTypes value) =
  makeEntity =<<
  case value of
  ExpressionLambda lambda -> liftM ((,) [] . ExpressionLambda) $ inferLambda lambda
  ExpressionPi lambda -> liftM ((,) [] . ExpressionPi) $ inferLambda lambda
  ExpressionApply (Apply func arg) -> do
    inferredFunc <- inferExpression scope func
    inferredArg <- inferExpression scope arg
    let
      (applyType, modArg) = case entityType inferredFunc of
        Entity piOrigin _ (ExpressionPi (Lambda paramType resultType)) : _ ->
          ( [subst (entityOriginGuid piOrigin) (entityValue inferredArg) resultType]
          , atEntityType (unify paramType) inferredArg)
        _ -> ([], inferredArg) -- TODO: Split to "bad type" and "missing type"
    return (applyType, ExpressionApply (Apply inferredFunc modArg))
  ExpressionGetVariable varRef -> do
    types <- case varRef of
      ParameterRef guid -> return . maybeToList $ lookup guid scope
      DefinitionRef defI -> do
        dType <- liftM defType $ Transaction.readIRef defI
        inferredDType <-
          liftM convertExpression $ DataLoad.loadExpression dType Nothing
        return [inferredDType]
    return (types, ExpressionGetVariable varRef)
  ExpressionLiteralInteger int ->
    let intType = Entity (OriginGenerated zeroGuid) [] $ ExpressionBuiltin (FFIName ["Prelude"] "Integer")
    in return ([intType], ExpressionLiteralInteger int)
  x -> return ([], x)
  where
    makeEntity (ts, expr) = do
      expandedTs <- mapM expand $ ts ++ prevTypes
      return $ Entity origin expandedTs expr
    inferLambda (Lambda paramType body) = do
      inferredParamType <- inferExpression scope paramType
      liftM (Lambda inferredParamType) $
        inferExpression ((entityOriginGuid origin, inferredParamType) : scope) body

-- This is replaced in all use cases by uniqify:
zeroGuid :: Guid
zeroGuid = Guid.make mempty

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
  Identity.runIdentity . foldValues f
  where
    f (Entity origin ts val) =
      return $
      Entity origin (uniqifyList Map.empty (guidToStdGen (entityOriginGuid origin)) ts) val

foldValues
  :: Monad m
  => (EntityT f Expression -> m (EntityT f Expression))
  -> EntityT f Expression -> m (EntityT f Expression)
foldValues f (Entity origin ts v) =
  f . Entity origin ts =<<
  case v of
  ExpressionLambda lambda -> liftM ExpressionLambda $ onLambda lambda
  ExpressionPi lambda -> liftM ExpressionPi $ onLambda lambda
  ExpressionApply (Apply func arg) ->
    liftM ExpressionApply $ liftM2 Apply (foldValues f func) (foldValues f arg)
  x -> return x
  where
    onLambda (Lambda paramType body) =
      liftM2 Lambda (foldValues f paramType) (foldValues f body)

-- | Execute on types in the Expression tree
mapTypes
  :: Monad m
  => (EntityT f Expression -> m (EntityT f Expression))
  -> EntityT f Expression -> m (EntityT f Expression)
mapTypes f =
  foldValues g
  where
    recurse = f <=< mapTypes f
    g (Entity origin ts v) = do
      newTs <- mapM recurse ts
      liftM (Entity origin newTs) $
        case v of
        ExpressionLambda lambda -> liftM ExpressionLambda $ onLambda lambda
        ExpressionPi lambda -> liftM ExpressionPi $ onLambda lambda
        x -> return x
    onLambda (Lambda paramType body) = do
      newParamType <- recurse paramType
      return $ Lambda newParamType body

uniqifyList
  :: Map Guid Guid -> Random.StdGen
  -> [EntityT m Expression] -> [EntityT m Expression]
uniqifyList symbolMap =
  zipWith (uniqify symbolMap) . map Random.mkStdGen . Random.randoms

canonicalize :: Monad m => EntityT m Expression -> Transaction ViewTag m (EntityT m Expression)
canonicalize expr = do
  globals <- Property.get Anchors.builtinsMap
  let
    f entity@(Entity origin ts (ExpressionBuiltin name)) =
      return .
      maybe entity (Entity origin ts . ExpressionGetVariable) $
      Map.lookup name globals
    f entity = return entity
  foldValues f expr

inferDefinition
  :: Monad m
  => DataLoad.EntityT m Definition
  -> Transaction ViewTag m (EntityT m Definition)
inferDefinition (DataLoad.Entity iref mReplace value) =
  liftM (Entity (OriginStored (Stored iref mReplace)) []) $
  case value of
  Definition typeI bodyI -> do
    inferredType <- liftM uniqifyTypes . mapTypes canonicalize $ convertExpression typeI
    inferredBody <- liftM uniqifyTypes . mapTypes canonicalize =<< inferExpression [] (convertExpression bodyI)
    return $ Definition inferredType inferredBody

loadInferDefinition
  :: Monad m => IRef (Definition IRef)
  -> Transaction ViewTag m (EntityT m Definition)
loadInferDefinition = inferDefinition <=< DataLoad.loadDefinition
