{-# LANGUAGE TemplateHaskell, TypeFamilies, FlexibleContexts, GeneralizedNewtypeDeriving #-}
module Editor.Data.Typed
  ( ExpressionEntity(..)
  , ExpressionE
  , atEeInferredTypes, atEeValue
  , eeReplace, eeGuid, eeIRef
  , InferredTypeEntity(..)
  , ExpressionIT
  , DefinitionEntity(..)
  , atDeIRef, atDeValue
  , deGuid
  , loadInferDefinition
  , loadInferExpression
  , mapExpressionEntities
  , StoredExpression(..), esGuid -- re-export from Data.Load
  ) where

import Control.Applicative (Applicative, liftA2)
import Control.Monad (liftM, liftM2, (<=<))
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Random (RandomT, nextRandom, runRandomT)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.Monad.Trans.State (StateT)
import Data.Functor.Identity (runIdentity)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Store.Guid (Guid)
import Data.Store.Transaction (Transaction)
import Editor.Anchors (ViewTag)
import Editor.Data.Load (StoredExpression(..), esGuid)
import qualified Control.Monad.Trans.Reader as Reader
import qualified Control.Monad.Trans.State as State
import qualified Data.AtFieldTH as AtFieldTH
import qualified Data.Binary.Utils as BinaryUtils
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Store.Guid as Guid
import qualified Data.Store.IRef as IRef
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Editor.Anchors as Anchors
import qualified Editor.Data as Data
import qualified Editor.Data.Load as DataLoad
import qualified System.Random as Random

type T = Transaction ViewTag

type Scope = [(Guid, InferredTypeEntity)]

eeGuid :: ExpressionEntity m -> Guid
eeGuid = esGuid . eeStored

type ExpressionIT = Data.Expression InferredTypeEntity
data InferredTypeEntity = InferredTypeEntity
  { iteGuid :: Guid
  , iteValue :: ExpressionIT
  } deriving (Eq)

type ExpressionE m = Data.Expression (ExpressionEntity m)
data ExpressionEntity m = ExpressionEntity
  { eeStored :: DataLoad.StoredExpression m
  , eeInferredTypes :: [InferredTypeEntity]
  , eeValue :: ExpressionE m
  } deriving (Eq)

data DefinitionEntity m = DefinitionEntity
  { deIRef :: Data.DefinitionIRef
  , deValue :: Data.Definition (ExpressionEntity m)
  } deriving (Eq)

deGuid :: DefinitionEntity m -> Guid
deGuid = IRef.guid . deIRef

AtFieldTH.make ''ExpressionEntity
AtFieldTH.make ''DefinitionEntity

eeReplace :: ExpressionEntity m -> Maybe (Data.ExpressionIRef -> m ())
eeReplace = esReplace . eeStored

eeIRef :: ExpressionEntity m -> Data.ExpressionIRef
eeIRef = esIRef . eeStored

subst
  :: Guid
  -> ExpressionIT
  -> InferredTypeEntity
  -> InferredTypeEntity
subst guid newExpr (InferredTypeEntity g value) =
  InferredTypeEntity g $
  case value of
  Data.ExpressionLambda lambda -> Data.ExpressionLambda $ onLambda lambda
  Data.ExpressionPi lambda -> Data.ExpressionPi $ onLambda lambda
  Data.ExpressionApply (Data.Apply func arg) ->
    Data.ExpressionApply $ Data.Apply (s func) (s arg)
  var@(Data.ExpressionGetVariable (Data.ParameterRef guidRef))
    | guidRef == guid -> newExpr
    | otherwise -> var
  x -> x
  where
    s = subst guid newExpr
    onLambda (Data.Lambda paramType body) =
      Data.Lambda (s paramType) (s body)

--------------- Infer Stack boilerplate:

type InInfer m a =
  ReaderT Scope (RandomT Random.StdGen (T m)) a

newtype Infer m a = Infer { unInfer :: InInfer m a }
  deriving (Functor, Applicative, Monad)

AtFieldTH.make ''Infer

liftScope :: InInfer m a -> Infer m a
liftScope = Infer

liftRandom
  :: Monad m
  => RandomT Random.StdGen (T m) a -> Infer m a
liftRandom = liftScope . lift

liftTransaction
  :: Monad m => T m a -> Infer m a
liftTransaction = liftRandom . lift

-- Reader "local" operation cannot simply be lifted...
localScope
  :: Monad m => (Scope -> Scope)
  -> Infer m a -> Infer m a
localScope = atInfer . Reader.local

----------------- Infer operations:

runInfer :: Monad m => Infer m a -> T m a
runInfer =
  runRandomT (Random.mkStdGen 0) .
  (`runReaderT` []) .
  unInfer

putInScope
  :: Monad m => [(Guid, InferredTypeEntity)]
  -> Infer m a
  -> Infer m a
putInScope = localScope . (++)

_readScope :: Monad m => Infer m Scope
_readScope = liftScope Reader.ask

_nextGuid :: Monad m => Infer m Guid
_nextGuid = liftRandom nextRandom

_findInScope :: Monad m => Guid -> Infer m (Maybe InferredTypeEntity)
_findInScope guid = liftM (lookup guid) _readScope

_generateEntity
  :: Monad m
  => ExpressionIT
  -> Infer m InferredTypeEntity
_generateEntity v = do
  g <- _nextGuid
  return $ InferredTypeEntity g v

--------------

mapInferredTypeEntities
  :: Monad m
  => (InferredTypeEntity -> m InferredTypeEntity)
  -> InferredTypeEntity -> m InferredTypeEntity
mapInferredTypeEntities f =
  Data.mapMExpression g
  where
    g (InferredTypeEntity guid t) =
      (return t, f . InferredTypeEntity guid)

mapExpressionEntities
  :: Monad m
  => (ExpressionEntity f -> m (ExpressionEntity f))
  -> ExpressionEntity f -> m (ExpressionEntity f)
mapExpressionEntities f =
  Data.mapMExpression g
  where
    g (ExpressionEntity stored ts val) =
      (return val, f . ExpressionEntity stored ts)

fromEntity :: ExpressionEntity m -> InferredTypeEntity
fromEntity =
  runIdentity . Data.mapMExpression f
  where
    f (ExpressionEntity stored _ val) =
      (return val, return . InferredTypeEntity (esGuid stored))

fromLoaded :: DataLoad.ExpressionEntity m -> ExpressionEntity m
fromLoaded =
  runIdentity . Data.mapMExpression f
  where
    f (DataLoad.ExpressionEntity stored val) =
      (return val, return . ExpressionEntity stored [])

expand :: Monad m => InferredTypeEntity -> Infer m InferredTypeEntity
expand =
  mapInferredTypeEntities f
  where
    f (InferredTypeEntity _
       (Data.ExpressionGetVariable
        (Data.DefinitionRef defI))) =
      do
        def <- liftTransaction $ Transaction.readIRef defI
        liftM (fromEntity . fromLoaded) . liftTransaction $
          DataLoad.loadExpression (Data.defBody def) Nothing
    f (InferredTypeEntity _
       (Data.ExpressionApply
        (Data.Apply
         (InferredTypeEntity lambdaGuid
          (Data.ExpressionLambda
           (Data.Lambda _ body))) val))) =
      return $ subst lambdaGuid (iteValue val) body
    f x = return x

inferExpression
  :: Monad m
  => ExpressionEntity (T m)
  -> Infer m (ExpressionEntity (T m))
inferExpression (ExpressionEntity stored prevTypes value) =
  makeEntity =<<
  case value of
  Data.ExpressionLambda lambda ->
    liftM ((,) [] . Data.ExpressionLambda) $ inferLambda lambda
  Data.ExpressionPi lambda -> do
    liftM ((,) [] . Data.ExpressionPi) $ inferLambda lambda
  Data.ExpressionApply (Data.Apply func arg) ->
    liftM ((,) [] . Data.ExpressionApply) $
    liftM2 Data.Apply (inferExpression func) (inferExpression arg)
  x -> return ([], x)
  where
    makeEntity (ts, expr) = do
      expandedTs <- liftM pruneSameTypes . mapM expand $ ts ++ prevTypes
      return $ ExpressionEntity stored expandedTs expr
    inferLambda (Data.Lambda paramType body) = do
      inferredParamType <- inferExpression paramType
      liftM (Data.Lambda inferredParamType) .
        putInScope [(esGuid stored, fromEntity inferredParamType)] $
        inferExpression body

canonizeIdentifiers
  :: Random.StdGen
  -> InferredTypeEntity
  -> InferredTypeEntity
canonizeIdentifiers gen =
  runIdentity . runRandomT gen . (`runReaderT` Map.empty) . f
  where
    onLambda oldGuid newGuid (Data.Lambda paramType body) =
      liftM2 Data.Lambda (f paramType) .
      Reader.local (Map.insert oldGuid newGuid) $ f body
    f (InferredTypeEntity oldGuid v) = do
      newGuid <- lift nextRandom
      liftM (InferredTypeEntity newGuid) $
        case v of
        Data.ExpressionLambda lambda ->
          liftM Data.ExpressionLambda $ onLambda oldGuid newGuid lambda
        Data.ExpressionPi lambda ->
          liftM Data.ExpressionPi $ onLambda oldGuid newGuid lambda
        Data.ExpressionApply (Data.Apply func arg) ->
          liftM Data.ExpressionApply $
          liftM2 Data.Apply (f func) (f arg)
        Data.ExpressionGetVariable (Data.ParameterRef guid) ->
          Reader.asks
          (Data.ExpressionGetVariable . Data.ParameterRef . fromMaybe guid .
           Map.lookup guid)
        x -> return x

alphaEq :: InferredTypeEntity -> InferredTypeEntity -> Bool
alphaEq e0 e1 =
  canonizeIdentifiers gen e0 == canonizeIdentifiers gen e1
  where
    gen = Random.mkStdGen 0

unify
  :: InferredTypeEntity
  -> InferredTypeEntity
  -> StateT (Map Guid InferredTypeEntity) Maybe InferredTypeEntity
unify
  (InferredTypeEntity guid0 value0)
  (InferredTypeEntity _ value1) =
  fmap (InferredTypeEntity guid0) $
  case (value0, value1) of
  (Data.ExpressionHole, _) -> return value1
  (_, Data.ExpressionHole) -> return value0
  (Data.ExpressionLambda lambda0,
   Data.ExpressionLambda lambda1) ->
    fmap Data.ExpressionLambda $ unifyLambda lambda0 lambda1
  (Data.ExpressionPi lambda0,
   Data.ExpressionPi lambda1) ->
    fmap Data.ExpressionPi $ unifyLambda lambda0 lambda1
  (Data.ExpressionApply (Data.Apply func0 arg0),
   Data.ExpressionApply (Data.Apply func1 arg1)) ->
    fmap Data.ExpressionApply $
    liftA2 Data.Apply (unify func0 func1) (unify arg0 arg1)
  (Data.ExpressionGetVariable (Data.ParameterRef guidRef0),
   other1) ->
    inferVariable guidRef0 other1
  (other0,
   Data.ExpressionGetVariable (Data.ParameterRef guidRef1)) ->
    inferVariable guidRef1 other0
  -- Only LiteralInteger, Builtin, Magic here. If any constructors are
  -- added, need to match them here
  (x, y)
    | x == y -> return x
    | otherwise -> lift Nothing
  where
    unifyLambda
      (Data.Lambda paramType0 result0)
      (Data.Lambda paramType1 result1) =
        liftA2 Data.Lambda
        (unify paramType0 paramType1)
        (unify result0 result1)
    inferVariable guidRef value = do
      mValue <- State.gets $ Map.lookup guidRef
      case mValue of
        Nothing -> do
          State.modify $ Map.insert guidRef e
          return value
        Just prevValue -> do
          newValue <- unify e prevValue
          State.modify $ Map.insert guidRef newValue
          return $ iteValue newValue
      where
        e = InferredTypeEntity guid0 value

unification :: [InferredTypeEntity] -> [InferredTypeEntity]
unification = foldr add []
  where
    add x [] = [x]
    add x (y:ys) =
      case (`State.evalStateT` Map.empty) $ unify x y of
        Nothing -> y : add x ys
        Just new -> new : ys

pruneSameTypes
  :: [InferredTypeEntity]
  -> [InferredTypeEntity]
pruneSameTypes = unification . List.nubBy alphaEq

_addTypes
  :: [InferredTypeEntity]
  -> [InferredTypeEntity]
  -> [InferredTypeEntity]
_addTypes xs ys = pruneSameTypes $ xs ++ ys

-- | Execute on types in the Data.Expression tree
onAllInferredTypes
  :: Monad m
  => ([InferredTypeEntity] -> m [InferredTypeEntity])
  -> ExpressionEntity f -> m (ExpressionEntity f)
onAllInferredTypes f =
  mapExpressionEntities g
  where
    g (ExpressionEntity stored ts v) = do
      newTs <- f ts
      return $ ExpressionEntity stored newTs v

canonizeIdentifiersTypes :: ExpressionEntity m -> ExpressionEntity m
canonizeIdentifiersTypes =
  runIdentity . mapExpressionEntities f
  where
    f (ExpressionEntity stored ts val) =
      return $
      ExpressionEntity stored (zipWith canonizeIdentifiers gens ts) val
      where
        guid = esGuid stored
        gens =
          map Random.mkStdGen . Random.randoms $
          guidToStdGen guid
    guidToStdGen = Random.mkStdGen . BinaryUtils.decodeS . Guid.bs

builtinsToGlobals
  :: Monad m
  => InferredTypeEntity -> Infer m InferredTypeEntity
builtinsToGlobals expr = do
  globals <- liftTransaction $ Property.get Anchors.builtinsMap
  let
    f entity@(InferredTypeEntity guid (Data.ExpressionBuiltin name)) =
      return .
      maybe entity
      (InferredTypeEntity guid . Data.ExpressionGetVariable) $
      Map.lookup name globals
    f entity = return entity
  mapInferredTypeEntities f expr

sanitize
  :: Monad m => ExpressionEntity f
  -> Infer m (ExpressionEntity f)
sanitize = liftM canonizeIdentifiersTypes . onAllInferredTypes (mapM builtinsToGlobals)

inferDefinition
  :: Monad m
  => DataLoad.DefinitionEntity (T m)
  -> Infer m (DefinitionEntity (T m))
inferDefinition (DataLoad.DefinitionEntity iref value) =
  liftM (DefinitionEntity iref) $
  case value of
  Data.Definition typeI bodyI -> do
    inferredType <- sanitize $ fromLoaded typeI
    inferredBody <-
      sanitize <=< inferExpression $
      fromLoaded bodyI
    return $ Data.Definition inferredType inferredBody

inferRootExpression
  :: Monad m => DataLoad.ExpressionEntity (T m)
  -> Infer m (ExpressionEntity (T m))
inferRootExpression =
  sanitize <=< inferExpression . fromLoaded

loadInferDefinition
  :: Monad m => Data.DefinitionIRef
  -> T m (DefinitionEntity (T m))
loadInferDefinition =
  runInfer . inferDefinition <=< DataLoad.loadDefinition

loadInferExpression
  :: Monad m => Data.ExpressionIRef
  -> T m (ExpressionEntity (T m))
loadInferExpression =
  runInfer . inferRootExpression <=< flip DataLoad.loadExpression Nothing
