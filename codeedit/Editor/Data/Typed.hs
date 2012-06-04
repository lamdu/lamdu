{-# LANGUAGE TemplateHaskell, TypeFamilies, FlexibleContexts, GeneralizedNewtypeDeriving #-}
module Editor.Data.Typed
  ( Entity(..), atEntityType, atEntityValue
  , EntityM, EntityT
  , Stored(..), EntityOrigin(..)
  , entityReplace
  , loadInferDefinition
  , loadInferExpression
  , entityGuid, entityIRef
  , writeIRef, writeIRefVia
  , foldValues, mapTypes
  ) where

import Control.Applicative (Applicative, liftA2)
import Control.Monad (liftM, liftM2, (<=<))
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Random (RandomT, nextRandom, runRandomT)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.Monad.Trans.Writer (WriterT, runWriterT)
import Data.Binary (Binary)
import Data.Map (Map)
import Data.Maybe (fromMaybe, maybeToList)
import Data.Store.Guid (Guid)
import Data.Store.IRef (IRef)
import Data.Store.Transaction (Transaction)
import Editor.Anchors (ViewTag)
import Editor.Data (Definition(..), Expression(..), FFIName(..), Apply(..), Lambda(..), VariableRef(..))
import qualified Control.Monad.Trans.Reader as Reader
import qualified Control.Monad.Trans.Writer as Writer
import qualified Data.AtFieldTH as AtFieldTH
import qualified Data.Binary.Utils as BinaryUtils
import qualified Data.Functor.Identity as Identity
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

type EntityM m f = Entity m (f (Entity m))
type EntityT m f = EntityM (Transaction ViewTag m) f

data Entity m a = Entity
  { entityOrigin :: EntityOrigin m a
  , entityType :: [EntityM m Expression] -- Inferred types
  , entityValue :: a
  } deriving Eq

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

--------------- Infer Stack boilerplate:

type InInfer m a =
  ReaderT (Scope m) (WriterT (Scope m) (RandomT Random.StdGen (Transaction ViewTag m))) a

newtype Infer m a = Infer { unInfer :: InInfer m a }
  deriving (Functor, Applicative, Monad)

AtFieldTH.make ''Infer

liftScope :: InInfer m a -> Infer m a
liftScope = Infer

liftUsedVarTypes
  :: Monad m
  => WriterT (Scope m) (RandomT Random.StdGen (Transaction ViewTag m)) a
  -> Infer m a
liftUsedVarTypes = liftScope . lift

liftRandom
  :: Monad m
  => RandomT Random.StdGen (Transaction ViewTag m) a -> Infer m a
liftRandom = liftUsedVarTypes . lift

liftTransaction
  :: Monad m => Transaction ViewTag m a -> Infer m a
liftTransaction = liftRandom . lift

-- Writer "listen" operation cannot simply be lifted...
listenUsedVarTypes :: Monad m => Infer m a -> Infer m (a, Scope m)
listenUsedVarTypes = atInfer $ Reader.mapReaderT Writer.listen

censorUsedVarTypes
  :: Monad m => (Scope m -> Scope m) -> Infer m a -> Infer m a
censorUsedVarTypes = atInfer . Reader.mapReaderT . Writer.censor

-- Reader "local" operation cannot simply be lifted...
localScope
  :: Monad m => (Scope m -> Scope m)
  -> Infer m a -> Infer m a
localScope = atInfer . Reader.local

----------------- Infer operations:

untellUsedVar :: Monad m => Guid -> Infer m a -> Infer m a
untellUsedVar guid = censorUsedVarTypes $ filter ((/= guid) . fst)

lookupAll :: Eq k => k -> [(k, v)] -> [v]
lookupAll x = map snd . filter ((== x) . fst)

-- Get the usedVar from the given computation and remove it from the
-- result computation
popUsedVar
  :: Monad m => Guid -> Infer m a -> Infer m (a, [EntityT m Expression])
popUsedVar guid act = do
  (result, allUsedVars) <- untellUsedVar guid $ listenUsedVarTypes act
  return (result, lookupAll guid allUsedVars)

runInfer :: Monad m => Infer m a -> Transaction ViewTag m a
runInfer =
  runRandomT (Random.mkStdGen 0) .
  liftM fst . runWriterT .
  (`runReaderT` []) .
  unInfer

tellUsedVarTypes :: Monad m => Scope m -> Infer m ()
tellUsedVarTypes = liftUsedVarTypes . Writer.tell

putInScope
  :: Monad m => [(Guid, EntityT m Expression)]
  -> Infer m a
  -> Infer m a
putInScope = localScope . (++)

readScope :: Monad m => Infer m (Scope m)
readScope = liftScope Reader.ask

nextGuid :: Monad m => Infer m Guid
nextGuid = liftRandom nextRandom

--------------

findInScope :: Monad m => Guid -> Infer m (Maybe (EntityT m Expression))
findInScope guid = liftM (lookup guid) readScope

generateEntity
  :: Monad m
  => [EntityT m Expression]
  -> f (TypedEntity m)
  -> Infer m (EntityT m f)
generateEntity ts v = do
  g <- nextGuid
  return $ Entity (OriginGenerated g) ts v

expand :: Monad m => EntityT m Expression -> Infer m (EntityT m Expression)
expand =
  foldValues f
  where
    f (Entity _ _ (ExpressionGetVariable (DefinitionRef defI))) = do
      def <- liftTransaction $ Transaction.readIRef defI
      liftM convertExpression . liftTransaction $
        DataLoad.loadExpression (defBody def) Nothing
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

holify :: Monad m => [EntityT m Expression] -> Infer m [EntityT m Expression]
holify [] = liftM (:[]) $ generateEntity [] ExpressionHole
holify xs = return xs

inferExpression
  :: Monad m
  => EntityT m Expression
  -> Infer m (EntityT m Expression)
inferExpression (Entity origin prevTypes value) =
  makeEntity =<<
  case value of
  ExpressionLambda lambda -> do
    let
      lambdaGuid = entityOriginGuid origin
      (_, resultTypes) =
        unzip $ concatMap (extractPi . entityValue) prevTypes
      lambdaType paramType resultType =
        generateEntity [] . ExpressionPi $ Lambda paramType resultType
    (inferredLambda@(Lambda paramType body), usedParamTypes) <-
      popUsedVar lambdaGuid . inferLambda $
      (Data.atLambdaBody . atEntityType . unify) resultTypes lambda
    let allParamTypes = pruneSameTypes $ paramType : usedParamTypes
    pis <- sequence $ liftA2 lambdaType allParamTypes (entityType body)
    return (pis, ExpressionLambda inferredLambda)
  ExpressionPi lambda -> do
    inferredLambda@(Lambda _ resultType) <- inferLambda lambda
    return (entityType resultType, ExpressionPi inferredLambda)
  ExpressionApply (Apply func arg) -> do
    let
      funcType selfType argType =
        generateEntity [] . ExpressionPi $ Lambda argType selfType
      argTypes = entityType arg
    funcTypes <-
      sequence =<<
      (liftM2 . liftA2) funcType (holify prevTypes) (holify argTypes)
    inferredFunc <- inferExpression ((atEntityType . unify) funcTypes func)
    (applyType, modArg) <-
      case entityType inferredFunc of
      Entity piOrigin _
        (ExpressionPi (Lambda paramType resultType)) : _ -> do
          inferredArg <-
            inferExpression ((atEntityType . unify) [paramType] arg)
          return
            ( [subst (entityOriginGuid piOrigin)
               (entityValue inferredArg) resultType]
            , inferredArg )
      _ -> do
        inferredArg <- inferExpression arg
        -- TODO: Split to "bad type" and "missing type":
        return ([], inferredArg)
    return (applyType, ExpressionApply (Apply inferredFunc modArg))
  ExpressionGetVariable varRef -> do
    types <- case varRef of
      ParameterRef guid -> do
        tellUsedVarTypes $ map ((,) guid) prevTypes
        liftM maybeToList $ findInScope guid
      DefinitionRef defI -> do
        dType <-
          liftM defType . liftTransaction $ Transaction.readIRef defI
        inferredDType <-
          liftM convertExpression . liftTransaction $
          DataLoad.loadExpression dType Nothing
        return [inferredDType]
    return (types, ExpressionGetVariable varRef)
  ExpressionLiteralInteger int -> do
    intType <-
      generateEntity [] $
      ExpressionBuiltin (FFIName ["Prelude"] "Integer")
    return ([intType], ExpressionLiteralInteger int)
  ExpressionHole ->
    liftM (flip (,) ExpressionHole . (: [])) $
    generateEntity [] ExpressionHole
  x -> return ([], x)
  where
    extractPi (ExpressionPi (Lambda paramType resultType)) = [(paramType, resultType)]
    extractPi _ = []
    makeEntity (ts, expr) = do
      expandedTs <- liftM pruneSameTypes . mapM expand $ ts ++ prevTypes
      return $ Entity origin expandedTs expr
    inferLambda (Lambda paramType body) = do
      inferredParamType <- inferExpression paramType
      liftM (Lambda inferredParamType) .
        putInScope [(entityOriginGuid origin, inferredParamType)] $
        inferExpression body

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

alphaEq :: EntityT m Expression -> EntityT m Expression -> Bool
alphaEq e0 e1 =
  uniqify Map.empty gen e0 == uniqify Map.empty gen e1
  where
    gen = Random.mkStdGen 0

pruneSameTypes
  :: [EntityT m Expression]
  -> [EntityT m Expression]
pruneSameTypes = List.nubBy alphaEq

unify
  :: [EntityT m Expression]
  -> [EntityT m Expression]
  -> [EntityT m Expression]
unify xs ys = pruneSameTypes $ xs ++ ys

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

uniqifyTypes :: EntityT m Expression -> EntityT m Expression
uniqifyTypes =
  Identity.runIdentity . foldValues f
  where
    f (Entity origin ts val) =
      return $
      Entity origin (uniqifyList Map.empty (guidToStdGen (entityOriginGuid origin)) ts) val

canonicalize :: Monad m => EntityT m Expression -> Infer m (EntityT m Expression)
canonicalize expr = do
  globals <- liftTransaction $ Property.get Anchors.builtinsMap
  let
    f entity@(Entity origin ts (ExpressionBuiltin name)) =
      return .
      maybe entity (Entity origin ts . ExpressionGetVariable) $
      Map.lookup name globals
    f entity = return entity
  foldValues f expr

sanitize
  :: Monad m => EntityT m Expression
  -> Infer m (EntityT m Expression)
sanitize = liftM uniqifyTypes . mapTypes canonicalize

inferDefinition
  :: Monad m
  => DataLoad.EntityT m Definition
  -> Infer m (EntityT m Definition)
inferDefinition (DataLoad.Entity iref mReplace value) =
  liftM (Entity (OriginStored (Stored iref mReplace)) []) $
  case value of
  Definition typeI bodyI -> do
    inferredType <- sanitize $ convertExpression typeI
    inferredBody <-
      sanitize <=< inferExpression .
      (atEntityType . unify) [inferredType] $
      convertExpression bodyI
    return $ Definition inferredType inferredBody

inferRootExpression
  :: Monad m => DataLoad.EntityT m Expression
  -> Infer m (EntityT m Expression)
inferRootExpression =
  sanitize <=< inferExpression . convertExpression

loadInferDefinition
  :: Monad m => IRef (Definition IRef)
  -> Transaction ViewTag m (EntityT m Definition)
loadInferDefinition =
  runInfer . inferDefinition <=< DataLoad.loadDefinition

loadInferExpression
  :: Monad m => IRef (Expression IRef)
  -> Transaction ViewTag m (EntityT m Expression)
loadInferExpression =
  runInfer . inferRootExpression <=< flip DataLoad.loadExpression Nothing
