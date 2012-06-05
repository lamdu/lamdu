{-# LANGUAGE TemplateHaskell, TypeFamilies, FlexibleContexts, GeneralizedNewtypeDeriving #-}
module Editor.Data.Typed
  ( ExpressionEntity(..)
  , atEeInferredTypes, atEeValue
  , eeReplace, eeGuid, eeIRef
  , DefinitionEntity(..)
  , atDeIRef, atDeValue
  , deGuid
  , Origin(..)
  , loadInferDefinition
  , loadInferExpression
  , writeIRef, writeIRefVia
  , foldValues, mapTypes
  ) where

import Control.Applicative (Applicative, liftA2)
import Control.Monad (liftM, liftM2, (<=<))
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Random (RandomT, nextRandom, runRandomT)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.Monad.Trans.State (StateT)
import Control.Monad.Trans.Writer (WriterT, runWriterT)
import Data.Map (Map)
import Data.Maybe (fromMaybe, maybeToList)
import Data.Store.Guid (Guid)
import Data.Store.Transaction (Transaction)
import Editor.Anchors (ViewTag)
import qualified Control.Monad.Trans.Reader as Reader
import qualified Control.Monad.Trans.State as State
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

type T = Transaction ViewTag

data Origin m
  = OriginGenerated Guid
  | OriginStored (DataLoad.StoredExpression m)
  deriving Eq

type Scope m = [(Guid, ExpressionEntity (T m))]

originGuid :: Origin m -> Guid
originGuid (OriginGenerated guid) = guid
originGuid (OriginStored stored) = DataLoad.esGuid stored

eeGuid :: ExpressionEntity m -> Guid
eeGuid = originGuid . eeOrigin

type ExpressionE m = Data.Expression (ExpressionEntity m)

data ExpressionEntity m = ExpressionEntity
  { eeOrigin :: Origin m
  , eeInferredTypes :: [ExpressionEntity m]
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

originStored
  :: Origin m
  -> Maybe (DataLoad.StoredExpression m)
originStored (OriginGenerated _) = Nothing
originStored (OriginStored x) = Just x

eeStored :: ExpressionEntity m -> Maybe (DataLoad.StoredExpression m)
eeStored = originStored . eeOrigin

eeReplace :: ExpressionEntity m -> Maybe (Data.ExpressionIRef -> m ())
eeReplace = DataLoad.esReplace <=< eeStored

eeIRef :: ExpressionEntity m -> Maybe Data.ExpressionIRef
eeIRef = fmap DataLoad.esIRef . eeStored

writeIRef
  :: Monad m
  => ExpressionEntity (T m)
  -> Maybe (Data.ExpressionI -> Transaction t m ())
writeIRef = fmap Data.writeExprIRef . eeIRef

argument :: (a -> b) -> (b -> c) -> a -> c
argument = flip (.)

writeIRefVia
  :: Monad m
  => (a -> Data.ExpressionI)
  -> ExpressionEntity (T m)
  -> Maybe (a -> Transaction t m ())
writeIRefVia f = (fmap . fmap . argument) f writeIRef

subst
  :: Guid
  -> ExpressionE m
  -> ExpressionEntity m
  -> ExpressionEntity m
subst guid newExpr (ExpressionEntity origin mType value) =
  ExpressionEntity origin mType $
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
  ReaderT (Scope m) (WriterT (Scope m) (RandomT Random.StdGen (T m))) a

newtype Infer m a = Infer { unInfer :: InInfer m a }
  deriving (Functor, Applicative, Monad)

AtFieldTH.make ''Infer

liftScope :: InInfer m a -> Infer m a
liftScope = Infer

liftUsedVarTypes
  :: Monad m
  => WriterT (Scope m) (RandomT Random.StdGen (T m)) a
  -> Infer m a
liftUsedVarTypes = liftScope . lift

liftRandom
  :: Monad m
  => RandomT Random.StdGen (T m) a -> Infer m a
liftRandom = liftUsedVarTypes . lift

liftTransaction
  :: Monad m => T m a -> Infer m a
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
  :: Monad m => Guid -> Infer m a -> Infer m (a, [ExpressionEntity (T m)])
popUsedVar guid act = do
  (result, allUsedVars) <- untellUsedVar guid $ listenUsedVarTypes act
  return (result, lookupAll guid allUsedVars)

runInfer :: Monad m => Infer m a -> T m a
runInfer =
  runRandomT (Random.mkStdGen 0) .
  liftM fst . runWriterT .
  (`runReaderT` []) .
  unInfer

tellUsedVarTypes :: Monad m => Scope m -> Infer m ()
tellUsedVarTypes = liftUsedVarTypes . Writer.tell

putInScope
  :: Monad m => [(Guid, ExpressionEntity (T m))]
  -> Infer m a
  -> Infer m a
putInScope = localScope . (++)

readScope :: Monad m => Infer m (Scope m)
readScope = liftScope Reader.ask

nextGuid :: Monad m => Infer m Guid
nextGuid = liftRandom nextRandom

findInScope :: Monad m => Guid -> Infer m (Maybe (ExpressionEntity (T m)))
findInScope guid = liftM (lookup guid) readScope

generateEntity
  :: Monad m
  => [ExpressionEntity (T m)]
  -> Data.Expression (ExpressionEntity (T m))
  -> Infer m (ExpressionEntity (T m))
generateEntity ts v = do
  g <- nextGuid
  return $ ExpressionEntity (OriginGenerated g) ts v

--------------

expand
  :: Monad m => ExpressionEntity (T m) -> Infer m (ExpressionEntity (T m))
expand =
  foldValues f
  where
    f (ExpressionEntity _ _
       (Data.ExpressionGetVariable (Data.DefinitionRef defI)))
      = do
        def <- liftTransaction $ Transaction.readIRef defI
        liftM convertExpression . liftTransaction $
          DataLoad.loadExpression (Data.defBody def) Nothing
    f (ExpressionEntity _ _
       (Data.ExpressionApply
        (Data.Apply
         (ExpressionEntity lambdaOrigin _
          (Data.ExpressionLambda (Data.Lambda _ body))) val))) =
      return $ subst (originGuid lambdaOrigin) (eeValue val) body
    f x = return x

convertExpression
  :: DataLoad.ExpressionEntity (T m) -> ExpressionEntity (T m)
convertExpression (DataLoad.ExpressionEntity stored value) =
  ExpressionEntity (OriginStored stored) [] $
  case value of
  Data.ExpressionLambda lambda ->
    Data.ExpressionLambda $ convertLambda lambda
  Data.ExpressionPi lambda -> Data.ExpressionPi $ convertLambda lambda
  Data.ExpressionApply (Data.Apply func arg) ->
    Data.ExpressionApply $
    Data.Apply (convertExpression func) (convertExpression arg)
  Data.ExpressionGetVariable varRef -> Data.ExpressionGetVariable varRef
  Data.ExpressionLiteralInteger int -> Data.ExpressionLiteralInteger int
  Data.ExpressionBuiltin bi -> Data.ExpressionBuiltin bi
  Data.ExpressionHole -> Data.ExpressionHole
  Data.ExpressionMagic -> Data.ExpressionMagic
  where
    convertLambda (Data.Lambda paramType body) =
      Data.Lambda (convertExpression paramType) (convertExpression body)

holify
  :: Monad m
  => [ExpressionEntity (T m)] -> Infer m [ExpressionEntity (T m)]
holify [] = liftM (:[]) $ generateEntity [] Data.ExpressionHole
holify xs = return xs

inferExpression
  :: Monad m
  => ExpressionEntity (T m)
  -> Infer m (ExpressionEntity (T m))
inferExpression (ExpressionEntity origin prevTypes value) =
  makeEntity =<<
  case value of
  Data.ExpressionLambda lambda -> do
    let
      lambdaGuid = originGuid origin
      (_, resultTypes) = extractPis prevTypes
      lambdaType paramType resultType =
        generateEntity [] . Data.ExpressionPi $
        Data.Lambda paramType resultType
    (inferredLambda@(Data.Lambda paramType body), usedParamTypes) <-
      popUsedVar lambdaGuid . inferLambda $
      (Data.atLambdaBody . atEeInferredTypes . addTypes) resultTypes lambda
    let allParamTypes = pruneSameTypes $ paramType : usedParamTypes
    pis <- sequence $ liftA2 lambdaType allParamTypes (eeInferredTypes body)
    return (pis, Data.ExpressionLambda inferredLambda)
  Data.ExpressionPi lambda -> do
    inferredLambda@(Data.Lambda _ resultType) <- inferLambda lambda
    return (eeInferredTypes resultType, Data.ExpressionPi inferredLambda)
  Data.ExpressionApply (Data.Apply func arg) -> do
    let
      funcType selfType argType =
        generateEntity [] . Data.ExpressionPi $ Data.Lambda argType selfType
      argTypes = eeInferredTypes arg
    funcTypes <-
      sequence =<<
      (liftM2 . liftA2) funcType (holify prevTypes) (holify argTypes)
    inferredFunc <-
      inferExpression $ (atEeInferredTypes . addTypes) funcTypes func
    let
      substArg
        (ExpressionEntity piOrigin piTs
         (Data.ExpressionPi (Data.Lambda paramType resultType))) =
        ExpressionEntity piOrigin piTs . Data.ExpressionPi .
        Data.Lambda paramType $
        subst (originGuid piOrigin) (eeValue arg) resultType
      substArg x = x
      (paramTypes, resultTypes) =
        extractPis . map substArg $ eeInferredTypes inferredFunc
    inferredArg <-
      inferExpression $ (atEeInferredTypes . addTypes) paramTypes arg
    return
      (map substArg resultTypes,
       Data.ExpressionApply (Data.Apply inferredFunc inferredArg))
  Data.ExpressionGetVariable varRef -> do
    types <- case varRef of
      Data.ParameterRef guid -> do
        tellUsedVarTypes $ map ((,) guid) prevTypes
        liftM maybeToList $ findInScope guid
      Data.DefinitionRef defI -> do
        dType <-
          liftM Data.defType . liftTransaction $ Transaction.readIRef defI
        inferredDType <-
          liftM convertExpression . liftTransaction $
          DataLoad.loadExpression dType Nothing
        return [inferredDType]
    return (types, Data.ExpressionGetVariable varRef)
  Data.ExpressionLiteralInteger int -> do
    intType <-
      generateEntity [] $
      Data.ExpressionBuiltin (Data.FFIName ["Prelude"] "Integer")
    return ([intType], Data.ExpressionLiteralInteger int)
  x ->
    liftM (flip (,) x . (: [])) $
    generateEntity [] Data.ExpressionHole
  where
    extractPis = unzip . concatMap (extractPi . eeValue)
    extractPi (Data.ExpressionPi (Data.Lambda paramType resultType)) =
      [(paramType, resultType)]
    extractPi _ = []
    makeEntity (ts, expr) = do
      expandedTs <- liftM pruneSameTypes . mapM expand $ ts ++ prevTypes
      return $ ExpressionEntity origin expandedTs expr
    inferLambda (Data.Lambda paramType body) = do
      inferredParamType <- inferExpression paramType
      liftM (Data.Lambda inferredParamType) .
        putInScope [(originGuid origin, inferredParamType)] $
        inferExpression body

guidToStdGen :: Guid -> Random.StdGen
guidToStdGen = Random.mkStdGen . BinaryUtils.decodeS . Guid.bs

canonizeIdentifiers
  :: Map Guid Guid
  -> Random.StdGen -> ExpressionEntity (T m) -> ExpressionEntity (T m)
canonizeIdentifiers symbolMap gen (ExpressionEntity oldOrigin ts v) =
  ExpressionEntity (OriginGenerated newGuid)
  (canonizeIdentifiersList symbolMap genT ts) $
  case v of
  Data.ExpressionLambda lambda -> Data.ExpressionLambda $ onLambda lambda
  Data.ExpressionPi lambda -> Data.ExpressionPi $ onLambda lambda
  Data.ExpressionApply (Data.Apply func arg) ->
    Data.ExpressionApply $ Data.Apply (u genV0 func) (u genV1 arg)
  Data.ExpressionGetVariable (Data.ParameterRef guid) ->
    Data.ExpressionGetVariable . Data.ParameterRef . fromMaybe guid $
    Map.lookup guid symbolMap
  x -> x
  where
    oldGuid = originGuid oldOrigin
    u = canonizeIdentifiers symbolMap
    (genV0, genV1) = Random.split genV
    onLambda (Data.Lambda paramType body) =
      Data.Lambda (u genV0 paramType) $
      canonizeIdentifiers
      (Map.insert oldGuid newGuid symbolMap) genV1 body
    (newGuid, newGen) = Random.random gen
    (genT, genV) = Random.split newGen

alphaEq :: ExpressionEntity (T m) -> ExpressionEntity (T m) -> Bool
alphaEq e0 e1 =
  canonizeIdentifiers Map.empty gen e0 == canonizeIdentifiers Map.empty gen e1
  where
    gen = Random.mkStdGen 0

unify
  :: ExpressionEntity m
  -> ExpressionEntity m
  -> StateT (Map Guid (ExpressionEntity m)) Maybe (ExpressionEntity m)
unify (ExpressionEntity origin0 types0 value0) (ExpressionEntity _ _ value1) =
  fmap (ExpressionEntity origin0 types0) $
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
  (Data.ExpressionGetVariable (Data.ParameterRef guid0),
   other1) ->
    inferVariable guid0 other1
  (other0,
   Data.ExpressionGetVariable (Data.ParameterRef guid1)) ->
    inferVariable guid1 other0
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
    inferVariable guid value = do
      mValue <- State.gets $ Map.lookup guid
      case mValue of
        Nothing -> do
          State.modify $ Map.insert guid e
          return value
        Just prevValue -> do
          newValue <- unify e prevValue
          State.modify $ Map.insert guid newValue
          return $ eeValue newValue
      where
        e = ExpressionEntity origin0 types0 value

unification :: [ExpressionEntity m] -> [ExpressionEntity m]
unification = foldr add []
  where
    add x [] = [x]
    add x (y:ys) =
      case (`State.evalStateT` Map.empty) $ unify x y of
        Nothing -> y : add x ys
        Just new -> new : ys

pruneSameTypes
  :: [ExpressionEntity (T m)]
  -> [ExpressionEntity (T m)]
pruneSameTypes = unification . List.nubBy alphaEq

addTypes
  :: [ExpressionEntity (T m)]
  -> [ExpressionEntity (T m)]
  -> [ExpressionEntity (T m)]
addTypes xs ys = pruneSameTypes $ xs ++ ys

foldValues
  :: Monad m
  => (ExpressionEntity f -> m (ExpressionEntity f))
  -> ExpressionEntity f -> m (ExpressionEntity f)
foldValues f (ExpressionEntity origin ts v) =
  f . ExpressionEntity origin ts =<<
  case v of
  Data.ExpressionLambda lambda ->
    liftM Data.ExpressionLambda $ onLambda lambda
  Data.ExpressionPi lambda -> liftM Data.ExpressionPi $ onLambda lambda
  Data.ExpressionApply (Data.Apply func arg) ->
    liftM Data.ExpressionApply $
    liftM2 Data.Apply (foldValues f func) (foldValues f arg)
  x -> return x
  where
    onLambda (Data.Lambda paramType body) =
      liftM2 Data.Lambda (foldValues f paramType) (foldValues f body)

-- | Execute on types in the Data.Expression tree
mapTypes
  :: Monad m
  => (ExpressionEntity f -> m (ExpressionEntity f))
  -> ExpressionEntity f -> m (ExpressionEntity f)
mapTypes f =
  foldValues g
  where
    recurse = f <=< mapTypes f
    g (ExpressionEntity origin ts v) = do
      newTs <- mapM recurse ts
      liftM (ExpressionEntity origin newTs) $
        case v of
        Data.ExpressionLambda lambda ->
          liftM Data.ExpressionLambda $ onLambda lambda
        Data.ExpressionPi lambda ->
          liftM Data.ExpressionPi $ onLambda lambda
        x -> return x
    onLambda (Data.Lambda paramType body) = do
      newParamType <- recurse paramType
      return $ Data.Lambda newParamType body

canonizeIdentifiersList
  :: Map Guid Guid -> Random.StdGen
  -> [ExpressionEntity (T m)] -> [ExpressionEntity (T m)]
canonizeIdentifiersList symbolMap =
  zipWith (canonizeIdentifiers symbolMap) .
  map Random.mkStdGen . Random.randoms

canonizeIdentifiersTypes :: ExpressionEntity (T m) -> ExpressionEntity (T m)
canonizeIdentifiersTypes =
  Identity.runIdentity . foldValues f
  where
    f (ExpressionEntity origin ts val) =
      return $
      ExpressionEntity origin
      (canonizeIdentifiersList Map.empty
       (guidToStdGen (originGuid origin)) ts) val

builtinsToGlobals
  :: Monad m => ExpressionEntity (T m) -> Infer m (ExpressionEntity (T m))
builtinsToGlobals expr = do
  globals <- liftTransaction $ Property.get Anchors.builtinsMap
  let
    f entity@(ExpressionEntity origin ts (Data.ExpressionBuiltin name)) =
      return .
      maybe entity
      (ExpressionEntity origin ts . Data.ExpressionGetVariable) $
      Map.lookup name globals
    f entity = return entity
  foldValues f expr

sanitize
  :: Monad m => ExpressionEntity (T m)
  -> Infer m (ExpressionEntity (T m))
sanitize = liftM canonizeIdentifiersTypes . mapTypes builtinsToGlobals

inferDefinition
  :: Monad m
  => DataLoad.DefinitionEntity (T m)
  -> Infer m (DefinitionEntity (T m))
inferDefinition (DataLoad.DefinitionEntity iref value) =
  liftM (DefinitionEntity iref) $
  case value of
  Data.Definition typeI bodyI -> do
    inferredType <- sanitize $ convertExpression typeI
    inferredBody <-
      sanitize <=< inferExpression .
      (atEeInferredTypes . addTypes) [inferredType] $
      convertExpression bodyI
    return $ Data.Definition inferredType inferredBody

inferRootExpression
  :: Monad m => DataLoad.ExpressionEntity (T m)
  -> Infer m (ExpressionEntity (T m))
inferRootExpression =
  sanitize <=< inferExpression . convertExpression

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
