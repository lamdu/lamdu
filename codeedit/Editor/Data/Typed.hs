{-# LANGUAGE
    TemplateHaskell,
    GeneralizedNewtypeDeriving
  #-}
module Editor.Data.Typed
  ( ExpressionEntity(..)
  , ExpressionE
  , atEeInferredType, atEeValue
  , eeReplace, eeGuid, eeIRef
  , InferredTypeEntity(..)
  , InferredType(..)
  , DefinitionEntity(..)
  , atDeIRef, atDeValue
  , deGuid
  , loadInferDefinition
  , loadInferExpression
  , mapMExpressionEntities
  , StoredExpression(..), esGuid -- re-export from Data.Load
  , TypeData, TypedExpressionEntity, TypedDefinitionEntity
  ) where

--import qualified Data.Store.Transaction as Transaction
import Control.Applicative (Applicative)
import Control.Monad (liftM, liftM2, (<=<), when, unless, filterM)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Random (RandomT, nextRandom, runRandomT)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.Monad.Trans.UnionFind (UnionFindT, evalUnionFindT)
import Data.Functor.Identity (Identity(..))
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Monoid (Any(..), mconcat)
import Data.Store.Guid (Guid)
import Data.Store.Transaction (Transaction)
import Editor.Anchors (ViewTag)
import Editor.Data.Load (StoredExpression(..), esGuid)
import qualified Control.Monad.Trans.Reader as Reader
import qualified Control.Monad.Trans.UnionFind as UnionFind
import qualified Data.AtFieldTH as AtFieldTH
import qualified Data.Binary.Utils as BinaryUtils
import qualified Data.Map as Map
import qualified Data.Store.Guid as Guid
import qualified Data.Store.IRef as IRef
import qualified Data.Store.Property as Property
import qualified Editor.Anchors as Anchors
import qualified Editor.Data as Data
import qualified Editor.Data.Load as DataLoad
import qualified System.Random as Random

type T = Transaction ViewTag

type TypedExpressionEntity = ExpressionEntity [InferredType]
type TypedDefinitionEntity = DefinitionEntity [InferredType]

eeGuid :: ExpressionEntity it m -> Guid
eeGuid = esGuid . eeStored

newtype TypeData = TypeData
  { unTypeData :: [InferredTypeEntity TypeRef]
  } deriving (Show)
type TypeRef = UnionFind.Point TypeData

newtype InferredType = InferredType
  { unInferredType :: InferredTypeEntity InferredType
  } deriving (Show, Eq)

data InferredTypeEntity ref = InferredTypeEntity
  { iteGuid :: Guid
  , iteValue :: Data.Expression ref
  } deriving (Show, Eq)

type ExpressionE it m = Data.Expression (ExpressionEntity it m)
data ExpressionEntity it m = ExpressionEntity
  { eeStored :: DataLoad.StoredExpression m
  , eeInferredType :: it
  , eeValue :: ExpressionE it m
  } deriving (Eq)

data DefinitionEntity it m = DefinitionEntity
  { deIRef :: Data.DefinitionIRef
  , deValue :: Data.Definition (ExpressionEntity it m)
  } deriving (Eq)

deGuid :: DefinitionEntity it m -> Guid
deGuid = IRef.guid . deIRef

AtFieldTH.make ''InferredTypeEntity
AtFieldTH.make ''ExpressionEntity
AtFieldTH.make ''DefinitionEntity

eeReplace :: ExpressionEntity it m -> Maybe (Data.ExpressionIRef -> m ())
eeReplace = esReplace . eeStored

eeIRef :: ExpressionEntity it m -> Data.ExpressionIRef
eeIRef = esIRef . eeStored

--------------- Infer Stack boilerplate:

type Scope = [(Guid, TypeRef)]

newtype Infer m a = Infer { unInfer :: ReaderT Scope (UnionFindT TypeData (RandomT Random.StdGen (T m))) a }
  deriving (Functor, Applicative, Monad)

AtFieldTH.make ''Infer

liftScope :: ReaderT Scope (UnionFindT TypeData (RandomT Random.StdGen (T m))) a -> Infer m a
liftScope = Infer

liftTypeRef
  :: Monad m
  => UnionFindT TypeData (RandomT Random.StdGen (T m)) a -> Infer m a
liftTypeRef = liftScope . lift

liftRandom
  :: Monad m
  => RandomT Random.StdGen (T m) a -> Infer m a
liftRandom = liftTypeRef . lift

liftTransaction
  :: Monad m => T m a -> Infer m a
liftTransaction = liftRandom . lift

-- Reader "local" operation cannot simply be lifted...
localScope
  :: Monad m => (Scope -> Scope)
  -> Infer m a -> Infer m a
localScope = atInfer . Reader.local

----------------- Infer operations:

makeTypeRef :: Monad m => [InferredTypeEntity TypeRef] -> Infer m TypeRef
makeTypeRef = liftTypeRef . UnionFind.new . TypeData

getTypeRef :: Monad m => TypeRef -> Infer m [InferredTypeEntity TypeRef]
getTypeRef = liftM unTypeData . liftTypeRef . UnionFind.descr

setTypeRef :: Monad m => TypeRef -> [InferredTypeEntity TypeRef] -> Infer m ()
setTypeRef typeRef [] = do
  g <- nextGuid
  liftTypeRef . UnionFind.setDescr typeRef $
    TypeData [InferredTypeEntity g Data.ExpressionHole]
setTypeRef typeRef types =
  liftTypeRef . UnionFind.setDescr typeRef $
  TypeData types

sanitize
  :: Monad m => TypedExpressionEntity f
  -> T m (TypedExpressionEntity f)
sanitize =
  liftM canonizeIdentifiersTypes .
  (atInferredTypes . const . mapM) builtinsToGlobals

runInfer
  :: Monad m
  => Infer m (TypedExpressionEntity f)
  -> T m (TypedExpressionEntity f)
runInfer =
  sanitize <=<
  runRandomT (Random.mkStdGen 0) .
  evalUnionFindT .
  (`runReaderT` []) .
  unInfer

putInScope
  :: Monad m => [(Guid, TypeRef)]
  -> Infer m a
  -> Infer m a
putInScope = localScope . (++)

readScope :: Monad m => Infer m Scope
readScope = liftScope Reader.ask

nextGuid :: Monad m => Infer m Guid
nextGuid = liftRandom nextRandom

findInScope :: Monad m => Guid -> Infer m (Maybe TypeRef)
findInScope guid = liftM (lookup guid) readScope

generateEntity
  :: Monad m
  => Data.Expression TypeRef
  -> Infer m TypeRef
generateEntity v = do
  g <- nextGuid
  makeTypeRef [InferredTypeEntity g v]

--------------

mapInferredTypes
  :: Monad m
  => (InferredType -> m InferredType)
  -> InferredType -> m InferredType
mapInferredTypes f =
  Data.mapMExpression g
  where
    g (InferredType (InferredTypeEntity guid t)) =
      ( return t
      , f . InferredType . InferredTypeEntity guid
      )

mapMExpressionEntities
  :: Monad m
  => (StoredExpression f
      -> a
      -> ExpressionE b g
      -> m (ExpressionEntity b g))
  -> ExpressionEntity a f
  -> m (ExpressionEntity b g)
mapMExpressionEntities f =
  Data.mapMExpression g
  where
    g (ExpressionEntity stored a val) =
      (return val, f stored a)

atInferredTypes
  :: Monad m
  => (StoredExpression f -> a -> m b)
  -> ExpressionEntity a f
  -> m (ExpressionEntity b f)
atInferredTypes f =
  mapMExpressionEntities g
  where
    g stored a v = do
      b <- f stored a
      return $ ExpressionEntity stored b v

fromEntity
  :: Monad m => (Guid -> Data.Expression ref -> m ref)
  -> ExpressionEntity it f -> m ref
fromEntity mk =
  Data.mapMExpression f
  where
    f (ExpressionEntity stored _ val) =
      ( return val
      , mk $ esGuid stored
      )

inferredTypeFromEntity
  :: ExpressionEntity it f -> InferredType
inferredTypeFromEntity =
  fmap runIdentity $ fromEntity f
  where
    f guid = return . InferredType . InferredTypeEntity guid

ignoreStoredMonad
  :: ExpressionEntity it (T Identity)
  -> ExpressionEntity it (T Identity)
ignoreStoredMonad = id

expand :: Monad m => ExpressionEntity it f -> T m InferredType
expand =
  (`runReaderT` Map.empty) . recurse . inferredTypeFromEntity
  where
    recurse e@(InferredType (InferredTypeEntity guid val)) =
      case val of
      Data.ExpressionGetVariable (Data.DefinitionRef defI) ->
        -- TODO: expand the result recursively (with some recursive
        -- constraint)
        liftM
        (inferredTypeFromEntity .
         ignoreStoredMonad .
         fromLoaded () . Data.defBody . DataLoad.defEntityValue) .
        lift $ DataLoad.loadDefinition defI
      Data.ExpressionGetVariable (Data.ParameterRef guidRef) -> do
        mValueEntity <- Reader.asks (Map.lookup guidRef)
        return $ fromMaybe e mValueEntity
      Data.ExpressionApply
        (Data.Apply
         (InferredType
          (InferredTypeEntity lamGuid
           -- TODO: Don't ignore paramType, we do want to recursively
           -- type-check this:
           (Data.ExpressionLambda (Data.Lambda _paramType body))))
         argEntity) -> do
          newArgEntity <- recurse argEntity
          newBody <-
            Reader.local (Map.insert lamGuid newArgEntity) $
            recurse body
          return newBody
      Data.ExpressionLambda (Data.Lambda paramType body) ->
        recurseLambda Data.ExpressionLambda paramType body
      Data.ExpressionPi (Data.Lambda paramType body) -> do
        newParamType <- recurse paramType
        recurseLambda Data.ExpressionPi newParamType body
      _ -> return e
      where
        recurseLambda cons paramType body = do
          newBody <- recurse body
          return .
            InferredType . InferredTypeEntity guid . cons $
            Data.Lambda paramType newBody

typeRefFromEntity
  :: Monad m => ExpressionEntity it f
  -> Infer m TypeRef
typeRefFromEntity =
  Data.mapMExpression f <=< liftTransaction . expand
  where
    f (InferredType (InferredTypeEntity guid val)) =
      ( return val
      , makeTypeRef . (: []) . InferredTypeEntity guid
      )

fromLoaded
  :: it
  -> DataLoad.ExpressionEntity f
  -> ExpressionEntity it f
fromLoaded it =
  runIdentity . Data.mapMExpression f
  where
    f (DataLoad.ExpressionEntity stored val) =
      (return val, makeEntity stored)
    makeEntity stored newVal =
      return $ ExpressionEntity stored it newVal

addTypeRefs
  :: Monad m
  => ExpressionEntity () f
  -> Infer m (ExpressionEntity TypeRef f)
addTypeRefs =
  mapMExpressionEntities f
  where
    f stored () val = do
      typeRef <- generateEntity Data.ExpressionHole
      return $ ExpressionEntity stored typeRef val

-- TODO: Use ListT with ordinary Data.mapMExpression?
mapTypeRef
  :: Monad m
  => (InferredTypeEntity to -> Infer m to)
  -> TypeRef -> Infer m [to]
mapTypeRef f typeRef = do
  types <- getTypeRef typeRef
  liftM concat $ mapM onType types
  where
    onType (InferredTypeEntity guid expr) =
      mapM (f . InferredTypeEntity guid) =<<
      case expr of
      Data.ExpressionLambda lambda ->
        map1 Data.ExpressionLambda $ onLambda lambda
      Data.ExpressionPi lambda ->
        map1 Data.ExpressionPi $ onLambda lambda
      Data.ExpressionApply apply ->
        map1 Data.ExpressionApply $ onApply apply
      Data.ExpressionGetVariable varRef ->
        map0 $ Data.ExpressionGetVariable varRef
      Data.ExpressionHole ->
        map0 Data.ExpressionHole
      Data.ExpressionLiteralInteger int ->
        map0 $ Data.ExpressionLiteralInteger int
      Data.ExpressionBuiltin name ->
        map0 $ Data.ExpressionBuiltin name
      Data.ExpressionMagic ->
        map0 Data.ExpressionMagic
      where
        recurse = mapTypeRef f
        map0 = return . (: [])
        map1 = liftM . map
        map2 = liftM2 . liftM2
        onApply (Data.Apply funcType argType) =
          map2 Data.Apply
          (recurse funcType)
          (recurse argType)
        onLambda (Data.Lambda paramType resultType) =
          map2 Data.Lambda
          (recurse paramType)
          (recurse resultType)

derefTypeRefs
  :: Monad m
  => ExpressionEntity TypeRef f
  -> Infer m (ExpressionEntity [InferredType] f)
derefTypeRefs =
  mapMExpressionEntities f
  where
    isAHole Data.ExpressionHole = True
    isAHole _ = False
    f stored typeRef val = do
      types <-
        (liftM . filter) (not . isAHole . iteValue . unInferredType) $
        mapTypeRef (return . InferredType) typeRef
      return $ ExpressionEntity stored types val

unifyOnTree
  :: Monad m
  => ExpressionEntity TypeRef (T f)
  -> Infer m ()
unifyOnTree (ExpressionEntity stored typeRef value) = do
  setType =<< generateEntity Data.ExpressionHole
  case value of
    Data.ExpressionLambda lambda ->
      handleLambda lambda
    Data.ExpressionPi lambda@(Data.Lambda _ resultType) -> do
      inferLambda lambda . const . return $ eeInferredType resultType
    Data.ExpressionApply (Data.Apply func arg) -> do
      unifyOnTree func
      unifyOnTree arg
      let
        funcTypeRef = eeInferredType func
        argTypeRef = eeInferredType arg
        piType = Data.ExpressionPi $ Data.Lambda argTypeRef typeRef
      unify funcTypeRef =<< generateEntity piType
      funcTypes <- getTypeRef funcTypeRef
      sequence_
        [ subst piGuid (typeRefFromEntity arg) piResultTypeRef
        | InferredTypeEntity piGuid
          (Data.ExpressionPi
           (Data.Lambda _ piResultTypeRef))
          <- funcTypes
        ]
      return ()
    Data.ExpressionGetVariable (Data.ParameterRef guid) -> do
      mParamTypeRef <- findInScope guid
      case mParamTypeRef of
        -- TODO: Not in scope: Bad code,
        -- add an OutOfScopeReference type error
        Nothing -> return ()
        Just paramTypeRef -> setType paramTypeRef
    Data.ExpressionGetVariable (Data.DefinitionRef defI) -> do
      defTypeEntity <-
        liftM (fromLoaded [] . Data.defType . DataLoad.defEntityValue) .
        liftTransaction $
        DataLoad.loadDefinition defI
      defTypeRef <- typeRefFromEntity $ ignoreStoredMonad defTypeEntity
      setType defTypeRef
    Data.ExpressionLiteralInteger _ ->
      setType <=< generateEntity . Data.ExpressionBuiltin $ Data.FFIName ["Prelude"] "Integer"
    _ -> return ()
  where
    setType = unify typeRef
    handleLambda lambda@(Data.Lambda _ body) =
      inferLambda lambda $ \paramTypeRef ->
        generateEntity . Data.ExpressionPi .
        Data.Lambda paramTypeRef $
        eeInferredType body
    inferLambda (Data.Lambda paramType result) mkType = do
      paramTypeRef <- typeRefFromEntity paramType
      setType =<< mkType paramTypeRef
      putInScope [(esGuid stored, paramTypeRef)] $
        unifyOnTree result

tryRemap :: Ord k => k -> Map k k -> k
tryRemap x = fromMaybe x . Map.lookup x

canonizeIdentifiers
  :: Random.StdGen
  -> InferredType
  -> InferredType
canonizeIdentifiers gen =
  runIdentity . runRandomT gen . (`runReaderT` Map.empty) . f
  where
    onLambda oldGuid newGuid (Data.Lambda paramType body) =
      liftM2 Data.Lambda (f paramType) .
      Reader.local (Map.insert oldGuid newGuid) $ f body
    f (InferredType (InferredTypeEntity oldGuid v)) = do
      newGuid <- lift nextRandom
      liftM (InferredType . InferredTypeEntity newGuid) $
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
          (Data.ExpressionGetVariable . Data.ParameterRef . tryRemap guid)
        x -> return x

unify
  :: Monad m
  => TypeRef
  -> TypeRef
  -> Infer m ()
unify a b = do
  e <- liftTypeRef $ UnionFind.equivalent a b
  unless e $ do
    as <- getTypeRef a
    bs <- getTypeRef b
    us <-
      liftM (TypeData . concat) . sequence $
      liftM2 unifyPair as bs
    liftTypeRef $ do
      UnionFind.union a b
      UnionFind.setDescr a us

unifyPair
  :: Monad m
  => InferredTypeEntity TypeRef
  -> InferredTypeEntity TypeRef
  -> Infer m [InferredTypeEntity TypeRef]
unifyPair
  a@(InferredTypeEntity aGuid aVal)
  b@(InferredTypeEntity bGuid bVal)
  = case (aVal, bVal) of
    (Data.ExpressionHole, _) -> return [b]
    (_, Data.ExpressionHole) -> return [a]
    (Data.ExpressionPi l1,
     Data.ExpressionPi l2) -> do
      unifyLambdas l1 l2
    (Data.ExpressionLambda l1,
     Data.ExpressionLambda l2) -> do
      unifyLambdas l1 l2
    (Data.ExpressionApply (Data.Apply aFuncTypeRef aArgTypeRef),
     Data.ExpressionApply (Data.Apply bFuncTypeRef bArgTypeRef)) -> do
      unify aFuncTypeRef bFuncTypeRef
      unify aArgTypeRef bArgTypeRef
      return [a]
    (Data.ExpressionGetVariable aVarRef,
     Data.ExpressionGetVariable bVarRef)
      | aVarRef == bVarRef -> return [a]
      | otherwise -> return [a, b]
    (Data.ExpressionLiteralInteger aInt,
     Data.ExpressionLiteralInteger bInt)
      | aInt == bInt -> return [a]
      | otherwise -> return [a, b]
    (Data.ExpressionBuiltin aName,
     Data.ExpressionBuiltin bName)
      | aName == bName -> return [a]
      | otherwise -> return [a,b]
    (Data.ExpressionMagic,
     Data.ExpressionMagic) -> return [a]
    (_, _) -> return [a,b]
  where
    unifyLambdas
      (Data.Lambda aParamTypeRef aResultTypeRef)
      (Data.Lambda bParamTypeRef bResultTypeRef) = do
      unify aParamTypeRef bParamTypeRef
      -- Remap b's guid to a's guid and return a as the unification:
      let
        mkGetAGuidRef =
          generateEntity . Data.ExpressionGetVariable . Data.ParameterRef $
          aGuid
      subst bGuid mkGetAGuidRef bResultTypeRef
      unify aResultTypeRef bResultTypeRef
      return [a]

subst :: Monad m => Guid -> Infer m TypeRef -> TypeRef -> Infer m ()
subst from mkTo rootRef = do
  refs <- allUnder [] rootRef
  relevant <- filterM remove refs
  mapM_ ((mkTo >>=) . unify) relevant
  where
    removeFrom
      a@(InferredTypeEntity _
       (Data.ExpressionGetVariable (Data.ParameterRef guidRef)))
      | guidRef == from = (Any True, [])
      | otherwise = (Any False, [a])
    removeFrom x = (Any False, [x])
    remove typeRef = do
      (Any removed, new) <- liftM (mconcat . map removeFrom) $ getTypeRef typeRef
      when removed $
        setTypeRef typeRef new
      return removed

allUnder :: Monad m => [TypeRef] -> TypeRef -> Infer m [TypeRef]
allUnder visited typeRef = do
  alreadySeen <-
    liftTypeRef . liftM or $
    mapM (UnionFind.equivalent typeRef) visited
  if alreadySeen
    then return visited
    else do
      types <- getTypeRef typeRef
      liftM concat $ mapM (recurse (typeRef : visited)) types
  where
    recurse visited1 entity = do
      let
        mPair =
          case iteValue entity of
          Data.ExpressionPi (Data.Lambda p r) -> Just (p, r)
          Data.ExpressionLambda (Data.Lambda p r) -> Just (p, r)
          Data.ExpressionApply (Data.Apply f a) -> Just (f, a)
          _ -> Nothing
      case mPair of
        Nothing -> return visited1
        Just (a, b) -> do
          visited2 <- allUnder visited1 a
          allUnder visited2 b

canonizeIdentifiersTypes
  :: TypedExpressionEntity m
  -> TypedExpressionEntity m
canonizeIdentifiersTypes =
  runIdentity . atInferredTypes canonizeTypes
  where
    canonizeTypes stored =
      return . zipWith canonizeIdentifiers (gens (esGuid stored))
    gens guid =
      map Random.mkStdGen . Random.randoms $
      guidToStdGen guid
    guidToStdGen = Random.mkStdGen . BinaryUtils.decodeS . Guid.bs

builtinsToGlobals
  :: Monad m
  => InferredType -> T m InferredType
builtinsToGlobals expr = do
  globals <- Property.get Anchors.builtinsMap
  let
    f entity@(InferredTypeEntity guid (Data.ExpressionBuiltin name)) =
      return .
      maybe entity
      (InferredTypeEntity guid . Data.ExpressionGetVariable) $
      Map.lookup name globals
    f entity = return entity
  mapInferredTypes (liftM InferredType . f . unInferredType) expr

inferExpression
 :: Monad m
 => Maybe TypeRef
 -> ExpressionEntity () (T f)
 -> Infer m (TypedExpressionEntity (T f))
inferExpression mTypeRef expr = do
  withTypeRefs <- addTypeRefs expr
  case mTypeRef of
    Nothing -> return ()
    Just typeRef ->
      unify typeRef $ eeInferredType withTypeRefs
  unifyOnTree withTypeRefs
  derefTypeRefs withTypeRefs

inferDefinition
  :: Monad m
  => DataLoad.DefinitionEntity (T f)
  -> T m (TypedDefinitionEntity (T f))
inferDefinition (DataLoad.DefinitionEntity iref value) =
  liftM (DefinitionEntity iref) $
  case value of
  Data.Definition typeI bodyI -> do
    inferredType <- sanitize $ fromLoaded [] typeI

    inferredBody <- runInfer $ do
      inferredTypeRef <- typeRefFromEntity inferredType
      inferExpression (Just inferredTypeRef) $ fromLoaded () bodyI
    return $ Data.Definition inferredType inferredBody

loadInferDefinition
  :: Monad m => Data.DefinitionIRef
  -> T m (TypedDefinitionEntity (T m))
loadInferDefinition =
  inferDefinition <=< DataLoad.loadDefinition

loadInferExpression
  :: Monad m => Data.ExpressionIRef
  -> T m (TypedExpressionEntity (T m))
loadInferExpression =
  runInfer . inferExpression Nothing . fromLoaded () <=<
  flip DataLoad.loadExpression Nothing
