{-# LANGUAGE
    TemplateHaskell,
    GeneralizedNewtypeDeriving
  #-}
module Editor.Data.Typed
  ( StoredExpressionRef(..)
  , atEeInferredType, atEeValue
  , eeReplace, eeGuid, eeIRef
  , GuidExpression(..)
  , InferredTypeLoop(..)
  , InferredType(..)
  , StoredDefinition(..)
  , atDeIRef, atDeValue
  , deGuid
  , loadInferDefinition
  , loadInferExpression
  , mapMExpressionEntities
  , StoredExpression(..), esGuid -- re-export from Data.Load
  , TypeData, TypedStoredExpression, TypedStoredDefinition
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
import Editor.Data.Load (StoredExpressionRef(..), esGuid)
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

type TypedStoredExpression = StoredExpression [InferredTypeLoop]
type TypedStoredDefinition = StoredDefinition [InferredTypeLoop]

eeGuid :: StoredExpression it m -> Guid
eeGuid = esGuid . eeStored

newtype TypeData = TypeData
  { unTypeData :: [GuidExpression TypeRef]
  } deriving (Show)
type TypeRef = UnionFind.Point TypeData

newtype InferredType = InferredType
  { unInferredType :: GuidExpression InferredType
  } deriving (Show, Eq)

data GuidExpression ref = GuidExpression
  { geGuid :: Guid
  , geValue :: Data.Expression ref
  } deriving (Show, Eq)

data StoredExpression it m = StoredExpression
  { eeStored :: DataLoad.StoredExpressionRef m
  , eeInferredType :: it
  , eeValue :: Data.Expression (StoredExpression it m)
  } deriving (Eq)

data InferredTypeLoop
  = InferredTypeNoLoop (GuidExpression InferredTypeLoop)
  | InferredTypeLoop Guid
  deriving (Show, Eq)

data StoredDefinition it m = StoredDefinition
  { deIRef :: Data.DefinitionIRef
  , deValue :: Data.Definition (StoredExpression it m)
  } deriving (Eq)

deGuid :: StoredDefinition it m -> Guid
deGuid = IRef.guid . deIRef

AtFieldTH.make ''GuidExpression
AtFieldTH.make ''StoredExpression
AtFieldTH.make ''StoredDefinition

eeReplace :: StoredExpression it m -> Maybe (Data.ExpressionIRef -> m ())
eeReplace = esReplace . eeStored

eeIRef :: StoredExpression it m -> Data.ExpressionIRef
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

makeTypeRef :: Monad m => [GuidExpression TypeRef] -> Infer m TypeRef
makeTypeRef = liftTypeRef . UnionFind.new . TypeData

getTypeRef :: Monad m => TypeRef -> Infer m [GuidExpression TypeRef]
getTypeRef = liftM unTypeData . liftTypeRef . UnionFind.descr

setTypeRef :: Monad m => TypeRef -> [GuidExpression TypeRef] -> Infer m ()
setTypeRef typeRef [] = do
  g <- nextGuid
  liftTypeRef . UnionFind.setDescr typeRef $
    TypeData [GuidExpression g Data.ExpressionHole]
setTypeRef typeRef types =
  liftTypeRef . UnionFind.setDescr typeRef $
  TypeData types

runInfer
  :: Monad m
  => Infer m (TypedStoredExpression f)
  -> T m (TypedStoredExpression f)
runInfer =
  liftM canonizeIdentifiersTypes .
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
  makeTypeRef [GuidExpression g v]

--------------

mapInferredTypes
  :: Monad m
  => (InferredType -> m InferredType)
  -> InferredType -> m InferredType
mapInferredTypes f =
  Data.mapMExpression g
  where
    g (InferredType (GuidExpression guid t)) =
      ( return t
      , f . InferredType . GuidExpression guid
      )

mapMExpressionEntities
  :: Monad m
  => (StoredExpressionRef f
      -> a
      -> Data.Expression (StoredExpression b g)
      -> m (StoredExpression b g))
  -> StoredExpression a f
  -> m (StoredExpression b g)
mapMExpressionEntities f =
  Data.mapMExpression g
  where
    g (StoredExpression stored a val) =
      (return val, f stored a)

atInferredTypes
  :: Monad m
  => (StoredExpressionRef f -> a -> m b)
  -> StoredExpression a f
  -> m (StoredExpression b f)
atInferredTypes f =
  mapMExpressionEntities g
  where
    g stored a v = do
      b <- f stored a
      return $ StoredExpression stored b v

fromEntity
  :: Monad m => (Guid -> Data.Expression ref -> m ref)
  -> StoredExpression it f -> m ref
fromEntity mk =
  Data.mapMExpression f
  where
    f (StoredExpression stored _ val) =
      ( return val
      , mk $ esGuid stored
      )

inferredTypeFromEntity
  :: StoredExpression it f -> InferredType
inferredTypeFromEntity =
  fmap runIdentity $ fromEntity f
  where
    f guid = return . InferredType . GuidExpression guid

ignoreStoredMonad
  :: StoredExpression it (T Identity)
  -> StoredExpression it (T Identity)
ignoreStoredMonad = id

expand :: Monad m => StoredExpression it f -> T m InferredType
expand =
  (`runReaderT` Map.empty) . recurse . inferredTypeFromEntity
  where
    recurse e@(InferredType (GuidExpression guid val)) =
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
          (GuidExpression lamGuid
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
            InferredType . GuidExpression guid . cons $
            Data.Lambda paramType newBody

typeRefFromEntity
  :: Monad m => StoredExpression it f
  -> Infer m TypeRef
typeRefFromEntity entity =
  Data.mapMExpression f <=< liftTransaction $ builtinsToGlobals =<< expand entity
  where
    f (InferredType (GuidExpression guid val)) =
      ( return val
      , makeTypeRef . (: []) . GuidExpression guid
      )

fromLoaded
  :: it
  -> DataLoad.ExpressionEntity f
  -> StoredExpression it f
fromLoaded it =
  runIdentity . Data.mapMExpression f
  where
    f (DataLoad.ExpressionEntity stored val) =
      (return val, makeEntity stored)
    makeEntity stored newVal =
      return $ StoredExpression stored it newVal

addTypeRefs
  :: Monad m
  => StoredExpression () f
  -> Infer m (StoredExpression TypeRef f)
addTypeRefs =
  mapMExpressionEntities f
  where
    f stored () val = do
      typeRef <- generateEntity Data.ExpressionHole
      return $ StoredExpression stored typeRef val

-- TODO: Use ListT with ordinary Data.mapMExpression?
derefTypeRef
  :: Monad m
  => [TypeRef] -> TypeRef -> Infer m [InferredTypeLoop]
derefTypeRef visited typeRef = do
  isLoop <- liftTypeRef . liftM or $
    mapM (UnionFind.equivalent typeRef) visited
  if isLoop
    then liftM ((:[]) . InferredTypeLoop) nextGuid
    else liftM concat $ mapM onType =<< getTypeRef typeRef
  where
    onType (GuidExpression guid expr) =
      (liftM . map) (InferredTypeNoLoop . GuidExpression guid) $
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
        recurse = derefTypeRef (typeRef : visited)
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
  => StoredExpression TypeRef f
  -> Infer m (StoredExpression [InferredTypeLoop] f)
derefTypeRefs =
  mapMExpressionEntities f
  where
    inferredTypeIsHole (InferredTypeNoLoop (GuidExpression _ Data.ExpressionHole)) = True
    inferredTypeIsHole _ = False
    f stored typeRef val = do
      types <-
        (liftM . filter) (not . inferredTypeIsHole) $
        derefTypeRef [] typeRef
      return $ StoredExpression stored types val

unifyOnTree
  :: Monad m
  => StoredExpression TypeRef (T f)
  -> Infer m ()
unifyOnTree (StoredExpression stored typeRef value) = do
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
        | GuidExpression piGuid
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
  => GuidExpression TypeRef
  -> GuidExpression TypeRef
  -> Infer m [GuidExpression TypeRef]
unifyPair
  a@(GuidExpression aGuid aVal)
  b@(GuidExpression bGuid bVal)
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
      a@(GuidExpression _
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
          case geValue entity of
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
  :: TypedStoredExpression m
  -> TypedStoredExpression m
canonizeIdentifiersTypes =
  runIdentity . atInferredTypes canonizeTypes
  where
    canonizeTypes stored =
      return . zipWith canonizeIdentifiers (gens (esGuid stored))
    gens guid =
      map Random.mkStdGen . Random.randoms $
      guidToStdGen guid
    guidToStdGen = Random.mkStdGen . BinaryUtils.decodeS . Guid.bs
    canonizeIdentifiers gen =
      runIdentity . runRandomT gen . (`runReaderT` Map.empty) . f
      where
        onLambda oldGuid newGuid (Data.Lambda paramType body) =
          liftM2 Data.Lambda (f paramType) .
          Reader.local (Map.insert oldGuid newGuid) $ f body
        f (InferredTypeLoop guid) = return $ InferredTypeLoop guid
        f (InferredTypeNoLoop (GuidExpression oldGuid v)) = do
          newGuid <- lift nextRandom
          liftM (InferredTypeNoLoop . GuidExpression newGuid) $
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

-- TODO: Do this on the GuidExpression TypeRef (no need to handle loops)
builtinsToGlobals
  :: Monad m
  => InferredType -> T m InferredType
builtinsToGlobals expr = do
  globals <- Property.get Anchors.builtinsMap
  let
    f (InferredType entity@(GuidExpression guid (Data.ExpressionBuiltin name))) =
      return . InferredType .
      maybe entity
      (GuidExpression guid . Data.ExpressionGetVariable) $
      Map.lookup name globals
    f entity = return entity
  mapInferredTypes f expr

inferExpression
 :: Monad m
 => Maybe TypeRef
 -> StoredExpression () (T f)
 -> Infer m (TypedStoredExpression (T f))
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
  -> T m (TypedStoredDefinition (T f))
inferDefinition (DataLoad.DefinitionEntity iref value) =
  liftM (StoredDefinition iref) $
  case value of
  Data.Definition typeI bodyI -> do
    let inferredType = canonizeIdentifiersTypes $ fromLoaded [] typeI

    inferredBody <- runInfer $ do
      inferredTypeRef <- typeRefFromEntity inferredType
      inferExpression (Just inferredTypeRef) $ fromLoaded () bodyI
    return $ Data.Definition inferredType inferredBody

loadInferDefinition
  :: Monad m => Data.DefinitionIRef
  -> T m (TypedStoredDefinition (T m))
loadInferDefinition =
  inferDefinition <=< DataLoad.loadDefinition

loadInferExpression
  :: Monad m => Data.ExpressionIRef
  -> T m (TypedStoredExpression (T m))
loadInferExpression =
  runInfer . inferExpression Nothing . fromLoaded () <=<
  flip DataLoad.loadExpression Nothing
