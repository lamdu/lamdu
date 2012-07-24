{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving #-}
module Editor.Data.Typed
  ( atEeInferredType, atEeValue
  , storedGuid
  , LoopGuidExpression(..)
  , TypedStoredDefinition(..)
  , atDeIRef, atDeValue
  , deGuid
  , loadInferDefinition
  , loadInferExpression
  , pureExpressionFromStored
  , pureGuidFromLoop
  , alphaEq
  , unify
  , loadDefTypeWithinContext
  , StoredExpression(..)
  , TypeRef, TypeContext
  , TypedStoredExpression
  , resumeInfer, Infer
  , derefTypeRef, derefResumedInfer
  , makeSingletonTypeRef
  ) where

import Control.Applicative (Applicative)
import Control.Monad (liftM, liftM2, (<=<), when, unless, filterM)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Random (nextRandom, runRandomT)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.Monad.Trans.State (execStateT)
import Control.Monad.Trans.UnionFind (UnionFindT, resumeUnionFindT)
import Data.Function (on)
import Data.Functor.Identity (Identity(..))
import Data.Maybe (fromMaybe)
import Data.Monoid (Any(..), mconcat)
import Data.Store.Guid (Guid)
import Data.Store.Transaction (Transaction)
import Data.UnionFind.IntMap (UnionFind, newUnionFind)
import Editor.Anchors (ViewTag)
import System.Random (RandomGen)
import qualified Control.Monad.Trans.Reader as Reader
import qualified Control.Monad.Trans.State as State
import qualified Control.Monad.Trans.UnionFind as UnionFindT
import qualified Data.AtFieldTH as AtFieldTH
import qualified Data.Map as Map
import qualified Data.Store.Guid as Guid
import qualified Data.Store.IRef as IRef
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Data.UnionFind.IntMap as UnionFind
import qualified Editor.Anchors as Anchors
import qualified Editor.Data as Data
import qualified Editor.Data.Load as DataLoad
import qualified System.Random as Random
import qualified System.Random.Utils as RandomUtils

type T = Transaction ViewTag

type TypedStoredExpression = StoredExpression TypeRef

eipGuid :: Data.ExpressionIRefProperty m -> Guid
eipGuid = IRef.guid . Data.unExpressionIRef . Property.value

storedGuid :: StoredExpression it m -> Guid
storedGuid = eipGuid . eeStored

newtype TypeData = TypeData
  { unTypeData :: [Data.GuidExpression TypeRef]
  } deriving (Show)
type TypeRef = UnionFindT.Point TypeData

data StoredExpression it m = StoredExpression
  { eeStored :: Data.ExpressionIRefProperty m
  , eeInferredType :: it
  , eeInferredValue :: it
  , eeValue :: Data.Expression (StoredExpression it m)
  }

data LoopGuidExpression
  = NoLoop (Data.GuidExpression LoopGuidExpression)
  | Loop Guid
  deriving (Show, Eq)

type TypeContext = UnionFind TypeData

data TypedStoredDefinition m = TypedStoredDefinition
  { deIRef :: Data.DefinitionIRef
  , deInferredType :: TypeRef
  , deValue :: Data.Definition (TypedStoredExpression m)
  , deTypeContext :: TypeContext
  }

deGuid :: TypedStoredDefinition m -> Guid
deGuid = IRef.guid . deIRef

AtFieldTH.make ''StoredExpression
AtFieldTH.make ''TypedStoredDefinition

--------------- Infer Stack boilerplate:

newtype Infer m a = Infer
  { unInfer :: UnionFindT TypeData m a
  } deriving (Functor, Applicative, Monad, MonadTrans)
AtFieldTH.make ''Infer

----------------- Infer operations:

makeTypeRef :: Monad m => [Data.GuidExpression TypeRef] -> Infer m TypeRef
makeTypeRef = Infer . UnionFindT.new . TypeData

getTypeRef :: Monad m => TypeRef -> Infer m [Data.GuidExpression TypeRef]
getTypeRef = liftM unTypeData . Infer . UnionFindT.descr

setTypeRef :: Monad m => TypeRef -> [Data.GuidExpression TypeRef] -> Infer m ()
setTypeRef typeRef types =
  Infer . UnionFindT.setDescr typeRef $
  TypeData types

runInfer :: Monad m => Infer m a -> m (TypeContext, a)
runInfer = resumeInfer newUnionFind

resumeInfer :: Monad m => TypeContext -> Infer m a -> m (TypeContext, a)
resumeInfer typeContext = resumeUnionFindT typeContext . unInfer

makeSingletonTypeRef :: Monad m => Guid -> Data.Expression TypeRef -> Infer m TypeRef
makeSingletonTypeRef _ Data.ExpressionHole = makeTypeRef []
makeSingletonTypeRef guid expr = makeTypeRef [Data.GuidExpression guid expr]

--------------

-- Entities whose Guid does not matter until canonization can just use
-- a zero guid:
zeroGuid :: Guid
zeroGuid = Guid.fromString "ZeroGuid"

fromStoredExpression
  :: Monad m => (Guid -> Data.Expression ref -> m ref)
  -> StoredExpression it f -> m ref
fromStoredExpression mk =
  Data.mapMExpression f
  where
    f (StoredExpression stored _ _ val) =
      ( return val
      , mk $ eipGuid stored
      )

pureGuidFromLoop :: LoopGuidExpression -> Maybe Data.PureGuidExpression
pureGuidFromLoop =
  Data.mapMExpression f
  where
    f Loop {} = ( Nothing, const Nothing )
    f (NoLoop (Data.GuidExpression guid itlExpr)) =
      ( Just itlExpr, Just . Data.PureGuidExpression . Data.GuidExpression guid )

pureExpressionFromStored
  :: StoredExpression it f -> Data.PureGuidExpression
pureExpressionFromStored =
  fmap runIdentity $ fromStoredExpression f
  where
    f guid = return . Data.PureGuidExpression . Data.GuidExpression guid

ignoreStoredMonad
  :: StoredExpression it (T Identity)
  -> StoredExpression it (T Identity)
ignoreStoredMonad = id

loadDefinitionBodyPure ::
  Monad m => Data.DefinitionIRef -> T m Data.PureGuidExpression
loadDefinitionBodyPure =
  liftM
  (pureExpressionFromStored .
   ignoreStoredMonad .
   storedFromLoaded () . Data.defBody . DataLoad.defEntityValue) .
  DataLoad.loadDefinition

expand :: Monad m => Data.PureGuidExpression -> T m Data.PureGuidExpression
expand =
  (`runReaderT` Map.empty) . recurse
  where
    recurse e@(Data.PureGuidExpression (Data.GuidExpression guid val)) =
      case val of
      Data.ExpressionGetVariable (Data.DefinitionRef defI) ->
        -- TODO: expand the result recursively (with some recursive
        -- constraint)
        lift $ loadDefinitionBodyPure defI
      Data.ExpressionGetVariable (Data.ParameterRef guidRef) -> do
        mValue <- Reader.asks (Map.lookup guidRef)
        return $ fromMaybe e mValue
      Data.ExpressionApply
        (Data.Apply
         (Data.PureGuidExpression
          (Data.GuidExpression lamGuid
           -- TODO: Don't ignore paramType, we do want to recursively
           -- type-check this:
           (Data.ExpressionLambda (Data.Lambda _paramType body))))
         arg) -> do
          newArg <- recurse arg
          Reader.local (Map.insert lamGuid newArg) $
            recurse body
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
            Data.PureGuidExpression . Data.GuidExpression guid . cons $
            Data.Lambda paramType newBody

typeRefFromPure :: Monad m => Data.PureGuidExpression -> Infer (T m) TypeRef
typeRefFromPure =
  Data.mapMExpression f <=< lift . expand
  where
    f (Data.PureGuidExpression (Data.GuidExpression guid val)) =
      ( return val
      , makeSingletonTypeRef guid
      )

typeRefFromStored
  :: Monad m => StoredExpression it f
  -> Infer (T m) TypeRef
typeRefFromStored =
  typeRefFromPure . pureExpressionFromStored

storedFromLoaded
  :: it
  -> DataLoad.ExpressionEntity f
  -> StoredExpression it f
storedFromLoaded it =
  runIdentity . Data.mapMExpression f
  where
    f (DataLoad.ExpressionEntity stored val) =
      (return val, makeStored stored)
    makeStored stored newVal =
      return $ StoredExpression stored it it newVal

addTypeRefs
  :: Monad m
  => DataLoad.ExpressionEntity f
  -> Infer (T m) (StoredExpression TypeRef f)
addTypeRefs =
  Data.mapMExpression f . storedFromLoaded ()
  where
    f (StoredExpression irefProp () () val) =
      ( return val
      , \newVal -> do
          typeRef <- makeTypeRef []
          valRef <-
            case newVal of
            Data.ExpressionGetVariable (Data.DefinitionRef ref) ->
               typeRefFromPure =<< lift (loadDefinitionBodyPure ref)
            _ -> makeSingletonTypeRef (eipGuid irefProp) $ fmap eeInferredValue newVal
          return $ StoredExpression irefProp typeRef valRef newVal
      )

derefResumedInfer
  :: (Monad m, RandomGen g)
  => g -> Anchors.BuiltinsMap -> TypeContext
  -> Infer m TypeRef -> m [LoopGuidExpression]
derefResumedInfer gen builtinsMap typeContext action = do
  (newTypeContext, typeRef) <- resumeInfer typeContext action
  return $ derefTypeRef gen builtinsMap newTypeContext typeRef

derefTypeRef
  :: RandomGen g
  => g -> Anchors.BuiltinsMap
  -> TypeContext -> TypeRef -> [LoopGuidExpression]
derefTypeRef stdGen builtinsMap typeContext rootTypeRef =
  (canonizeInferredExpression stdGen .
   map (builtinsToGlobals builtinsMap)) .
  (`runReaderT` []) $ go rootTypeRef
  where
    canonizeInferredExpression = zipWith canonizeIdentifiers . RandomUtils.splits
    go typeRef = do
      visited <- Reader.ask
      if any (UnionFind.equivalent typeContext typeRef) visited
        then return $ Loop zeroGuid
        else
        onType typeRef <=<
        lift . unTypeData $
        UnionFind.descr typeContext typeRef
    onType typeRef (Data.GuidExpression guid expr) =
      liftM (NoLoop . Data.GuidExpression guid) .
      Data.sequenceExpression $ fmap recurse expr
      where
        recurse =
          Reader.mapReaderT holify .
          Reader.local (typeRef :) .
          go
        holify [] =
          [NoLoop
           (Data.GuidExpression zeroGuid Data.ExpressionHole)]
        holify xs = xs

unifyOnTree
  :: Monad m
  => (Data.DefinitionIRef -> Infer (T m) TypeRef)
  -> StoredExpression TypeRef f
  -> Infer (T m) ()
unifyOnTree defTypeRef = (`runReaderT` []) . go
  where
    go (StoredExpression stored typeRef _ value) =
      case value of
      Data.ExpressionLambda (Data.Lambda paramType body) -> do
        let paramTypeRef = eeInferredValue paramType
        -- We use "flip unify typeRef" so that the new Pi will be
        -- the official Pi guid due to the "left-bias" in
        -- unify/unifyPair. Thus we can later assume that we got the
        -- same guid in the pi and the lambda.
        lift $ flip unify typeRef <=< makePi (eipGuid stored) .
          Data.Lambda paramTypeRef $ eeInferredType body
        Reader.local ((eipGuid stored, paramTypeRef):) $ go body
      Data.ExpressionPi (Data.Lambda paramType resultType) -> do
        let paramTypeRef = eeInferredValue paramType
        lift . setType $ eeInferredType resultType
        Reader.local ((eipGuid stored, paramTypeRef):) $ go resultType
      Data.ExpressionApply (Data.Apply func arg) -> do
        go func
        go arg
        lift $ do
          let
            funcTypeRef = eeInferredType func
            argTypeRef = eeInferredType arg
            argValRef = eeInferredValue arg
          -- We give the new Pi the same Guid as the Apply. This is
          -- fine because Apply's Guid is meaningless and the
          -- canonization will fix it later anyway.
          unify funcTypeRef <=< makePi (eipGuid stored) $
            Data.Lambda argTypeRef typeRef
          funcTypes <- getTypeRef funcTypeRef
          sequence_
            [ subst piGuid (return argValRef) piResultTypeRef
            | Data.GuidExpression piGuid
              (Data.ExpressionPi (Data.Lambda _ piResultTypeRef))
              <- funcTypes
            ]
      Data.ExpressionGetVariable (Data.ParameterRef guid) -> do
        mParamTypeRef <- Reader.asks $ lookup guid
        lift $ case mParamTypeRef of
          -- TODO: Not in scope: Bad code,
          -- add an OutOfScopeReference type error
          Nothing -> return ()
          Just paramTypeRef -> setType paramTypeRef
      Data.ExpressionGetVariable (Data.DefinitionRef defI) ->
        lift $ setType =<< defTypeRef defI
      Data.ExpressionLiteralInteger _ ->
        lift $ setType =<<
        typeRefFromStored . storedFromLoaded () =<<
        lift (DataLoad.loadExpression =<< Anchors.integerType)
      Data.ExpressionBuiltin (Data.Builtin _ bType) ->
        lift $ setType =<< typeRefFromStored bType
      _ -> return ()
      where
        makePi guid = makeSingletonTypeRef guid . Data.ExpressionPi
        setType = unify typeRef

unify
  :: Monad m
  => TypeRef
  -> TypeRef
  -> Infer m ()
unify a b = do
  e <- Infer $ UnionFindT.equivalent a b
  unless e $ do
    as <- getTypeRef a
    bs <- getTypeRef b
    bConflicts <- filterM (liftM not . matches as) bs
    -- Need to re-get a after the side-effecting matches:
    result <- liftM (++ bConflicts) $ getTypeRef a
    Infer $ do
      a `UnionFindT.union` b
      UnionFindT.setDescr a $ TypeData result
  where
    matches as y = liftM or $ mapM (`unifyPair` y) as

-- biased towards left child (if unifying Pis,
-- substs right child's guids to left)
unifyPair
  :: Monad m
  => Data.GuidExpression TypeRef
  -> Data.GuidExpression TypeRef
  -> Infer m Bool
unifyPair
  (Data.GuidExpression aGuid aVal)
  (Data.GuidExpression bGuid bVal)
  = case (aVal, bVal) of
    (Data.ExpressionPi l1,
     Data.ExpressionPi l2) ->
      unifyLambdas l1 l2
    (Data.ExpressionLambda l1,
     Data.ExpressionLambda l2) ->
      unifyLambdas l1 l2
    (Data.ExpressionApply (Data.Apply aFuncTypeRef aArgTypeRef),
     Data.ExpressionApply (Data.Apply bFuncTypeRef bArgTypeRef)) -> do
      unify aFuncTypeRef bFuncTypeRef
      unify aArgTypeRef bArgTypeRef
      return True
    (Data.ExpressionBuiltin (Data.Builtin name1 _),
     Data.ExpressionBuiltin (Data.Builtin name2 _)) -> return $ name1 == name2
    (Data.ExpressionGetVariable v1,
     Data.ExpressionGetVariable v2) -> return $ v1 == v2
    (Data.ExpressionLiteralInteger i1,
     Data.ExpressionLiteralInteger i2) -> return $ i1 == i2
    (Data.ExpressionMagic,
     Data.ExpressionMagic) -> return True
    _ -> return False
  where
    unifyLambdas
      (Data.Lambda aParamTypeRef aResultTypeRef)
      (Data.Lambda bParamTypeRef bResultTypeRef) = do
      unify aParamTypeRef bParamTypeRef
      -- Remap b's guid to a's guid and return a as the unification:
      let
        mkGetAGuidRef =
          makeSingletonTypeRef zeroGuid . Data.ExpressionGetVariable $
          Data.ParameterRef aGuid
      subst bGuid mkGetAGuidRef bResultTypeRef
      unify aResultTypeRef bResultTypeRef
      return True

subst :: Monad m => Guid -> Infer m TypeRef -> TypeRef -> Infer m ()
subst from mkTo rootRef = do
  refs <- allUnder rootRef
  mapM_ replace refs
  where
    removeFrom
      a@(Data.GuidExpression _
       (Data.ExpressionGetVariable (Data.ParameterRef guidRef)))
      | guidRef == from = (Any True, [])
      | otherwise = (Any False, [a])
    removeFrom x = (Any False, [x])
    replace typeRef = do
      (Any removed, new) <- liftM (mconcat . map removeFrom) $ getTypeRef typeRef
      when removed $ do
        setTypeRef typeRef new
        unify typeRef =<< mkTo

allUnder :: Monad m => TypeRef -> Infer m [TypeRef]
allUnder =
  (`execStateT` []) . recurse
  where
    recurse typeRef = do
      visited <- State.get
      alreadySeen <-
        lift . Infer . liftM or $
        mapM (UnionFindT.equivalent typeRef) visited
      unless alreadySeen $ do
        State.modify (typeRef :)
        types <- lift $ getTypeRef typeRef
        mapM_ onType types
    onType =
      Data.sequenceExpression . fmap recurse . Data.geValue

canonizeIdentifiers
  :: RandomGen g => g -> LoopGuidExpression -> LoopGuidExpression
canonizeIdentifiers gen =
  runIdentity . runRandomT gen . (`runReaderT` Map.empty) . f
  where
    onLambda oldGuid newGuid (Data.Lambda paramType body) =
      liftM2 Data.Lambda (f paramType) .
      Reader.local (Map.insert oldGuid newGuid) $ f body
    f (Loop _oldGuid) =
      lift . liftM Loop $ nextRandom
    f (NoLoop (Data.GuidExpression oldGuid v)) = do
      newGuid <- lift nextRandom
      liftM (NoLoop . Data.GuidExpression newGuid) $
        case v of
        Data.ExpressionLambda lambda ->
          liftM Data.ExpressionLambda $ onLambda oldGuid newGuid lambda
        Data.ExpressionPi lambda ->
          liftM Data.ExpressionPi $ onLambda oldGuid newGuid lambda
        Data.ExpressionApply (Data.Apply func arg) ->
          liftM Data.ExpressionApply $
          liftM2 Data.Apply (f func) (f arg)
        gv@(Data.ExpressionGetVariable (Data.ParameterRef guid)) ->
          Reader.asks $
          maybe gv (Data.ExpressionGetVariable . Data.ParameterRef) .
          Map.lookup guid
        x -> return x

alphaEq :: Data.PureGuidExpression -> Data.PureGuidExpression -> Bool
alphaEq =
  on (==) (canonizeIdentifiers (Random.mkStdGen 0) . toLoop)
  where
    toLoop = runIdentity . Data.mapMExpression f
    f (Data.PureGuidExpression (Data.GuidExpression guid expr)) =
      ( Identity expr
      , Identity . NoLoop . Data.GuidExpression guid
      )

builtinsToGlobals :: Anchors.BuiltinsMap -> LoopGuidExpression -> LoopGuidExpression
builtinsToGlobals _ x@(Loop _) = x
builtinsToGlobals builtinsMap (NoLoop (Data.GuidExpression guid expr)) =
  NoLoop . Data.GuidExpression guid $
  fmap (builtinsToGlobals builtinsMap) $
  case expr of
  builtin@(Data.ExpressionBuiltin (Data.Builtin name _)) ->
    (maybe builtin Data.ExpressionGetVariable . Map.lookup name)
    builtinsMap
  other -> other

loadDefTypePure :: Monad m => Data.DefinitionIRef -> T m Data.PureGuidExpression
loadDefTypePure = DataLoad.loadPureExpression . Data.defType <=< Transaction.readIRef

loadDefTypeToTypeRef :: Monad m => Data.DefinitionIRef -> Infer (T m) TypeRef
loadDefTypeToTypeRef = typeRefFromPure <=< lift . loadDefTypePure

loadDefTypeWithinContext
  :: Monad m
  => Maybe (TypedStoredDefinition (T m)) -> Data.DefinitionIRef -> Infer (T m) TypeRef
loadDefTypeWithinContext Nothing = loadDefTypeToTypeRef
loadDefTypeWithinContext (Just def) = getDefTypeRef (deInferredType def) (deIRef def)

getDefTypeRef
  :: Monad m => TypeRef -> Data.DefinitionIRef -> Data.DefinitionIRef -> Infer (T m) TypeRef
getDefTypeRef defTypeRef defI getDefI
  | getDefI == defI = return defTypeRef
  | otherwise = loadDefTypeToTypeRef getDefI

inferDefinition
  :: (Monad m, Monad f)
  => DataLoad.DefinitionEntity (T f)
  -> T m (TypedStoredDefinition (T f))
inferDefinition (DataLoad.DefinitionEntity defI (Data.Definition bodyI typeI)) = do
  (typeContext, (bodyStored, typeStored)) <- runInfer $ do
    bodyStored <- addTypeRefs bodyI
    typeStored <- addTypeRefs typeI
    unifyOnTree (getDefTypeRef (eeInferredType bodyStored) defI) bodyStored
    return (bodyStored, typeStored)
  return TypedStoredDefinition
    { deIRef = defI
    , deInferredType = eeInferredType bodyStored
    , deValue = Data.Definition bodyStored typeStored
    , deTypeContext = typeContext
    }

loadInferDefinition
  :: Monad m => Data.DefinitionIRef
  -> T m (TypedStoredDefinition (T m))
loadInferDefinition =
  inferDefinition <=< DataLoad.loadDefinition

loadInferExpression
  :: Monad m
  => Data.ExpressionIRefProperty (T m)
  -> T m (TypedStoredExpression (T m))
loadInferExpression exprProp = do
  expr <- DataLoad.loadExpression exprProp
  liftM snd . runInfer $ do
    exprStored <- addTypeRefs expr
    unifyOnTree loadDefTypeToTypeRef exprStored
    return exprStored
