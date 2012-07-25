{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving #-}
module Editor.Data.Typed
  ( LoopGuidExpression(..)
  , pureGuidFromLoop

  , Expression(..), StoredExpression
  , pureInferExpressionWithinContext
  , atEeInferredType, atEeValue
  , loadInferExpression, inferExpression
  , toPureExpression
  , alphaEq

  , StoredDefinition(..)
  , atDeIRef, atDeValue
  , deGuid
  , loadInferDefinition
  , loadDefTypeWithinContext

  , TypeRef, TypeContext
  , Infer, resumeInfer
  , unify, derefTypeRef, derefResumedInfer
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

eipGuid :: Data.ExpressionIRefProperty m -> Guid
eipGuid = IRef.guid . Data.unExpressionIRef . Property.value

newtype TypeData = TypeData
  { unTypeData :: [Data.GuidExpression TypeRef]
  } deriving (Show)
type TypeRef = UnionFindT.Point TypeData

data Expression s = Expression
  { eeRef :: s
  , eeGuid :: Guid
  , eeInferredType :: TypeRef
  , eeInferredValue :: TypeRef
  , eeValue :: Data.Expression (Expression s)
  }

type StoredExpression f = Expression (Data.ExpressionIRefProperty f)

data LoopGuidExpression
  = NoLoop (Data.GuidExpression LoopGuidExpression)
  | Loop Guid
  deriving (Show, Eq)

type TypeContext = UnionFind TypeData

data StoredDefinition m = StoredDefinition
  { deIRef :: Data.DefinitionIRef
  , deInferredType :: TypeRef
  , deValue :: Data.Definition (StoredExpression m)
  , deTypeContext :: TypeContext
  }

deGuid :: StoredDefinition m -> Guid
deGuid = IRef.guid . deIRef

AtFieldTH.make ''Expression
AtFieldTH.make ''StoredDefinition

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

fromExpression
  :: Monad m => (Guid -> Data.Expression ref -> m ref)
  -> Expression s -> m ref
fromExpression mk =
  Data.mapMExpression f
  where
    f (Expression _ g _ _ val) = (return val, mk g)

pureGuidFromLoop :: LoopGuidExpression -> Maybe Data.PureGuidExpression
pureGuidFromLoop =
  Data.mapMExpression f
  where
    f Loop {} = ( Nothing, const Nothing )
    f (NoLoop (Data.GuidExpression guid itlExpr)) =
      ( Just itlExpr, Just . Data.PureGuidExpression . Data.GuidExpression guid )

toPureExpression :: Expression s -> Data.PureGuidExpression
toPureExpression =
  fmap runIdentity $ fromExpression f
  where
    f guid = return . Data.PureGuidExpression . Data.GuidExpression guid

expand :: Monad m => Data.PureGuidExpression -> T m Data.PureGuidExpression
expand =
  (`runReaderT` Map.empty) . recurse
  where
    recurse e@(Data.PureGuidExpression (Data.GuidExpression guid val)) =
      case val of
      Data.ExpressionGetVariable (Data.DefinitionRef defI) ->
        -- TODO: expand the result recursively (with some recursive
        -- constraint)
        lift $ DataLoad.loadPureDefinitionBody defI
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

toExpr
  :: Monad m
  => (expr -> (Guid, Data.Expression expr, s))
  -> expr
  -> Infer (T m) (Expression s)
toExpr extract =
  Data.mapMExpression f
  where
    f wrapped =
      ( return expr
      , \newVal -> do
          typeRef <- makeTypeRef []
          valRef <-
            case newVal of
            Data.ExpressionGetVariable (Data.DefinitionRef ref) ->
               typeRefFromPure =<< lift (DataLoad.loadPureDefinitionBody ref)
            _ -> makeSingletonTypeRef g $ fmap eeInferredValue newVal
          return $ Expression prop g typeRef valRef newVal
      )
      where
        (g, expr, prop) = extract wrapped

fromPure :: Monad m => Data.PureGuidExpression -> Infer (T m) (Expression ())
fromPure =
  toExpr extract
  where
    extract (Data.PureGuidExpression (Data.GuidExpression g expr)) =
      ( g, expr, () )

fromLoaded
  :: Monad m
  => DataLoad.ExpressionEntity f
  -> Infer (T m) (StoredExpression f)
fromLoaded =
  toExpr extract
  where
    extract (DataLoad.ExpressionEntity irefProp val) =
      ( eipGuid irefProp, val, irefProp )

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

inferExpression
  :: Monad m
  => (Data.DefinitionIRef -> Infer (T m) TypeRef)
  -> Expression s
  -> Infer (T m) ()
inferExpression defTypeRef = (`runReaderT` []) . go
  where
    go (Expression _ g typeRef _ value) =
      case value of
      Data.ExpressionLambda (Data.Lambda paramType body) -> do
        let paramTypeRef = eeInferredValue paramType
        -- We use "flip unify typeRef" so that the new Pi will be
        -- the official Pi guid due to the "left-bias" in
        -- unify/unifyPair. Thus we can later assume that we got the
        -- same guid in the pi and the lambda.
        lift $ flip unify typeRef <=< makePi g .
          Data.Lambda paramTypeRef $ eeInferredType body
        Reader.local ((g, paramTypeRef):) $ go body
      Data.ExpressionPi (Data.Lambda paramType resultType) -> do
        let paramTypeRef = eeInferredValue paramType
        lift . setType $ eeInferredType resultType
        Reader.local ((g, paramTypeRef):) $ go resultType
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
          unify funcTypeRef <=< makePi g $
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
        typeRefFromPure =<<
        lift
        (DataLoad.loadPureExpression . Property.value =<<
         Anchors.integerType)
      Data.ExpressionBuiltin (Data.Builtin _ bType) ->
        lift $ setType =<< typeRefFromPure (toPureExpression bType)
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
  => Maybe (StoredDefinition (T m)) -> Data.DefinitionIRef -> Infer (T m) TypeRef
loadDefTypeWithinContext Nothing = loadDefTypeToTypeRef
loadDefTypeWithinContext (Just def) = getDefTypeRef (deInferredType def) (deIRef def)

pureInferExpressionWithinContext
  :: Monad m
  => Maybe (StoredDefinition (T m))
  -> Data.PureGuidExpression -> Infer (T m) (Expression ())
pureInferExpressionWithinContext mDef pureExpr = do
  expr <- fromPure pureExpr
  inferExpression (loadDefTypeWithinContext mDef) expr
  return expr

getDefTypeRef
  :: Monad m => TypeRef -> Data.DefinitionIRef -> Data.DefinitionIRef -> Infer (T m) TypeRef
getDefTypeRef defTypeRef defI getDefI
  | getDefI == defI = return defTypeRef
  | otherwise = loadDefTypeToTypeRef getDefI

inferDefinition
  :: (Monad m, Monad f)
  => DataLoad.DefinitionEntity (T f)
  -> T m (StoredDefinition (T f))
inferDefinition (DataLoad.DefinitionEntity defI (Data.Definition bodyI typeI)) = do
  (typeContext, (bodyExpr, typeExpr)) <- runInfer $ do
    bodyExpr <- fromLoaded bodyI
    typeExpr <- fromLoaded typeI
    inferExpression (getDefTypeRef (eeInferredType bodyExpr) defI) bodyExpr
    return (bodyExpr, typeExpr)
  return StoredDefinition
    { deIRef = defI
    , deInferredType = eeInferredType bodyExpr
    , deValue = Data.Definition bodyExpr typeExpr
    , deTypeContext = typeContext
    }

loadInferDefinition
  :: Monad m => Data.DefinitionIRef
  -> T m (StoredDefinition (T m))
loadInferDefinition =
  inferDefinition <=< DataLoad.loadDefinition

loadInferExpression
  :: Monad m
  => Data.ExpressionIRefProperty (T m)
  -> T m (Expression (Data.ExpressionIRefProperty (T m)))
loadInferExpression exprProp = do
  expr <- DataLoad.loadExpression exprProp
  liftM snd . runInfer $ do
    tExpr <- fromLoaded expr
    inferExpression loadDefTypeToTypeRef tExpr
    return tExpr
