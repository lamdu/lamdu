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

  , Ref, Scope
  , TypeContext, emptyTypeContext
  , Infer, resumeInfer
  , InferActions(..), inferActions, liftInferActions
  , unify, derefRef
  , makeSingletonRef
  ) where

import Control.Applicative (Applicative, liftA2)
import Control.Arrow (first, second)
import Control.Monad (liftM, liftM2, (<=<), when, unless)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Random (nextRandom, runRandomT)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.Monad.Trans.State (StateT(..))
import Data.Function (on)
import Data.Functor.Identity (Identity(..))
import Data.Map (Map)
import Data.Maybe (fromMaybe, catMaybes, isNothing)
import Data.Set (Set)
import Data.Store.Guid (Guid)
import Data.Store.Transaction (Transaction)
import Data.UnionFind.IntMap (UnionFind)
import Editor.Anchors (ViewTag)
import System.Random (RandomGen)
import qualified Control.Monad.Trans.Reader as Reader
import qualified Control.Monad.Trans.State as State
import qualified Data.AtFieldTH as AtFieldTH
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Store.Guid as Guid
import qualified Data.Store.IRef as IRef
import qualified Data.Store.Property as Property
import qualified Data.Tuple as Tuple
import qualified Data.UnionFind.IntMap as UnionFind
import qualified Editor.Anchors as Anchors
import qualified Editor.Data as Data
import qualified Editor.Data.Load as DataLoad
import qualified System.Random as Random
import qualified System.Random.Utils as RandomUtils

type T = Transaction ViewTag

eipGuid :: Data.ExpressionIRefProperty m -> Guid
eipGuid = IRef.guid . Data.unExpressionIRef . Property.value

-- Rules about inferring apply type/value relationships:
--
-- If you have:
--
-- func:funcT  arg:argT
-- --------------------
--     apply:applyT
--
-- then:
--
-- resultT <=> new independent typeref
-- funcT <=> Pi (x:argT) -> resultT
-- applyT <= foreach Pi in funcT, result of the Pi: subst: (x => arg)
-- apply <= foreach Lam in func, result(body) subst'd: (x => arg)
--
-- Whenever a Pi result component is a hole, it can be polymorphic,
-- which means it can refer to the Pi arg[s] before it, in which case
-- it would not be valid to infer its type as monomorphic (unify into
-- it). After a "subst" replaces GetVariable's in a Pi result with
-- anything (including holes), we know they can no longer possibly
-- refer to previous Pi args, therefore they are monomorphic and can
-- be unified into.

data ApplyPos = ApplyFuncType | ApplyFuncValue
  deriving (Show, Eq)

data UnifyRule = UnifyRule
  { urApplyPos :: ApplyPos
  , urDest :: Ref
  , urArg :: Ref
  } deriving (Show)

data Constraints = Constraints
  { tcLambdaGuids :: Set Guid
  , tcExprs :: [Data.Expression FatRef]
  , tcRules :: [UnifyRule]
  }

instance Show Constraints where
  show (Constraints guids exprs rules) =
    unwords
    [ Set.toList guids >>= ((++ ":") . show)
    , show exprs
    , show rules
    ]

emptyConstraints :: Constraints
emptyConstraints = Constraints Set.empty [] []

type Ref = UnionFind.Point Constraints
data FatRef = FatRef
  { frPoint :: Ref
  , _frIsSubst :: Bool
  }
instance Show FatRef where
  show (FatRef point False) = show point
  show (FatRef point True) = 'F' : show point

fatten :: Ref -> FatRef
fatten r = FatRef r False

data Expression s = Expression
  { eeRef :: s
  , eeGuid :: Guid
  , eeInferredType :: Ref
  , eeInferredValue :: Ref
  , eeValue :: Data.Expression (Expression s)
  }
instance Show (Expression s) where
  show (Expression _ guid it iv value) =
    unwords ["(", show guid, show value, "(=", show iv, ") :", show it, ")"]

type StoredExpression f = Expression (Data.ExpressionIRefProperty f)

data LoopGuidExpression
  = NoLoop (Data.GuidExpression LoopGuidExpression)
  | Loop Guid
  deriving (Show, Eq)

data TypeContext = TypeContext
  { tcConstraints :: UnionFind Constraints
  , tcParamGuidMap :: Map Guid Guid
  }
AtFieldTH.make ''TypeContext

emptyTypeContext :: TypeContext
emptyTypeContext = TypeContext UnionFind.empty Map.empty

data StoredDefinition m = StoredDefinition
  { deIRef :: Data.DefinitionIRef
  , deInferredType :: Ref
  , deValue :: Data.Definition (StoredExpression m)
  , deTypeContext :: TypeContext
  }

deGuid :: StoredDefinition m -> Guid
deGuid = IRef.guid . deIRef

AtFieldTH.make ''Expression
AtFieldTH.make ''StoredDefinition

--------------- Infer Stack boilerplate:

data InferActions m = InferActions
  { onConflict :: m ()
  , loadPureDefinitionBody :: Data.DefinitionIRef -> m Data.PureGuidExpression
  , loadPureDefinitionType :: Data.DefinitionIRef -> m Data.PureGuidExpression
  }

inferActions :: Monad m => InferActions (T m)
inferActions = InferActions
  { onConflict = return ()
  , loadPureDefinitionBody = DataLoad.loadPureDefinitionBody
  , loadPureDefinitionType = DataLoad.loadPureDefinitionType
  }

liftInferActions :: (MonadTrans t, Monad m) => InferActions m -> InferActions (t m)
liftInferActions (InferActions a b c) =
  InferActions (lift a) (fmap lift b) (fmap lift c)

newtype Infer m a = Infer
  { unInfer :: ReaderT (InferActions m) (StateT TypeContext m) a
  } deriving (Functor, Applicative, Monad)
AtFieldTH.make ''Infer

----------------- Infer operations:

liftConflictActionReader :: ReaderT (InferActions m) (StateT TypeContext m) a -> Infer m a
liftConflictActionReader = Infer

liftTypeContext :: Monad m => StateT TypeContext m a -> Infer m a
liftTypeContext = liftConflictActionReader . lift

instance MonadTrans Infer where
  lift = liftTypeContext . lift

getsConstraints :: Monad m => (UnionFind Constraints -> a) -> Infer m a
getsConstraints f = liftTypeContext $ State.gets (f . tcConstraints)

getsParamGuidMap :: Monad m => (Map Guid Guid -> a) -> Infer m a
getsParamGuidMap f = liftTypeContext $ State.gets (f . tcParamGuidMap)

makeRef :: Monad m => Constraints -> Infer m Ref
makeRef x =
  liftTypeContext $ StateT f
  where
    f state =
      return (result, state { tcConstraints = newConstraints} )
      where
        (result, newConstraints) = UnionFind.fresh x $ tcConstraints state

getRef :: Monad m => Ref -> Infer m Constraints
getRef = getsConstraints . UnionFind.descr

getActions :: Monad m => Infer m (InferActions (Infer m))
getActions = liftConflictActionReader $ Reader.asks liftInferActions

runInfer :: Monad m => InferActions m -> Infer m a -> m (TypeContext, a)
runInfer actions = resumeInfer actions emptyTypeContext

resumeInfer
  :: Monad m
  => InferActions m -> TypeContext -> Infer m a -> m (TypeContext, a)
resumeInfer actions typeContext =
  liftM Tuple.swap . (`runStateT` typeContext) . (`runReaderT` actions) . unInfer

makeNoConstraints :: Monad m => Infer m Ref
makeNoConstraints = makeRef emptyConstraints

makeRuleRef :: Monad m => UnifyRule -> Infer m Ref
makeRuleRef rule = makeRef emptyConstraints { tcRules = [rule] }

makeSingletonRef :: Monad m => [Guid] -> Data.Expression FatRef -> Infer m Ref
makeSingletonRef guids expr =
  case expr of
  Data.ExpressionHole -> makeNoConstraints
  Data.ExpressionLambda _ -> makeLambda
  Data.ExpressionPi _ -> makeLambda
  _ -> makeRef emptyConstraints { tcLambdaGuids = Set.empty, tcExprs = [expr] }
  where
    makeLambda =
      makeRef emptyConstraints { tcLambdaGuids = Set.fromList guids, tcExprs = [expr] }

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

expand :: Monad m => Data.PureGuidExpression -> Infer m Data.PureGuidExpression
expand e@(Data.PureGuidExpression (Data.GuidExpression guid val)) =
  case val of
  Data.ExpressionGetVariable (Data.DefinitionRef defI) ->
    -- TODO: expand the result recursively (with some recursive
    -- constraint)
    (`loadPureDefinitionBody` defI) =<< getActions
  Data.ExpressionApply (Data.Apply func arg) ->
    liftM (makePureGuidExpr . Data.ExpressionApply) $
    liftM2 Data.Apply (expand func) (expand arg)
  Data.ExpressionLambda lambda ->
    recurseLambda Data.ExpressionLambda lambda
  Data.ExpressionPi lambda ->
    recurseLambda Data.ExpressionPi lambda
  _ -> return e
  where
    makePureGuidExpr =
      Data.PureGuidExpression . Data.GuidExpression guid
    recurseLambda cons (Data.Lambda paramType body) =
      liftM (makePureGuidExpr . cons) $
      liftM2 Data.Lambda (expand paramType) (expand body)

refFromPure :: Monad m => Data.PureGuidExpression -> Infer m Ref
refFromPure =
  liftM frPoint . Data.mapMExpression f <=< expand
  where
    f (Data.PureGuidExpression (Data.GuidExpression guid val)) =
      ( return val
      , liftM fatten . makeSingletonRef [guid]
      )

toExpr
  :: Monad m
  => (expr -> (Guid, Data.Expression expr, s))
  -> expr
  -> Infer m (Expression s)
toExpr extract =
  Data.mapMExpression f
  where
    f wrapped =
      ( return expr
      , \newVal -> do
          typeRef <- makeNoConstraints
          valRef <-
            case newVal of
            Data.ExpressionGetVariable (Data.DefinitionRef ref) ->
               refFromPure =<< (`loadPureDefinitionBody` ref) =<< getActions
            -- Apply might be of lambda (can be detected much later)
            -- and in that case, it's a redex we want to peel off the
            -- Apply-of-Lambda wrapper. So we defer the insertion of
            -- the Apply into the constraints until later when it is
            -- known whether it is a redex or not.
            Data.ExpressionApply _ -> makeNoConstraints
            _ -> makeSingletonRef [g] $ fmap (fatten . eeInferredValue) newVal
          return $ Expression prop g typeRef valRef newVal
      )
      where
        (g, expr, prop) = extract wrapped

fromPure :: Monad m => Data.PureGuidExpression -> Infer m (Expression ())
fromPure =
  toExpr extract
  where
    extract (Data.PureGuidExpression (Data.GuidExpression g expr)) =
      ( g, expr, () )

fromLoaded
  :: Monad m
  => DataLoad.ExpressionEntity f
  -> Infer m (StoredExpression f)
fromLoaded =
  toExpr extract
  where
    extract (DataLoad.ExpressionEntity irefProp val) =
      ( eipGuid irefProp, val, irefProp )

derefRef
  :: RandomGen g
  => g -> Anchors.BuiltinsMap
  -> TypeContext -> Ref -> [LoopGuidExpression]
derefRef stdGen builtinsMap typeContext rootRef =
  (canonizeInferredExpression stdGen .
   map (builtinsToGlobals builtinsMap)) .
  (`runReaderT` (Set.empty, Map.empty)) $ go rootRef
  where
    canonizeInferredExpression = zipWith canonizeIdentifiers . RandomUtils.splits
    go ref = do
      visited <- Reader.asks fst
      if Set.member (UnionFind.repr ref (tcConstraints typeContext)) visited
        then return $ Loop zeroGuid
        else do
          let
            constraints = UnionFind.descr ref $ tcConstraints typeContext
            (guid, mapping) =
              case Set.toList (tcLambdaGuids constraints) of
              [] -> (zeroGuid, Map.empty)
              (x : xs) -> (x, Map.fromList (map (flip (,) x) xs))
          expr <- lift $ tcExprs constraints
          liftM (NoLoop . Data.GuidExpression guid) .
            Reader.local
            ( (first . Set.insert) (UnionFind.repr ref (tcConstraints typeContext))
            . (second . Map.union) mapping
            ) $ recurse expr
    recurse (Data.ExpressionGetVariable (Data.ParameterRef p)) =
      Reader.asks
      ( Data.ExpressionGetVariable . Data.ParameterRef
      . fromMaybe p . Map.lookup p . snd
      )
    recurse expr =
      Data.sequenceExpression $ fmap (Reader.mapReaderT holify . go . frPoint) expr
    holify [] =
      [NoLoop
       (Data.GuidExpression zeroGuid Data.ExpressionHole)]
    holify xs = xs

type Scope = Map Guid Ref

inferExpression
  :: Monad m
  => Scope
  -> (Data.DefinitionIRef -> Infer m Ref)
  -> Expression s
  -> Infer m ()
inferExpression scope defRef (Expression _ g typeRef valueRef value) =
  case value of
  Data.ExpressionLambda lambda@(Data.Lambda paramType body) -> do
    onLambda lambda
    -- We use "flip unify typeRef" so that the new Pi will be
    -- the official Pi guid due to the "left-bias" in
    -- unify/unifyPair. Thus we can later assume that we got the
    -- same guid in the pi and the lambda.
    flip unify typeRef <=< makePi g .
      wrapCons Data.Lambda (eeInferredValue paramType) $ eeInferredType body
  Data.ExpressionPi lambda@(Data.Lambda _ resultType) -> do
    onLambda lambda
    -- TODO: Is Set:Set a good idea? Agda uses "eeInferredType
    -- resultType" as the type
    setType =<< mkSet
    unify (eeInferredType resultType) =<< mkSet
  Data.ExpressionApply (Data.Apply func arg) -> do
    go [] func
    go [] arg
    let
      funcValRef = eeInferredValue func
      funcTypeRef = eeInferredType func
      argValRef = eeInferredValue arg
      argTypeRef = eeInferredType arg
    -- We give the new Pi the same Guid as the Apply. This is
    -- fine because Apply's Guid is meaningless and the
    -- canonization will fix it later anyway.
    unify funcTypeRef =<<
      makePi g . wrapCons Data.Lambda argTypeRef =<< makeNoConstraints
    unify funcTypeRef =<<
      makeRuleRef UnifyRule
        { urApplyPos = ApplyFuncType
        , urDest = typeRef
        , urArg = argValRef
        }
    unify funcValRef =<<
      makeRuleRef UnifyRule
        { urApplyPos = ApplyFuncValue
        , urDest = valueRef
        , urArg = argValRef
        }
  Data.ExpressionGetVariable (Data.ParameterRef guid) ->
    case Map.lookup guid scope of
      -- TODO: Not in scope: Bad code,
      -- add an OutOfScopeReference type error
      Nothing -> return ()
      Just paramRef -> setType paramRef
  Data.ExpressionGetVariable (Data.DefinitionRef defI) ->
    setType =<< defRef defI
  Data.ExpressionLiteralInteger _ ->
    setType =<< mkBuiltin ["Prelude"] "Integer"
  Data.ExpressionBuiltin (Data.Builtin _ bType) ->
    setType =<< refFromPure (toPureExpression bType)
  _ -> return ()
  where
    wrapCons = (`on` fatten)
    mkSet = makeSingletonRef [] Data.ExpressionSet
    mkBuiltin path name =
      makeSingletonRef [] . Data.ExpressionBuiltin .
      Data.Builtin (Data.FFIName path name) . fatten =<<
      makeNoConstraints
    makePi guid = makeSingletonRef [guid] . Data.ExpressionPi
    setType = unify typeRef
    go newVars = inferExpression (Map.fromList newVars `Map.union` scope) defRef
    onLambda (Data.Lambda paramType result) = do
      go [] paramType
      go [(g, eeInferredValue paramType)] result
      unify (eeInferredType paramType) =<< mkSet

-- New Ref tree has tcRules=[] everywhere
subst :: Monad m => Set Guid -> Ref -> FatRef -> Infer m FatRef
subst _ _ src@(FatRef _ True) = return src
subst from to (FatRef ref False) = do
  constraints <- getRef ref
  let
    fooResults = map foo $ tcExprs constraints
    isSubst = any isNothing fooResults
  exprs <- sequence $ catMaybes fooResults
  newRef <- makeRef emptyConstraints
    { tcLambdaGuids = tcLambdaGuids constraints
    , tcExprs = exprs
    }
  when isSubst $ unify newRef to
  return $ FatRef newRef isSubst
  where
    foo (Data.ExpressionGetVariable (Data.ParameterRef p))
      | Set.member p from = Nothing
    foo expr = Just . Data.sequenceExpression $ fmap (subst from to) expr

unify :: Monad m => Ref -> Ref -> Infer m ()
unify a b = do
  e <- getsConstraints $ UnionFind.equivalent a b
  unless e $ do
    -- Mark as unified immediately so recursive unifications hitting
    -- this pair will know it's being done...
    aConstraints <- getRef a
    bConstraints <- getRef b
    liftTypeContext . State.modify . atTcConstraints $ a `UnionFind.union` b
    paramGuidMapping <- getsParamGuidMap id
    let
      (unifiedConstraints, postAction) =
        unifyConstraints paramGuidMapping a aConstraints bConstraints
      mSomeAGuid = fmap fst . Set.minView $ tcLambdaGuids aConstraints
      mReprGuid = fmap (`lookupDefToKey` paramGuidMapping) mSomeAGuid
    let
      mapUpdates =
        case mReprGuid of
        Nothing -> Map.empty
        Just reprGuid ->
          Map.fromList . map (flip (,) reprGuid) . Set.toList $ tcLambdaGuids bConstraints
    liftTypeContext $ State.modify
      ( atTcConstraints (UnionFind.setDescr a unifiedConstraints)
      . atTcParamGuidMap (Map.union mapUpdates)
      )
    postAction

lookupDefToKey :: Ord k => k -> Map k k -> k
lookupDefToKey key = fromMaybe key . Map.lookup key

unifyConstraints
  :: Monad m => Map Guid Guid -> Ref -> Constraints -> Constraints -> (Constraints, Infer m ())
unifyConstraints paramGuidMapping exprRef aConstraints bConstraints =
  (unifiedConstraints, postAction)
  where
    aExprs = tcExprs aConstraints
    unifiedConstraints = Constraints
      { tcExprs = newExprs
      , tcRules = on (++) tcRules aConstraints bConstraints
      , tcLambdaGuids = on Set.union tcLambdaGuids aConstraints bConstraints
      }
    postAction = do
      when isNewConflict $ onConflict =<< getActions
      applyRulesRef aConstraints bConstraints
      applyRulesRef bConstraints aConstraints
      recursivePostAction
    (newExprs, isNewConflict, recursivePostAction) =
      go aExprs (tcExprs bConstraints)
    go [] exprs = (exprs, False, return ())
    go (x : xs) ys =
      (final : otherExprs, isCon || not (null finalBadYs), finalAction >> xsAction)
      where
        (otherExprs, isCon, xsAction) = go xs finalBadYs
        (final, finalBadYs, finalAction) = foldl step (x, [], return ()) ys
        step (current, badYs, action) y =
          case unifyPair paramGuidMapping current y of
          Nothing -> (current, y : badYs, action)
          Just (expr, act) -> (expr, badYs, action >> act)
    applyRulesRef xConstraints yConstraints =
      sequence_ $
        liftA2 (applyRule (tcLambdaGuids yConstraints))
        (tcRules xConstraints) (tcExprs yConstraints)
    applyRule guids (UnifyRule arrowType destRef argRef) expr =
      case (arrowType, expr) of
      (ApplyFuncValue, Data.ExpressionLambda (Data.Lambda _ body)) ->
        unify destRef . frPoint =<< subst guids argRef body
      -- We now know our Apply parent is not a redex, unify the
      -- Apply into the value
      (ApplyFuncValue, _) ->
        unify destRef =<<
          makeSingletonRef []
          (Data.ExpressionApply (Data.Apply (fatten exprRef) (fatten argRef)))
      (ApplyFuncType, Data.ExpressionPi (Data.Lambda _ resultType)) ->
        unify destRef . frPoint =<< subst guids argRef resultType
      _ -> return ()

-- biased towards left child (if unifying Pis,
-- substs right child's guids to left)
unifyPair
  :: Monad m
  => Map Guid Guid
  -> Data.Expression FatRef
  -> Data.Expression FatRef
  -> Maybe (Data.Expression FatRef, Infer m ())
unifyPair paramGuidMapping aVal bVal =
  case (aVal, bVal) of
  (Data.ExpressionPi l1,
   Data.ExpressionPi l2) ->
    unifyLambdas Data.ExpressionPi l1 l2
  (Data.ExpressionLambda l1,
   Data.ExpressionLambda l2) ->
    unifyLambdas Data.ExpressionLambda l1 l2
  (Data.ExpressionApply (Data.Apply aFuncRef aArgRef),
   Data.ExpressionApply (Data.Apply bFuncRef bArgRef)) ->
    mkStructuralResult (fmap Data.ExpressionApply . Data.Apply)
    (aFuncRef, aArgRef) (bFuncRef, bArgRef)
  (Data.ExpressionBuiltin (Data.Builtin name1 _),
   Data.ExpressionBuiltin (Data.Builtin name2 _)) ->
    cond $ name1 == name2
  (Data.ExpressionGetVariable (Data.ParameterRef p0),
   Data.ExpressionGetVariable (Data.ParameterRef p1)) ->
    cond $ on (==) (`lookupDefToKey` paramGuidMapping) p0 p1
  (Data.ExpressionGetVariable (Data.DefinitionRef d0),
   Data.ExpressionGetVariable (Data.DefinitionRef d1)) ->
    cond $ d0 == d1
  (Data.ExpressionLiteralInteger i1,
   Data.ExpressionLiteralInteger i2) -> cond $ i1 == i2
  (Data.ExpressionSet,
   Data.ExpressionSet) -> good
  _ -> Nothing
  where
    mkStructuralResult cons (x0, x1) (y0, y1) =
      Just (cons r0 r1, u0 >> u1)
      where
        (r0, u0) = unifyFatRef x0 y0
        (r1, u1) = unifyFatRef x1 y1
    unifyFatRef (FatRef pointA fA) (FatRef pointB fB) =
      (FatRef pointA (fA || fB), unify pointA pointB)
    good = Just (aVal, return ())
    cond True = good
    cond False = Nothing
    unifyLambdas
      cons
      (Data.Lambda aParamRef aResultRef)
      (Data.Lambda bParamRef bResultRef) =
        mkStructuralResult (fmap cons . Data.Lambda)
        (aParamRef, aResultRef) (bParamRef, bResultRef)

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

-- TODO: This should not go through LoopGuidExpression and should
-- probably be in Data.Ops or such along with canonize*
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
  Data.ExpressionBuiltin (Data.Builtin name _) -> res name
  Data.ExpressionSet -> res $ Data.FFIName ["Core"] "Set"
  other -> other
  where
    res name =
      maybe expr Data.ExpressionGetVariable $ Map.lookup name builtinsMap

loadDefTypeToRef :: Monad m => Data.DefinitionIRef -> Infer m Ref
loadDefTypeToRef def =
  refFromPure =<< (`loadPureDefinitionType` def) =<< getActions

loadDefTypeWithinContext
  :: Monad m
  => Maybe (StoredDefinition (T f)) -> Data.DefinitionIRef -> Infer m Ref
loadDefTypeWithinContext Nothing = loadDefTypeToRef
loadDefTypeWithinContext (Just def) = getDefRef (deInferredType def) (deIRef def)

pureInferExpressionWithinContext
  :: Monad m
  => Scope
  -> Maybe (StoredDefinition (T f))
  -> Data.PureGuidExpression -> Infer m (Expression ())
pureInferExpressionWithinContext scope mDef pureExpr = do
  expr <- fromPure pureExpr
  inferExpression scope (loadDefTypeWithinContext mDef) expr
  return expr

getDefRef
  :: Monad m => Ref -> Data.DefinitionIRef -> Data.DefinitionIRef -> Infer m Ref
getDefRef defRef defI getDefI
  | getDefI == defI = return defRef
  | otherwise = loadDefTypeToRef getDefI

inferDefinition
  :: (Monad m, Monad f)
  => DataLoad.DefinitionEntity (T f)
  -> T m (StoredDefinition (T f))
inferDefinition (DataLoad.DefinitionEntity defI (Data.Definition bodyI typeI)) = do
  (typeContext, (bodyExpr, typeExpr)) <- runInfer inferActions $ do
    bodyExpr <- fromLoaded bodyI
    typeExpr <- fromLoaded typeI
    inferExpression Map.empty (getDefRef (eeInferredType bodyExpr) defI) bodyExpr
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
  liftM snd . runInfer inferActions $ do
    tExpr <- fromLoaded expr
    inferExpression Map.empty loadDefTypeToRef tExpr
    return tExpr
