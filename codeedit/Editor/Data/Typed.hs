{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving #-}
module Editor.Data.Typed
  ( LoopGuidExpression(..)
  , pureGuidFromLoop

  , Expression(..), StoredExpression
  , pureInferExpressionWithinContext
  , eeInferredType, eeInferredValue, eeValue, eeRef, eeGuid
  , loadInferExpression, inferExpression
  , toPureExpression
  , alphaEq

  , StoredDefinition(..)
  , deIRef, deValue, deTypeContext, deInferredType
  , deRecursiveInferredType, deFinalTypeContext
  , deGuid
  , loadInferDefinition

  , Ref, Scope
  , TypeContext, emptyTypeContext
  , FinalTypeContext, finalizeTypeContext
  , Infer, resumeInfer
  , InferActions(..), onConflict
  , inferActions
  , unify, derefRef
  , makeSingletonRef
  ) where

import Control.Lens ((.~), (^.))
import Control.Monad (liftM, liftM2, (<=<), when, unless, forM_)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Random (nextRandom, runRandomT)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.Monad.Trans.State (StateT(..))
import Data.Function (on)
import Data.Functor.Identity (Identity(..))
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Store.Guid (Guid)
import Data.Store.Transaction (Transaction)
import Data.UnionFind.IntMap (UnionFind)
import Editor.Anchors (ViewTag)
import System.Random (RandomGen)
import qualified Control.Lens as Lens
import qualified Control.Lens.TH as LensTH
import qualified Control.Monad.Trans.Reader as Reader
import qualified Control.Monad.Trans.State as State
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

data SubstRule = SubstRule
  { _srFromGuids :: Set Guid
  , _srSubstitutedRefs :: [Ref]
  } deriving (Show, Eq)

data ApplyFuncRole = ApplyFuncType | ApplyFuncValue
  deriving (Show, Eq)

data UnifyRuleType = ApplyFuncRule ApplyFuncRole | CopySubst SubstRule
  deriving (Show, Eq)

data UnifyRule = UnifyRule
  { urType :: UnifyRuleType
  , urDest :: Ref
  , urArg :: Ref
  } deriving (Show)

data Constraints = Constraints
  { tcLambdaGuids :: Set Guid
  , tcExprs :: [Data.Expression Ref]
  , tcRules :: [UnifyRule]
  , tcSubstitutedRefs :: [Ref]
  }

instance Show Constraints where
  show (Constraints guids exprs rules substRefs) =
    unwords
    [ Set.toList guids >>= ((++ ":") . show)
    , show exprs
    , show rules
    , show substRefs
    ]

emptyConstraints :: Constraints
emptyConstraints = Constraints Set.empty [] [] []

type Ref = UnionFind.Point Constraints

data Expression s = Expression
  { _eeRef :: s
  , _eeGuid :: Guid
  , _eeInferredType :: Ref
  , _eeInferredValue :: Ref
  , _eeValue :: Data.Expression (Expression s)
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
  { _tcConstraints :: UnionFind Constraints
  , _tcParamGuidMap :: Map Guid Guid
  }
LensTH.makeLenses ''TypeContext

-- After the polymorphic recursion phase
newtype FinalTypeContext = FinalTypeContext {
  _finalTypeContext :: TypeContext
  }
-- LensTH.makeLenses ''FinalTypeContext

emptyTypeContext :: TypeContext
emptyTypeContext = TypeContext UnionFind.empty Map.empty

data StoredDefinition m = StoredDefinition
  { _deIRef :: Data.DefinitionIRef
  , _deInferredType :: Ref
  , _deRecursiveInferredType :: Ref
  , _deValue :: Data.Definition (StoredExpression m)
  , _deTypeContext :: TypeContext
  , _deFinalTypeContext :: FinalTypeContext
  }

LensTH.makeLenses ''Expression
LensTH.makeLenses ''StoredDefinition

deGuid :: StoredDefinition m -> Guid
deGuid = IRef.guid . Lens.view deIRef

--------------- Infer Stack boilerplate:

data InferActions m = InferActions
  { _onConflict :: m ()
  , _loadPureDefinitionBody ::
      Data.DefinitionIRef -> m Data.PureGuidExpression
  , _aGetDefTypeRef :: Data.DefinitionIRef -> Infer m Ref
  , _allowCopy :: Bool
  }

newtype Infer m a = Infer
  { _infer :: ReaderT (InferActions m) (StateT TypeContext m) a
  } deriving (Functor, Monad)

LensTH.makeLenses ''InferActions
LensTH.makeLenses ''Infer

inferActions ::
  (Monad m, Monad f) =>
  (T f Data.PureGuidExpression -> m Data.PureGuidExpression) ->
  InferActions m
inferActions liftTransaction = InferActions
  { _onConflict = return ()
  , _loadPureDefinitionBody =
    liftTransaction . DataLoad.loadPureDefinitionBody
  , _aGetDefTypeRef =
    refFromPure <=< lift . liftTransaction . DataLoad.loadPureDefinitionType
  , _allowCopy = True
  }

getDefBodyRef :: Monad m => Data.DefinitionIRef -> Infer m Ref
getDefBodyRef def =
  refFromPure =<< lift . (flip (Lens.view loadPureDefinitionBody) def) =<< getActions

getDefTypeRef :: Monad m => Data.DefinitionIRef -> Infer m Ref
getDefTypeRef def = flip (Lens.view aGetDefTypeRef) def =<< getActions

----------------- Infer operations:

liftActionsReader :: ReaderT (InferActions m) (StateT TypeContext m) a -> Infer m a
liftActionsReader = Infer

liftTypeContext :: Monad m => StateT TypeContext m a -> Infer m a
liftTypeContext = liftActionsReader . lift

instance MonadTrans Infer where
  lift = liftTypeContext . lift

getsConstraints :: Monad m => (UnionFind Constraints -> a) -> Infer m a
getsConstraints f = liftTypeContext $ State.gets (f . Lens.view tcConstraints)

getsParamGuidMap :: Monad m => (Map Guid Guid -> a) -> Infer m a
getsParamGuidMap f = liftTypeContext $ State.gets (f . Lens.view tcParamGuidMap)

makeRef :: Monad m => Constraints -> Infer m Ref
makeRef x =
  liftTypeContext $ StateT f
  where
    f state =
      return (result, tcConstraints .~ newConstraints $ state )
      where
        (result, newConstraints) = UnionFind.fresh x $ state ^. tcConstraints

getRef :: Monad m => Ref -> Infer m Constraints
getRef = getsConstraints . UnionFind.descr

getActions :: Monad m => Infer m (InferActions m)
getActions = liftActionsReader Reader.ask

localActions :: Monad m => (InferActions m -> InferActions m) -> Infer m a -> Infer m a
localActions = Lens.over infer . Reader.local

runInfer :: Monad m => InferActions m -> Infer m a -> m (TypeContext, a)
runInfer actions = resumeInfer actions emptyTypeContext

resumeInfer
  :: Monad m
  => InferActions m -> TypeContext -> Infer m a -> m (TypeContext, a)
resumeInfer actions typeContext =
  liftM Tuple.swap . (`runStateT` typeContext) . (`runReaderT` actions) . Lens.view infer

finalizeTypeContext :: Monad m => Maybe (Ref, Ref) -> InferActions m -> TypeContext -> m FinalTypeContext
finalizeTypeContext Nothing _ ctx = return (FinalTypeContext ctx)
finalizeTypeContext (Just (defTypeRef, recursiveDefTypeRef)) actions
  typeContext =
  liftM (FinalTypeContext . fst) .
  resumeInfer (Lens.set allowCopy False actions) typeContext $
  unify defTypeRef recursiveDefTypeRef

makeNoConstraints :: Monad m => Infer m Ref
makeNoConstraints = makeRef emptyConstraints

makeRuleRef :: Monad m => UnifyRule -> Infer m Ref
makeRuleRef rule = makeRef emptyConstraints { tcRules = [rule] }

makeSingletonRef :: Monad m => [Guid] -> Data.Expression Ref -> Infer m Ref
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
    lift . (flip (Lens.view loadPureDefinitionBody) defI) =<< getActions
  Data.ExpressionApply (Data.Apply func arg) ->
    liftM makePureGuidExpr $
    liftM2 Data.makeApply (expand func) (expand arg)
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
  Data.mapMExpression f <=< expand
  where
    f (Data.PureGuidExpression (Data.GuidExpression guid val)) =
      ( return val
      , makeSingletonRef [guid]
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
               getDefBodyRef ref
            -- Apply might be of lambda (can be detected much later)
            -- and in that case, it's a redex we want to peel off the
            -- Apply-of-Lambda wrapper. So we defer the insertion of
            -- the Apply into the constraints until later when it is
            -- known whether it is a redex or not.
            Data.ExpressionApply _ -> makeNoConstraints
            _ -> makeSingletonRef [g] $ fmap (Lens.view eeInferredValue) newVal
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
  -> FinalTypeContext -> Ref -> [LoopGuidExpression]
derefRef stdGen builtinsMap (FinalTypeContext typeContext) rootRef =
  (canonizeInferredExpression stdGen .
   map (builtinsToGlobals builtinsMap)) .
  (`runReaderT` Set.empty) $ go rootRef
  where
    constraintsUF = typeContext ^. tcConstraints
    mapGuid = flip lookupDefToKey $ typeContext ^. tcParamGuidMap
    canonizeInferredExpression =
      zipWith canonizeIdentifiers . RandomUtils.splits
    go ref = do
      visited <- Reader.ask
      if Set.member (UnionFind.repr ref constraintsUF) visited
        then return $ Loop zeroGuid
        else do
          let
            constraints = UnionFind.descr ref constraintsUF
            guid =
              case Set.toList (tcLambdaGuids constraints) of
              [] -> zeroGuid
              (g : _) -> mapGuid g
          expr <- lift $ tcExprs constraints
          liftM (NoLoop . Data.GuidExpression guid) .
            (Reader.local . Set.insert)
            (UnionFind.repr ref constraintsUF) $
            recurse expr
    recurse (Data.ExpressionGetVariable (Data.ParameterRef p)) =
      return . Data.ExpressionGetVariable . Data.ParameterRef $ mapGuid p
    recurse expr =
      Data.sequenceExpression $ fmap (Reader.mapReaderT holify . go) expr
    holify [] =
      [NoLoop
       (Data.GuidExpression zeroGuid Data.ExpressionHole)]
    holify xs = xs

type Scope = Map Guid Ref

inferExpression
  :: Monad m
  => Scope
  -> Expression s
  -> Infer m ()
inferExpression scope (Expression _ g typeRef valueRef value) =
  case value of
  Data.ExpressionLambda lambda@(Data.Lambda paramType body) -> do
    onLambda lambda
    -- We use "flip unify typeRef" so that the new Pi will be
    -- the official Pi guid due to the "left-bias" in
    -- unify/unifyPair. Thus we can later assume that we got the
    -- same guid in the pi and the lambda.
    flip unify typeRef <=< makeSingletonRef [g] .
      Data.makePi (paramType ^. eeInferredValue) $ body ^. eeInferredType
  Data.ExpressionPi lambda@(Data.Lambda _ resultType) -> do
    onLambda lambda
    -- TODO: Is Set:Set a good idea? Agda uses "eeInferredType
    -- resultType" as the type
    setType =<< mkSet
    unify (resultType ^. eeInferredType) =<< mkSet
  Data.ExpressionApply (Data.Apply func arg) -> do
    go [] func
    go [] arg
    let
      funcValRef = func ^. eeInferredValue
      funcTypeRef = func ^. eeInferredType
      argValRef = arg ^. eeInferredValue
      argTypeRef = arg ^. eeInferredType
    -- We give the new Pi the same Guid as the Apply. This is
    -- fine because Apply's Guid is meaningless and the
    -- canonization will fix it later anyway.
    unify funcTypeRef =<<
      makeSingletonRef [g] . Data.makePi argTypeRef =<< makeNoConstraints
    unify funcTypeRef =<<
      makeRuleRef UnifyRule
        { urType = ApplyFuncRule ApplyFuncType
        , urDest = typeRef
        , urArg = argValRef
        }
    unify funcValRef =<<
      makeRuleRef UnifyRule
        { urType = ApplyFuncRule ApplyFuncValue
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
    setType =<< getDefTypeRef defI
  Data.ExpressionLiteralInteger _ ->
    setType =<< mkBuiltin ["Prelude"] "Integer"
  Data.ExpressionBuiltin (Data.Builtin _ bType) ->
    setType =<< refFromPure (toPureExpression bType)
  _ -> return ()
  where
    mkSet = makeSingletonRef [] Data.ExpressionSet
    mkBuiltin path name =
      makeSingletonRef [] . Data.ExpressionBuiltin .
      Data.Builtin (Data.FFIName path name) =<<
      makeNoConstraints
    setType = unify typeRef
    go newVars = inferExpression $ Map.fromList newVars `Map.union` scope
    onLambda (Data.Lambda paramType result) = do
      go [] paramType
      go [(g, paramType ^. eeInferredValue)] result
      unify (paramType ^. eeInferredType) =<< mkSet

unify :: Monad m => Ref -> Ref -> Infer m ()
unify a b = do
  e <- getsConstraints $ UnionFind.equivalent a b
  unless e $ do
    -- Mark as unified immediately so recursive unifications hitting
    -- this pair will know it's being done...
    aConstraints <- getRef a
    bConstraints <- getRef b
    liftTypeContext . State.modify . Lens.over tcConstraints $ a `UnionFind.union` b
    paramGuidMapping <- getsParamGuidMap id
    let
      (unifiedConstraints, postAction) =
        unifyConstraints paramGuidMapping a aConstraints bConstraints
      mSomeAGuid = fmap fst . Set.minView $ tcLambdaGuids aConstraints
      mReprGuid = fmap (`lookupDefToKey` paramGuidMapping) mSomeAGuid
      mapUpdates =
        case mReprGuid of
        Nothing -> Map.empty
        Just reprGuid ->
          Map.fromList . map (flip (,) reprGuid) . Set.toList $
          tcLambdaGuids bConstraints
    liftTypeContext $ State.modify
      ( Lens.over tcConstraints (UnionFind.setDescr a unifiedConstraints)
      . Lens.over tcParamGuidMap (Map.union mapUpdates)
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
      , tcSubstitutedRefs = on (++) tcSubstitutedRefs aConstraints bConstraints
      }
    postAction = do
      when isNewConflict $ lift . Lens.view onConflict =<< getActions
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
      mapM_ (applyRule yConstraints) (tcRules xConstraints)
    applyRule exprConstraints (UnifyRule ruleType destRef argRef) =
      case ruleType of
      CopySubst subst ->
        applySubstRule subst exprRef exprConstraints destRef argRef
      ApplyFuncRule role ->
        forM_ (tcExprs exprConstraints) $ \expr ->
          case (role, expr) of
          (ApplyFuncValue, Data.ExpressionLambda (Data.Lambda _ body)) ->
            -- TODO: Remove this rule
            addSubstRule body destRef
            (SubstRule (tcLambdaGuids exprConstraints) (tcSubstitutedRefs exprConstraints))
            argRef
          -- We now know our Apply parent is not a redex, unify the
          -- Apply into the value
          (ApplyFuncValue, _) ->
            unify destRef =<<
              makeSingletonRef []
              (Data.makeApply exprRef argRef)
          (ApplyFuncType, Data.ExpressionPi (Data.Lambda _ resultType)) ->
            -- TODO: Remove this rule
            addSubstRule resultType destRef
            (SubstRule (tcLambdaGuids exprConstraints) (tcSubstitutedRefs exprConstraints))
            argRef
          _ -> return ()

addSubstRule ::
  Monad m =>
  Ref -> Ref -> SubstRule -> Ref ->
  Infer m ()
addSubstRule ruleNode copyDest rule substArg =
  unify ruleNode =<< makeRef emptyConstraints
  { tcRules = [UnifyRule
    { urType = CopySubst rule
    , urArg = substArg
    , urDest = copyDest
    }
  ]}

applySubstRule ::
  Monad m =>
  SubstRule -> Ref -> Constraints -> Ref -> Ref ->
  Infer m ()
applySubstRule (SubstRule fromGuids substitutedRefs) srcRef srcConstraints copyDest substArg = do
  isSame <- getsConstraints $ UnionFind.equivalent srcRef copyDest
  unless isSame $ do
    isSubstituted <- liftM or $ mapM (getsConstraints . UnionFind.equivalent srcRef) substitutedRefs
    actions <- getActions
    if isSubstituted || not (Lens.view allowCopy actions)
      then unify copyDest srcRef
      else mapM_ (unify copyDest <=< processExpr) (tcExprs srcConstraints)
  where
    processExpr (Data.ExpressionGetVariable (Data.ParameterRef p))
      | Set.member p fromGuids = return substArg
    processExpr (Data.ExpressionPi (Data.Lambda paramType resultType)) =
      makeResult Data.makePi paramType resultType
    processExpr (Data.ExpressionLambda (Data.Lambda paramType result)) =
      makeResult Data.makeLambda paramType result
    processExpr (Data.ExpressionApply (Data.Apply func arg)) =
      makeResult Data.makeApply func arg
    processExpr expr = makeNode expr -- Leaf Node
    makeResult cons x y = do
      makeNode =<< on (liftM2 cons) makeDstChild x y
    makeNode expr =
      makeRef emptyConstraints
      { tcExprs = [expr]
      , tcSubstitutedRefs = (substArg : substitutedRefs)
      , tcLambdaGuids = tcLambdaGuids srcConstraints
      }
    makeDstChild src = do
      dst <- makeNoConstraints
      addSubstRule src dst (SubstRule fromGuids (tcSubstitutedRefs srcConstraints)) substArg
      return dst

-- biased towards left child (if unifying Pis,
-- substs right child's guids to left)
unifyPair
  :: Monad m
  => Map Guid Guid
  -> Data.Expression Ref
  -> Data.Expression Ref
  -> Maybe (Data.Expression Ref, Infer m ())
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
    mkStructuralResult Data.makeApply
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
      Just (cons x0 x1, unify x0 y0 >> unify x1 y1)
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
          liftM2 Data.makeApply (f func) (f arg)
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

addRecursiveDefRefGetter ::
  Monad m => Data.DefinitionIRef -> Ref -> Infer m a -> Infer m a
addRecursiveDefRefGetter defI =
  localActions . Lens.over aGetDefTypeRef .
  overrideInput defI . return

pureInferExpressionWithinContext
  :: Monad m
  => Scope
  -> Maybe (StoredDefinition (T f))
  -> Data.PureGuidExpression -> Infer m (Expression ())
pureInferExpressionWithinContext scope mDef pureExpr = do
  expr <- fromPure pureExpr
  loadDefTypeWithinContext mDef $ inferExpression scope expr
  return expr
  where
    loadDefTypeWithinContext Nothing = id
    loadDefTypeWithinContext (Just def) =
      addRecursiveDefRefGetter (def ^. deIRef) (def ^. deRecursiveInferredType)

overrideInput
  :: Eq a => a -> b -> (a -> b) -> a -> b
overrideInput k v f x
  | x == k = v
  | otherwise = f x

inferDefinition
  :: (Monad m, Monad f)
  => DataLoad.DefinitionEntity (T f)
  -> T m (StoredDefinition (T f))
inferDefinition (DataLoad.DefinitionEntity defI (Data.Definition bodyI typeI)) = do
  (typeContext, (bodyExpr, typeExpr, recursiveDefTypeRef)) <-
    runInfer (inferActions id) $ do
      bodyExpr <- fromLoaded bodyI
      typeExpr <- fromLoaded typeI
      -- Will be handled by finalizeTypeContext
      recursiveDefTypeRef <- makeNoConstraints
      addRecursiveDefRefGetter defI recursiveDefTypeRef $
        inferExpression Map.empty bodyExpr
      return (bodyExpr, typeExpr, recursiveDefTypeRef)
  let inferredType = bodyExpr ^. eeInferredType
  finalTypeContext <-
    finalizeTypeContext (Just (inferredType, recursiveDefTypeRef))
    (inferActions id) typeContext
  return StoredDefinition
    { _deIRef = defI
    , _deInferredType = inferredType
    , _deRecursiveInferredType = recursiveDefTypeRef
    , _deValue = Data.Definition bodyExpr typeExpr
    , _deTypeContext = typeContext
    , _deFinalTypeContext = finalTypeContext
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
  liftM snd . runInfer (inferActions id) $ do
    tExpr <- fromLoaded expr
    inferExpression Map.empty tExpr
    return tExpr
