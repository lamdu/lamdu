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

  , Ref
  , TypeContext, emptyTypeContext
  , Infer, resumeInfer
  , InferActions(..), inferActions, liftInferActions
  , unify, derefRef
  , makeSingletonRef
  ) where

import Control.Applicative (Applicative, liftA2)
import Control.Arrow (second)
import Control.Monad (liftM, liftM2, (<=<), when, unless)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Random (nextRandom, runRandomT)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.Monad.Trans.State (execStateT)
import Control.Monad.Trans.UnionFind (UnionFindT, resumeUnionFindT)
import Data.Either (partitionEithers)
import Data.Function (on)
import Data.Functor.Identity (Identity(..))
import Data.Maybe (mapMaybe)
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
  { tcExprs :: [Data.GuidExpression Ref]
  , tcRules :: [UnifyRule]
  , tcForceMonomorphic :: Bool
  } deriving (Show)

emptyConstraints :: Constraints
emptyConstraints = Constraints [] [] False

type Ref = UnionFindT.Point Constraints

data Expression s = Expression
  { eeRef :: s
  , eeGuid :: Guid
  , eeInferredType :: Ref
  , eeInferredValue :: Ref
  , eeValue :: Data.Expression (Expression s)
  }

type StoredExpression f = Expression (Data.ExpressionIRefProperty f)

data LoopGuidExpression
  = NoLoop (Data.GuidExpression LoopGuidExpression)
  | Loop Guid
  deriving (Show, Eq)

type TypeContext = UnionFind Constraints

emptyTypeContext :: TypeContext
emptyTypeContext = newUnionFind

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
  { unInfer :: ReaderT (InferActions m) (UnionFindT Constraints m) a
  } deriving (Functor, Applicative, Monad)
AtFieldTH.make ''Infer

----------------- Infer operations:

liftConflictActionReader :: ReaderT (InferActions m) (UnionFindT Constraints m) a -> Infer m a
liftConflictActionReader = Infer

liftUnionFind :: Monad m => UnionFindT Constraints m a -> Infer m a
liftUnionFind = liftConflictActionReader . lift

instance MonadTrans Infer where
  lift = liftUnionFind . lift

makeRef :: Monad m => Constraints -> Infer m Ref
makeRef = liftUnionFind . UnionFindT.new

getRef :: Monad m => Ref -> Infer m Constraints
getRef = liftUnionFind . UnionFindT.descr

getActions :: Monad m => Infer m (InferActions (Infer m))
getActions = liftConflictActionReader $ Reader.asks liftInferActions

runInfer :: Monad m => InferActions m -> Infer m a -> m (TypeContext, a)
runInfer actions = resumeInfer actions emptyTypeContext

resumeInfer
  :: Monad m => InferActions m -> TypeContext -> Infer m a -> m (TypeContext, a)
resumeInfer actions typeContext =
  resumeUnionFindT typeContext . (`runReaderT` actions) . unInfer

makeNoConstraints :: Monad m => Infer m Ref
makeNoConstraints = makeRef emptyConstraints

makeRuleRef :: Monad m => UnifyRule -> Infer m Ref
makeRuleRef rule = makeRef emptyConstraints { tcRules = [rule] }

makeSingletonRef :: Monad m => Guid -> Data.Expression Ref -> Infer m Ref
makeSingletonRef _ Data.ExpressionHole = makeNoConstraints
makeSingletonRef guid expr =
  makeRef emptyConstraints
  { tcExprs = [Data.GuidExpression guid expr] }

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
  Data.ExpressionLambda (Data.Lambda paramType body) ->
    recurseLambda Data.ExpressionLambda paramType body
  Data.ExpressionPi (Data.Lambda paramType body) -> do
    newParamType <- expand paramType
    recurseLambda Data.ExpressionPi newParamType body
  _ -> return e
  where
    makePureGuidExpr =
      Data.PureGuidExpression . Data.GuidExpression guid
    recurseLambda cons paramType =
      liftM (makePureGuidExpr . cons . Data.Lambda paramType) .
      expand

refFromPure :: Monad m => Data.PureGuidExpression -> Infer m Ref
refFromPure =
  Data.mapMExpression f <=< expand
  where
    f (Data.PureGuidExpression (Data.GuidExpression guid val)) =
      ( return val
      , makeSingletonRef guid
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
            _ -> makeSingletonRef g $ fmap eeInferredValue newVal
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
  (`runReaderT` []) $ go rootRef
  where
    canonizeInferredExpression = zipWith canonizeIdentifiers . RandomUtils.splits
    go ref = do
      visited <- Reader.ask
      if any (UnionFind.equivalent typeContext ref) visited
        then return $ Loop zeroGuid
        else
        onType ref <=<
        lift . tcExprs $
        UnionFind.descr typeContext ref
    onType ref (Data.GuidExpression guid expr) =
      liftM (NoLoop . Data.GuidExpression guid) .
      Data.sequenceExpression $ fmap recurse expr
      where
        recurse =
          Reader.mapReaderT holify .
          Reader.local (ref :) .
          go
        holify [] =
          [NoLoop
           (Data.GuidExpression zeroGuid Data.ExpressionHole)]
        holify xs = xs

inferExpression
  :: Monad m
  => [(Guid, Ref)]
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
      Data.Lambda (eeInferredValue paramType) $ eeInferredType body
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
      makePi g . Data.Lambda argTypeRef =<< makeNoConstraints
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
    case lookup guid scope of
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
    mkSet = mkBuiltin ["Core"] "Set"
    mkBuiltin path name =
      makeSingletonRef zeroGuid . Data.ExpressionBuiltin .
      Data.Builtin (Data.FFIName path name) =<<
      makeNoConstraints
    makePi guid = makeSingletonRef guid . Data.ExpressionPi
    setType = unify typeRef
    go newVars = inferExpression (newVars ++ scope) defRef
    onLambda (Data.Lambda paramType result) = do
      go [] paramType
      go [(g, eeInferredValue paramType)] result
      unify (eeInferredType paramType) =<< mkSet

-- New Ref tree has tcRules=[] everywhere
dupRefTreeExprs :: Monad m => Ref -> Infer m Ref
dupRefTreeExprs ref = do
  constraints <- getRef ref
  if tcForceMonomorphic constraints
    then return ref
    else do
      let origExprs = tcExprs constraints
      exprs <- mapM recurse origExprs
      makeRef emptyConstraints { tcExprs = exprs }
  where
    recurse (Data.GuidExpression guid expr) =
      liftM (Data.GuidExpression guid) .
      Data.sequenceExpression $ fmap dupRefTreeExprs expr

unify
  :: Monad m
  => Ref
  -> Ref
  -> Infer m ()
unify a b = do
  e <- liftUnionFind $ UnionFindT.equivalent a b
  unless e $ do
    -- Mark as unified immediately so recursive unifications hitting
    -- this pair will know it's being done...
    aConstraints <- getRef a
    bConstraints <- getRef b
    liftUnionFind $ a `UnionFindT.union` b
    let
      (unifiedConstraints, postAction) =
        unifyConstraints a aConstraints bConstraints
    liftUnionFind $ UnionFindT.setDescr a unifiedConstraints
    postAction

unifyConstraints
  :: Monad m => Ref -> Constraints -> Constraints -> (Constraints, Infer m ())
unifyConstraints exprRef aConstraints bConstraints =
  (unifiedConstraints, postAction)
  where
    aExprs = tcExprs aConstraints
    unifiedConstraints = Constraints
      { tcExprs = aExprs ++ bUnunified
      , tcRules = tcRules aConstraints ++ tcRules bConstraints
      , tcForceMonomorphic =
        on (||) tcForceMonomorphic aConstraints bConstraints
      }
    postAction = do
      when (all (not . null) [aExprs, bUnunified]) $ onConflict =<< getActions
      applyRulesRef aConstraints bConstraints
      applyRulesRef bConstraints aConstraints
      recursivePostAction
    (bUnunified, recursivePostAction) =
      second sequence_ . partitionEithers .
      map (matches aExprs) $ tcExprs bConstraints
    matches as y = f y $ mapMaybe (`unifyPair` y) as
    f y [] = Left y -- Keep y as a conflict
    f _ xs = Right $ sequence_ xs -- y matched existing stuff
    applyRulesRef xConstraints yConstraints =
      sequence_ $
        liftA2 applyRule
        (tcRules xConstraints) (tcExprs yConstraints)
    applyRule
      (UnifyRule arrowType destRef argRef)
      (Data.GuidExpression guid expr) =
        case (arrowType, expr) of
        (ApplyFuncValue, Data.ExpressionLambda (Data.Lambda _ body)) -> do
          subst guid (return argRef) body
          unify body destRef
        -- We now know our Apply parent is not a redex, unify the
        -- Apply into the value
        (ApplyFuncValue, _) ->
          unify destRef =<<
            makeSingletonRef zeroGuid
            (Data.ExpressionApply (Data.Apply exprRef argRef))
        (ApplyFuncType, Data.ExpressionPi (Data.Lambda _ resultType)) -> do
          newResultType <- dupRefTreeExprs resultType
          unify argRef =<<
            makeRef emptyConstraints { tcForceMonomorphic = True }
          subst guid (return argRef) newResultType
          unify destRef newResultType
        _ -> return ()

-- biased towards left child (if unifying Pis,
-- substs right child's guids to left)
unifyPair
  :: Monad m
  => Data.GuidExpression Ref
  -> Data.GuidExpression Ref
  -> Maybe (Infer m ())
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
    (Data.ExpressionApply (Data.Apply aFuncRef aArgRef),
     Data.ExpressionApply (Data.Apply bFuncRef bArgRef)) -> Just $ do
      unify aFuncRef bFuncRef
      unify aArgRef bArgRef
    (Data.ExpressionBuiltin (Data.Builtin name1 _),
     Data.ExpressionBuiltin (Data.Builtin name2 _)) ->
      cond $ name1 == name2
    (Data.ExpressionGetVariable v1,
     Data.ExpressionGetVariable v2) -> cond $ v1 == v2
    (Data.ExpressionLiteralInteger i1,
     Data.ExpressionLiteralInteger i2) -> cond $ i1 == i2
    (Data.ExpressionMagic,
     Data.ExpressionMagic) -> good
    _ -> Nothing
  where
    good = Just $ return ()
    cond True = good
    cond False = Nothing
    unifyLambdas
      (Data.Lambda aParamRef aResultRef)
      (Data.Lambda bParamRef bResultRef) = Just $ do
      unify aParamRef bParamRef
      -- Remap b's guid to a's guid and return a as the unification:
      let
        mkGetAGuidRef =
          makeSingletonRef zeroGuid . Data.ExpressionGetVariable $
          Data.ParameterRef aGuid
      subst bGuid mkGetAGuidRef bResultRef
      unify aResultRef bResultRef

subst :: Monad m => Guid -> Infer m Ref -> Ref -> Infer m ()
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
    replace ref = do
      oldConstraints <- getRef ref
      let
        (Any removed, exprsWOGetVar) =
          mconcat . map removeFrom $ tcExprs oldConstraints
      when removed $ do
        liftUnionFind . UnionFindT.setDescr ref $
          oldConstraints { tcExprs = exprsWOGetVar }
        unify ref =<< mkTo

allUnder :: Monad m => Ref -> Infer m [Ref]
allUnder =
  (`execStateT` []) . recurse
  where
    recurse ref = do
      visited <- State.get
      alreadySeen <-
        liftM or . lift . liftUnionFind $
        mapM (UnionFindT.equivalent ref) visited
      unless alreadySeen $ do
        State.modify (ref :)
        types <- liftM tcExprs . lift $ getRef ref
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
  builtin@(Data.ExpressionBuiltin (Data.Builtin name _)) ->
    (maybe builtin Data.ExpressionGetVariable . Map.lookup name)
    builtinsMap
  other -> other

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
  => [(Guid, Ref)]
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
    inferExpression [] (getDefRef (eeInferredType bodyExpr) defI) bodyExpr
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
    inferExpression [] loadDefTypeToRef tExpr
    return tExpr
