{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell, DeriveFunctor, DeriveFoldable, DeriveTraversable, DeriveDataTypeable #-}
module Editor.Data.Infer
  ( Expression, Inferred(..), rExpression
  , Loaded, load, inferLoaded
  , updateAndInfer
  -- TODO: Expose only ref readers for InferNode (instead of .. and TypedValue)
  , InferNode(..), TypedValue(..)
  , Error(..), ErrorDetails(..)
  , RefMap, Context, ExprRef
  , Loader(..), InferActions(..)
  , initial
  -- Used for inferring independent expressions in an inner infer context
  -- (See hole apply forms).
  , newNodeWithScope
  , newTypedNodeWithScope
  ) where

import Control.Applicative (Applicative(..), (<$), (<$>))
import Control.Lens ((%=), (.=), (^.), (+=))
import Control.Monad (guard, liftM, liftM2, unless, void, when)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Either (EitherT(..))
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.Monad.Trans.State (StateT(..), State, runState, execState)
import Control.Monad.Trans.Writer (Writer)
import Data.Binary (Binary(..), getWord8, putWord8)
import Data.Derive.Binary (makeBinary)
import Data.DeriveTH (derive)
import Data.Foldable (Foldable(..))
import Data.Functor.Identity (Identity(..))
import Data.IntMap (IntMap)
import Data.IntSet (IntSet)
import Data.Map (Map)
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Monoid (Monoid(..))
import Data.Store.Guid (Guid)
import Data.Traversable (Traversable)
import Data.Typeable (Typeable)
import Editor.Data.Infer.Rules (Rule(..), makeAllRules, makeResumptionRules, runRuleClosure)
import Editor.Data.Infer.Types
import qualified Control.Lens as Lens
import qualified Control.Lens.TH as LensTH
import qualified Control.Monad.Trans.Either as Either
import qualified Control.Monad.Trans.Reader as Reader
import qualified Control.Monad.Trans.State as State
import qualified Data.Foldable as Foldable
import qualified Data.IntSet as IntSet
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Traversable as Traversable
import qualified Editor.Data as Data
import qualified Editor.Data.IRef as DataIRef

mkOrigin :: Monad m => StateT Origin m Origin
mkOrigin = do
  r <- State.get
  State.modify (+1)
  return r

-- Initial Pass:
-- Get Definitions' types expand.
-- Use expression's structures except for Apply.
--   (because an Apply can result in something else
--    but for example an Int or Lambda stays the same)
-- Add SimpleType, Union, LambdaOrPi, LambdaBodyType, Apply rules
-- Param types of Lambdas and Pis are of type Set
-- Pi result type is of type Set

-- When recursing on an expression, we remember the parent expression guids,
-- And we make sure not to add a sub-expression with a parent guid (that's a recursive structure).

data RefData = RefData
  { _rExpression :: RefExpression
  , _rRules :: [Int] -- Rule id
  }
derive makeBinary ''RefData

--------------
--- RefMap:
data RefMap a = RefMap
  { _refs :: IntMap a
  , _nextRef :: Int
  }
LensTH.makeLenses ''RefData
LensTH.makeLenses ''RefMap
derive makeBinary ''RefMap

createEmptyRef :: Monad m => StateT (RefMap a) m Int
createEmptyRef = do
  key <- Lens.use nextRef
  nextRef += 1
  return key

refsMAt :: Functor f => Int -> (Maybe a -> f (Maybe a)) -> RefMap a -> f (RefMap a)
refsMAt k = refs . Lens.at k

refsAt :: Functor f => Int -> (a -> f a) -> RefMap a -> f (RefMap a)
refsAt k = refsMAt k . Lens.iso from Just
  where
    from = fromMaybe $ error msg
    msg = unwords ["intMapMod: key", show k, "not in map"]
--------------

data Context = Context
  { _exprMap :: RefMap RefData
  , _nextOrigin :: Int
  , _rules :: IntMap Rule
  , _nextRule :: Int
  } deriving (Typeable)
derive makeBinary ''Context

data InferState = InferState
  { _sContext :: Context
  , _sBfsNextLayer :: IntSet
  , _sBfsCurLayer :: IntSet
  }

data Inferred a = Inferred
  { iStored :: a
  , iValue :: Data.Expression DataIRef.DefinitionIRef ()
  , iType :: Data.Expression DataIRef.DefinitionIRef ()
  , iScope :: Map Guid (Data.Expression DataIRef.DefinitionIRef ())
  , iPoint :: InferNode
  } deriving (Functor, Foldable, Traversable)
derive makeBinary ''Inferred

type Expression a = Data.Expression DataIRef.DefinitionIRef (Inferred a)

data ErrorDetails
  = MismatchIn
    (Data.Expression DataIRef.DefinitionIRef ())
    (Data.Expression DataIRef.DefinitionIRef ())
  | InfiniteExpression (Data.Expression DataIRef.DefinitionIRef ())
  deriving (Show, Eq, Ord)
derive makeBinary ''ErrorDetails

data Error = Error
  { errRef :: ExprRef
  , errMismatch ::
    ( Data.Expression DataIRef.DefinitionIRef ()
    , Data.Expression DataIRef.DefinitionIRef ()
    )
  , errDetails :: ErrorDetails
  } deriving (Show, Eq, Ord)
derive makeBinary ''Error

newtype InferActions m = InferActions
  { reportError :: Error -> m ()
  }

LensTH.makeLenses ''Context
LensTH.makeLenses ''InferState

-- ExprRefMap:

createEmptyRefExpr :: Monad m => StateT Context m ExprRef
createEmptyRefExpr = liftM ExprRef $ Lens.zoom exprMap createEmptyRef

exprRefsMAt ::
  Functor f => ExprRef -> (Maybe RefData -> f (Maybe RefData)) -> Context -> f Context
exprRefsMAt k = exprMap . refsMAt (unExprRef k)

exprRefsAt ::
  Functor f => ExprRef -> (RefData -> f RefData) -> Context -> f Context
exprRefsAt k = exprMap . refsAt (unExprRef k)

-------------

-- TODO: createTypeVal should use newNode, not vice versa.
-- For use in loading phase only!
-- We don't create additional Refs afterwards!
createTypedVal :: Monad m => StateT Context m TypedValue
createTypedVal = liftM2 TypedValue createEmptyRefExpr createEmptyRefExpr

newNodeWithScope :: Scope -> Context -> (Context, InferNode)
newNodeWithScope scope prevContext =
  (resultContext, InferNode tv scope)
  where
    (tv, resultContext) = runState createTypedVal prevContext

newTypedNodeWithScope :: Scope -> ExprRef -> Context -> (Context, InferNode)
newTypedNodeWithScope scope typ prevContext =
  (resultContext, InferNode (TypedValue newValRef typ) scope)
  where
    (newValRef, resultContext) = runState createEmptyRefExpr prevContext

initial :: (Context, InferNode)
initial =
  newNodeWithScope mempty $ Context
    { _exprMap = RefMap
      { _refs = mempty
      , _nextRef = 0
      }
    , _nextOrigin = 0
    , _rules = mempty
    , _nextRule = 0
    }

newtype Loader m = Loader
  { loadPureDefinitionType :: DataIRef.DefinitionIRef -> m (Data.Expression DataIRef.DefinitionIRef ())
  }

-- Initial expression for inferred value and type of a stored entity.
-- Types are returned only in cases of expanding definitions.
initialExprs ::
  Map DataIRef.DefinitionIRef (Data.Expression DataIRef.DefinitionIRef ()) ->
  Scope ->
  Data.Expression DataIRef.DefinitionIRef () ->
  State Origin
  ( Data.Expression DataIRef.DefinitionIRef Origin
  , Data.Expression DataIRef.DefinitionIRef Origin
  )
initialExprs defTypes scope entity =
  case entity ^. Data.eValue of
  Data.ExpressionApply _ ->
    (,) <$> mkHoleO <*> mkHoleO
  Data.ExpressionLeaf (Data.GetVariable var@(Data.DefinitionRef ref))
    | not (Map.member var scope) ->
      (,) <$> addOrigin entity <*> addOrigin (defTypes Map.! ref)
  _ -> (,)
    <$> addOrigin (Lens.over Data.eValue (Data.pureHole <$) entity)
    <*> mkHoleO
  where
    mkHoleO = addOrigin Data.pureHole
    addOrigin = Traversable.mapM (const mkOrigin)

-- This is because platform's Either's Monad instance sucks
runEither :: EitherT l Identity a -> Either l a
runEither = runIdentity . runEitherT

guardEither :: l -> Bool -> EitherT l Identity ()
guardEither err False = Either.left err
guardEither _ True = return ()

guidRepeat :: RefExpression -> Bool
guidRepeat =
  go Set.empty
  where
    go forbidden (Data.Expression body pl)
      | Set.member g forbidden = True
      | otherwise =
        Foldable.any (go (Set.insert g forbidden)) body
      where
        g = Lens.view rplOrigin pl

-- Merge two expressions:
-- If they do not match, return Nothing.
-- Holes match with anything, expand to the other expr.
-- Guids come from the first expression (where available).
-- If guids repeat, fail.
mergeExprs ::
  RefExpression ->
  RefExpression ->
  Either ErrorDetails RefExpression
mergeExprs p0 p1 =
  runEither $ do
    result <- Data.matchExpression onMatch onMismatch p0 p1
    guardEither (InfiniteExpression (void result)) . not $ guidRepeat result
    return result
  where
    addSubstituted addition =
      Lens.over rplSubstitutedArgs
      ((mappend . Lens.view rplSubstitutedArgs) addition)
    onMatch x y = return $ addSubstituted y x
    onMismatch (Data.Expression (Data.ExpressionLeaf Data.Hole) s0) e1 =
      return $ fmap (addSubstituted s0) e1
    onMismatch e0 (Data.Expression (Data.ExpressionLeaf Data.Hole) s1) =
      return $ fmap (addSubstituted s1) e0
    onMismatch e0 e1 =
      Either.left $ MismatchIn (void e0) (void e1)

initializeRefData :: ExprRef -> Data.Expression DataIRef.DefinitionIRef Origin -> State Context ()
initializeRefData ref expr =
  exprRefsAt ref .= RefData (fmap (RefExprPayload mempty) expr) []

exprIntoContext ::
  Map DataIRef.DefinitionIRef (Data.Expression DataIRef.DefinitionIRef ()) -> Scope ->
  Data.Expression DataIRef.DefinitionIRef s -> TypedValue ->
  State Context (Data.Expression DataIRef.DefinitionIRef (InferNode, s))
exprIntoContext defTypes rootScope rootExpr rootTypedValue = do
  rootExprTV <-
    Traversable.traverse tupleInto .
    -- Make new TypedValues for all subexpressions except the root
    -- which shall use rootTypedValue
    Lens.set (Data.ePayload . Lens._2) (pure rootTypedValue) $
    fmap addTypedVal rootExpr
  Data.recurseWithScope addToScope f rootScope rootExprTV
  where
    tupleInto (x, act) = (,) x <$> act
    addTypedVal x = (x, createTypedVal)
    addToScope paramGuid (_, TypedValue paramTypeVal _) =
      Map.insert (Data.ParameterRef paramGuid) paramTypeVal
    f scope expr@(Data.Expression _ (originS, typedValue)) = do
      let TypedValue val typ = typedValue
      (initialVal, initialType) <-
        Lens.zoom nextOrigin . initialExprs defTypes scope $
        void expr
      initializeRefData val initialVal
      initializeRefData typ initialType
      return (InferNode typedValue scope, originS)

data Loaded a = Loaded
  { lRealExpr :: Data.Expression DataIRef.DefinitionIRef a
  , lMRecursiveDef :: Maybe DataIRef.DefinitionIRef
  , lDefinitionTypes :: Map DataIRef.DefinitionIRef (Data.Expression DataIRef.DefinitionIRef ())
  } deriving (Typeable)
derive makeBinary ''Loaded

ordNub :: Ord a => [a] -> [a]
ordNub = Set.toList . Set.fromList

load ::
  Monad m => Loader m ->
  Maybe DataIRef.DefinitionIRef -> Data.Expression DataIRef.DefinitionIRef a ->
  m (Loaded a)
load loader mRecursiveDef expr =
  liftM (Loaded expr mRecursiveDef . Map.fromList) .
  mapM loadType $ ordNub
  [ defI
  | Data.ExpressionLeaf (Data.GetVariable (Data.DefinitionRef defI)) <-
    map (Lens.view Data.eValue) $ Data.subExpressions expr
  , Just defI /= mRecursiveDef
  ]
  where
    loadType defI = liftM ((,) defI) $ loadPureDefinitionType loader defI

-- TODO: Preprocessed used to be "Loaded". Now it is just a step in
-- "infer". Does it still make sense to keep it separately the way it
-- is?
data Preprocessed a = Preprocessed
  { lExpr :: Data.Expression DataIRef.DefinitionIRef (InferNode, a)
  , lContext :: Context
  , lSavedRoot :: (Maybe RefData, Maybe RefData)
  }

preprocess :: Loaded a -> Context -> InferNode -> Preprocessed a
preprocess loaded initialContext (InferNode rootTv rootScope) =
  buildPreprocessed . (`runState` initialContext) $
  exprIntoContext (lDefinitionTypes loaded) scope
  (lRealExpr loaded) rootTv
  where
    TypedValue rootValR rootTypR = rootTv
    initialMRefData k =
      Lens.view (exprRefsMAt k) initialContext
    buildPreprocessed (node, resultContext) = Preprocessed
      { lExpr = node
      , lContext = resultContext
      , lSavedRoot =
        ( initialMRefData rootValR
        , initialMRefData rootTypR
        )
      }
    scope =
      case lMRecursiveDef loaded of
      Nothing -> rootScope
      Just iref -> Map.insert (Data.DefinitionRef iref) (tvType rootTv) rootScope

postProcess ::
  Data.Expression DataIRef.DefinitionIRef (InferNode, a) -> InferState -> (Expression a, Context)
postProcess expr inferState =
  (fmap derefNode expr, resultContext)
  where
    resultContext = inferState ^. sContext
    derefNode (inferNode, s) =
      Inferred
      { iStored = s
      , iValue = deref . tvVal $ nRefs inferNode
      , iType = deref . tvType $ nRefs inferNode
      , iScope =
        Map.fromList . mapMaybe onScopeElement . Map.toList $ nScope inferNode
      , iPoint = inferNode
      }
    onScopeElement (Data.ParameterRef guid, ref) = Just (guid, deref ref)
    onScopeElement _ = Nothing
    deref ref = void $ resultContext ^. exprRefsAt ref . rExpression

addRule :: Rule -> State InferState ()
addRule rule = do
  ruleId <- makeRule
  mapM_ (addRuleId ruleId) $ ruleInputs rule
  sBfsNextLayer . Lens.contains ruleId .= True
  where
    makeRule = do
      ruleId <- Lens.use (sContext . nextRule)
      sContext . nextRule += 1
      sContext . rules . Lens.at ruleId .= Just rule
      return ruleId
    addRuleId ruleId ref = sContext . exprRefsAt ref . rRules %= (ruleId :)

--- InferT:

newtype InferT m a =
  InferT { unInferT :: ReaderT (InferActions m) (StateT InferState m) a }
  deriving (Monad)

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

runInferT ::
  Monad m => InferActions m -> InferState ->
  InferT m a -> m (InferState, a)
runInferT actions state =
  liftM swap . (`runStateT` state) . (`runReaderT` actions) . unInferT

liftActions :: ReaderT (InferActions m) (StateT InferState m) a -> InferT m a
liftActions = InferT

liftState :: Monad m => StateT InferState m a -> InferT m a
liftState = liftActions . lift

{-# SPECIALIZE liftState :: StateT InferState Maybe a -> InferT Maybe a #-}
{-# SPECIALIZE liftState :: Monoid w => StateT InferState (Writer w) a -> InferT (Writer w) a #-}

instance MonadTrans InferT where
  lift = liftState . lift

execInferT ::
  Monad m => InferActions m ->
  InferState -> Data.Expression DataIRef.DefinitionIRef (InferNode, a) ->
  InferT m () -> m (Expression a, Context)
execInferT actions state expr act =
  liftM (postProcess expr . fst) .
  runInferT actions state $ do
    act
    executeRules

{-# SPECIALIZE
  execInferT ::
    InferActions Maybe ->
    InferState -> Data.Expression DataIRef.DefinitionIRef (InferNode, a) ->
    InferT Maybe () -> Maybe (Expression a, Context) #-}
{-# SPECIALIZE
  execInferT ::
    Monoid w => InferActions (Writer w) ->
    InferState -> Data.Expression DataIRef.DefinitionIRef (InferNode, a) ->
    InferT (Writer w) () -> Writer w (Expression a, Context) #-}

updateAndInfer ::
  Monad m => InferActions m -> Context ->
  [(ExprRef, Data.Expression DataIRef.DefinitionIRef ())] ->
  Expression a -> m (Expression a, Context)
updateAndInfer actions prevContext updates expr =
  execInferT actions inferState (f <$> expr) $ do
    mapM_ doUpdate updates
  where
    inferState = InferState prevContext mempty mempty
    f inferred = (iPoint inferred, iStored inferred)
    doUpdate (ref, newExpr) =
      setRefExpr ref =<<
      (liftState . Lens.zoom (sContext . nextOrigin) . makeRefExprFromPure) newExpr
    makeRefExprFromPure =
      Traversable.mapM . const $ liftM (RefExprPayload mempty) mkOrigin

inferLoaded ::
  Monad m => InferActions m -> Loaded a -> Context -> InferNode ->
  m (Expression a, Context)
inferLoaded actions loaded initialContext node =
  execInferT actions ruleInferState expr $ do
    restoreRoot rootValR rootValMRefData
    restoreRoot rootTypR rootTypMRefData
  where
    Preprocessed expr loadedContext (rootValMRefData, rootTypMRefData) =
      preprocess loaded initialContext node
    ruleInferState =
      (`execState` InferState loadedContext mempty mempty) $
      mapM_ addRule =<<
      Lens.zoom (sContext . nextOrigin)
      (makeRules rootValMRefData (fmap fst expr))
    makeRules Nothing = makeAllRules
    makeRules (Just _) = makeResumptionRules
    TypedValue rootValR rootTypR = nRefs . fst $ expr ^. Data.ePayload
    restoreRoot _ Nothing = return ()
    restoreRoot ref (Just (RefData refExpr refRules)) = do
      liftState $ sContext . exprRefsAt ref . rRules %= (refRules ++)
      setRefExpr ref refExpr

{-# SPECIALIZE
  inferLoaded ::
    InferActions Maybe -> Loaded a -> Context -> InferNode ->
    Maybe (Expression a, Context) #-}
{-# SPECIALIZE
  inferLoaded ::
    Monoid w => InferActions (Writer w) -> Loaded a ->
    Context -> InferNode ->
    Writer w (Expression a, Context) #-}

executeRules :: Monad m => InferT m ()
executeRules = do
  curLayer <- liftState $ Lens.use sBfsNextLayer
  liftState $ sBfsCurLayer .= curLayer
  liftState $ sBfsNextLayer .= IntSet.empty
  unless (IntSet.null curLayer) $ do
    mapM_ processRule $ IntSet.toList curLayer
    executeRules
  where
    processRule key = do
      liftState $ sBfsCurLayer . Lens.contains key .= False
      Just (Rule deps ruleClosure) <-
        liftState $ Lens.use (sContext . rules . Lens.at key)
      refExps <- mapM getRefExpr deps
      mapM_ (uncurry setRefExpr) $ runRuleClosure ruleClosure refExps

{-# SPECIALIZE executeRules :: InferT Maybe () #-}
{-# SPECIALIZE executeRules :: Monoid w => InferT (Writer w) () #-}

getRefExpr :: Monad m => ExprRef -> InferT m RefExpression
getRefExpr ref = liftState $ Lens.use (sContext . exprRefsAt ref . rExpression)

{-# SPECIALIZE getRefExpr :: ExprRef -> InferT Maybe RefExpression #-}
{-# SPECIALIZE getRefExpr :: Monoid w => ExprRef -> InferT (Writer w) RefExpression #-}

setRefExpr :: Monad m => ExprRef -> RefExpression -> InferT m ()
setRefExpr ref newExpr = do
  curExpr <- liftState $ Lens.use (sContext . exprRefsAt ref . rExpression)
  case mergeExprs curExpr newExpr of
    Right mergedExpr -> do
      let
        isChange = not $ equiv mergedExpr curExpr
        isHole =
          case mergedExpr ^. Data.eValue of
          Data.ExpressionLeaf Data.Hole -> True
          _ -> False
      when isChange $ touch ref
      when (isChange || isHole) $
        liftState $ sContext . exprRefsAt ref . rExpression .= mergedExpr
    Left details -> do
      report <- liftActions $ Reader.asks reportError
      lift $ report Error
        { errRef = ref
        , errMismatch = (void curExpr, void newExpr)
        , errDetails = details
        }
  where
    equiv x y =
      isJust $
      Data.matchExpression compareSubsts ((const . const) Nothing) x y
    compareSubsts x y = guard $ (x ^. rplSubstitutedArgs) == (y ^. rplSubstitutedArgs)

{-# SPECIALIZE setRefExpr :: ExprRef -> RefExpression -> InferT Maybe () #-}
{-# SPECIALIZE setRefExpr :: Monoid w => ExprRef -> RefExpression -> InferT (Writer w) () #-}

touch :: Monad m => ExprRef -> InferT m ()
touch ref =
  liftState $ do
    nodeRules <- Lens.use (sContext . exprRefsAt ref . rRules)
    curLayer <- Lens.use sBfsCurLayer
    sBfsNextLayer %=
      ( mappend . IntSet.fromList
      . filter (not . (`IntSet.member` curLayer))
      ) nodeRules

{-# SPECIALIZE touch :: ExprRef -> InferT Maybe () #-}
{-# SPECIALIZE touch :: Monoid w => ExprRef -> InferT (Writer w) () #-}
