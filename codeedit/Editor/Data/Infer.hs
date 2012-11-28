{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell, DeriveFunctor, DeriveFoldable, DeriveTraversable, DeriveDataTypeable #-}
module Editor.Data.Infer
  ( Expression, Inferred(..), rExpression
  , Loaded, load
  , IsNewRoot(..), inferLoaded
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
import Control.Monad.Trans.State (StateT(..), State, runState)
import Control.Monad.Trans.Writer (Writer)
import Data.Binary (Binary(..), getWord8, putWord8)
import Data.Derive.Binary (makeBinary)
import Data.DeriveTH (derive)
import Data.Foldable (Foldable(..))
import Data.Function (on)
import Data.Functor.Identity (Identity(..))
import Data.IntMap (IntMap)
import Data.IntSet (IntSet)
import Data.Map (Map)
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Monoid (Monoid(..))
import Data.Store.Guid (Guid)
import Data.Traversable (Traversable)
import Data.Tuple (swap)
import Data.Typeable (Typeable)
import Editor.Data.Infer.Rules (Rule(..), makeAllRules, makeResumptionRules, runRuleClosure, unionRules)
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

toStateT :: Monad m => State s a -> StateT s m a
toStateT = State.mapStateT (return . runIdentity)

mkOrigin :: State Origin Origin
mkOrigin = do
  r <- State.get
  State.modify (+1)
  return r

newtype RuleRef = RuleRef { unRuleRef :: Int }
derive makeBinary ''RuleRef
instance Show RuleRef where
  show = ('E' :) . show . unRuleRef

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
  , _rRules :: [RuleRef] -- Rule id
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

emptyRefMap :: RefMap a
emptyRefMap =
  RefMap
  { _refs = mempty
  , _nextRef = 0
  }

createEmptyRef :: State (RefMap a) Int
createEmptyRef = do
  key <- Lens.use nextRef
  nextRef += 1
  return key

refsAt :: Functor f => Int -> (a -> f a) -> RefMap a -> f (RefMap a)
refsAt k = refs . Lens.at k . Lens.iso from Just
  where
    from = fromMaybe $ error msg
    msg = unwords ["intMapMod: key", show k, "not in map"]

createRef :: a -> State (RefMap a) Int
createRef initialVal = do
  ref <- createEmptyRef
  refsAt ref .= initialVal
  return ref
--------------

data Context = Context
  { _exprMap :: RefMap RefData
  , _nextOrigin :: Int
  , _ruleMap :: RefMap Rule
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

createRefExpr :: State Context ExprRef
createRefExpr = do
  holeRefExpr <-
    liftM (Data.Expression Data.hole . RefExprPayload mempty) $
    Lens.zoom nextOrigin mkOrigin
  liftM ExprRef . Lens.zoom exprMap . createRef $
    RefData holeRefExpr mempty

exprRefsAt ::
  Functor f => ExprRef -> (RefData -> f RefData) -> Context -> f Context
exprRefsAt k = exprMap . refsAt (unExprRef k)

-- RuleRefMap

createEmptyRefRule :: State Context RuleRef
createEmptyRefRule = liftM RuleRef $ Lens.zoom ruleMap createEmptyRef

ruleRefsAt ::
  Functor f => RuleRef -> (Rule -> f Rule) -> Context -> f Context
ruleRefsAt k = ruleMap . refsAt (unRuleRef k)

-------------

-- TODO: createTypeVal should use newNode, not vice versa.
-- For use in loading phase only!
-- We don't create additional Refs afterwards!
createTypedVal :: State Context TypedValue
createTypedVal = TypedValue <$> createRefExpr <*> createRefExpr

newNodeWithScope :: Scope -> Context -> (Context, InferNode)
newNodeWithScope scope prevContext =
  (resultContext, InferNode tv scope)
  where
    (tv, resultContext) = runState createTypedVal prevContext

newTypedNodeWithScope :: Scope -> ExprRef -> Context -> (Context, InferNode)
newTypedNodeWithScope scope typ prevContext =
  (resultContext, InferNode (TypedValue newValRef typ) scope)
  where
    (newValRef, resultContext) = runState createRefExpr prevContext

initial :: Maybe DataIRef.DefinitionIRef -> (Context, InferNode)
initial mRecursiveDefI =
  (context, res)
  where
    (res, context) =
      (`runState` emptyContext) $ do
        rootTv <- createTypedVal
        let
          scope =
            case mRecursiveDefI of
            Nothing -> mempty
            Just recursiveDefI ->
              Map.singleton (Data.DefinitionRef recursiveDefI) (tvType rootTv)
        return $ InferNode rootTv scope
    emptyContext =
      Context
      { _exprMap = emptyRefMap
      , _nextOrigin = 0
      , _ruleMap = emptyRefMap
      }

--- InferT:

newtype InferT m a =
  InferT { unInferT :: ReaderT (InferActions m) (StateT InferState m) a }
  deriving (Monad)

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

postProcess ::
  (InferState, Data.Expression DataIRef.DefinitionIRef (InferNode, a)) -> (Expression a, Context)
postProcess (inferState, expr) =
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

getRefExpr :: Monad m => ExprRef -> InferT m RefExpression
getRefExpr ref = liftState $ Lens.use (sContext . exprRefsAt ref . rExpression)

{-# SPECIALIZE getRefExpr :: ExprRef -> InferT Maybe RefExpression #-}
{-# SPECIALIZE getRefExpr :: Monoid w => ExprRef -> InferT (Writer w) RefExpression #-}

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
      Rule deps ruleClosure <-
        liftState $ Lens.use (sContext . ruleRefsAt (RuleRef key))
      refExps <- mapM getRefExpr deps
      mapM_ (uncurry setRefExpr) $ runRuleClosure ruleClosure refExps

{-# SPECIALIZE executeRules :: InferT Maybe () #-}
{-# SPECIALIZE executeRules :: Monoid w => InferT (Writer w) () #-}

execInferT ::
  Monad m => InferActions m -> InferState ->
  InferT m (Data.Expression DataIRef.DefinitionIRef (InferNode, a)) ->
  m (Expression a, Context)
execInferT actions state act =
  liftM postProcess .
  runInferT actions state $ do
    res <- act
    executeRules
    return res

{-# SPECIALIZE
  execInferT ::
    InferActions Maybe -> InferState ->
    InferT Maybe (Data.Expression DataIRef.DefinitionIRef (InferNode, a)) ->
    Maybe (Expression a, Context)
  #-}

{-# SPECIALIZE
  execInferT ::
    Monoid w => InferActions (Writer w) -> InferState ->
    InferT (Writer w) (Data.Expression DataIRef.DefinitionIRef (InferNode, a)) ->
    (Writer w) (Expression a, Context)
  #-}

newtype Loader m = Loader
  { loadPureDefinitionType :: DataIRef.DefinitionIRef -> m (Data.Expression DataIRef.DefinitionIRef ())
  }

-- Initial expression for inferred value and type of a stored entity.
-- Types are returned only in cases of expanding definitions.
initialExprs ::
  Map DataIRef.DefinitionIRef (Data.Expression DataIRef.DefinitionIRef ()) ->
  Scope -> Data.Expression DataIRef.DefinitionIRef () ->
  State Origin
  ( Data.Expression DataIRef.DefinitionIRef Origin
  , Data.Expression DataIRef.DefinitionIRef Origin
  )
initialExprs defTypes scope entity =
  case entity ^. Data.eValue of
  Data.ExpressionApply _ ->
    liftM2 (,) mkHoleO mkHoleO
  Data.ExpressionLeaf (Data.GetVariable var@(Data.DefinitionRef ref))
    | not (Map.member var scope) ->
      liftM2 (,) (addOrigin entity) (addOrigin (defTypes Map.! ref))
  _ -> liftM2 (,) (addOrigin (Lens.over Data.eValue (Data.pureHole <$) entity)) mkHoleO
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

touch :: Monad m => ExprRef -> InferT m ()
touch ref =
  liftState $ do
    nodeRules <- Lens.use (sContext . exprRefsAt ref . rRules)
    curLayer <- Lens.use sBfsCurLayer
    sBfsNextLayer %=
      ( mappend . IntSet.fromList
      . filter (not . (`IntSet.member` curLayer))
      . map unRuleRef
      ) nodeRules

{-# SPECIALIZE touch :: ExprRef -> InferT Maybe () #-}
{-# SPECIALIZE touch :: Monoid w => ExprRef -> InferT (Writer w) () #-}

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

exprIntoContext ::
  Monad m =>
  Map DataIRef.DefinitionIRef (Data.Expression DataIRef.DefinitionIRef ()) -> Scope ->
  Data.Expression DataIRef.DefinitionIRef s ->
  InferT m (Data.Expression DataIRef.DefinitionIRef (InferNode, s))
exprIntoContext defTypes rootScope rootExpr = do
  rootExprTV <-
    liftState . Lens.zoom sContext .
    Traversable.mapM tupleInto $
    fmap addTypedVal rootExpr
  Data.recurseWithScopeM addToScope f rootScope rootExprTV
  where
    tupleInto (x, act) = liftM ((,) x) act
    addTypedVal x = (x, toStateT createTypedVal)
    addToScope paramGuid (_, TypedValue paramTypeVal _) =
      Map.insert (Data.ParameterRef paramGuid) paramTypeVal
    f scope expr@(Data.Expression _ (originS, typedValue)) = do
      let TypedValue val typ = typedValue
      (initialVal, initialType) <-
        liftState . Lens.zoom (sContext . nextOrigin) . toStateT . initialExprs defTypes scope $
        void expr
      setRefExpr val $ RefExprPayload mempty <$> initialVal
      setRefExpr typ $ RefExprPayload mempty <$> initialType
      return (InferNode typedValue scope, originS)

data Loaded a = Loaded
  { lRealExpr :: Data.Expression DataIRef.DefinitionIRef a
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
  liftM (Loaded expr . Map.fromList) .
  mapM loadType $ ordNub
  [ defI
  | Data.ExpressionLeaf (Data.GetVariable (Data.DefinitionRef defI)) <-
    map (Lens.view Data.eValue) $ Data.subExpressions expr
  , Just defI /= mRecursiveDef
  ]
  where
    loadType defI = liftM ((,) defI) $ loadPureDefinitionType loader defI

addRule :: Rule -> State InferState ()
addRule rule = do
  ruleRef <- makeRule
  mapM_ (addRuleId ruleRef) $ ruleInputs rule
  sBfsNextLayer . Lens.contains (unRuleRef ruleRef) .= True
  where
    makeRule = do
      ruleRef <- Lens.zoom sContext createEmptyRefRule
      sContext . ruleRefsAt ruleRef .= rule
      return ruleRef
    addRuleId ruleRef ref = sContext . exprRefsAt ref . rRules %= (ruleRef :)

updateAndInfer ::
  Monad m => InferActions m -> Context ->
  [(ExprRef, Data.Expression DataIRef.DefinitionIRef ())] ->
  Expression a -> m (Expression a, Context)
updateAndInfer actions prevContext updates expr =
  execInferT actions inferState $ do
    mapM_ doUpdate updates
    return $ f <$> expr
  where
    inferState = InferState prevContext mempty mempty
    f inferred = (iPoint inferred, iStored inferred)
    doUpdate (ref, newExpr) =
      setRefExpr ref =<<
      (liftState . Lens.zoom (sContext . nextOrigin) . makeRefExprFromPure) newExpr
    makeRefExprFromPure =
      Traversable.mapM . const . liftM (RefExprPayload mempty) $ toStateT mkOrigin

data IsNewRoot = NewRoot | ExistingNode
  deriving (Typeable)
derive makeBinary ''IsNewRoot

inferLoaded ::
  Monad m => InferActions m -> IsNewRoot -> Loaded a -> Context -> InferNode ->
  m (Expression a, Context)
inferLoaded actions isNewRoot loaded initialContext node =
  execInferT actions initialState $ do
    expr <- exprIntoContext (lDefinitionTypes loaded) (nScope node) (lRealExpr loaded)
    liftState . toStateT $ do
      let
        addUnionRules f =
          mapM_ addRule $ on unionRules (f . nRefs) node . fst $ expr ^. Data.ePayload
      addUnionRules tvVal
      addUnionRules tvType
      rules <-
        Lens.zoom (sContext . nextOrigin) .
        makeRules $ fmap fst expr
      mapM_ addRule rules
    return expr
  where
    initialState = InferState initialContext mempty mempty
    makeRules =
      case isNewRoot of
      NewRoot -> makeAllRules
      ExistingNode -> makeResumptionRules

{-# SPECIALIZE
  inferLoaded ::
    InferActions Maybe -> IsNewRoot -> Loaded a -> Context -> InferNode ->
    Maybe (Expression a, Context)
  #-}
{-# SPECIALIZE
  inferLoaded ::
    Monoid w => InferActions (Writer w) -> IsNewRoot -> Loaded a -> Context -> InferNode ->
    Writer w (Expression a, Context)
  #-}