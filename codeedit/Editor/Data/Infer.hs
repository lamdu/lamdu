{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell, DeriveFunctor, DeriveFoldable, DeriveTraversable, DeriveDataTypeable #-}
module Editor.Data.Infer
  ( Expression, Inferred(..), rExpression
  , Loaded, load, infer
  -- TODO: Expose only ref readers for InferNode (instead of .. and TypedValue)
  , InferNode(..), TypedValue(..)
  , Error(..), ErrorDetails(..)
  , RefMap, Ref
  , Loader(..), InferActions(..)
  , initial
  -- Used for inferring independent expressions in an inner infer context
  -- (See hole apply forms).
  , newNodeWithScope
  , newTypedNodeWithScope
  ) where

import Control.Applicative (Applicative(..), (<$), (<$>), (<*))
import Control.Arrow (first)
import Control.Lens ((%=), (.=), (^.), (+=))
import Control.Monad (guard, liftM, liftM2, unless, void, when)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Either (EitherT(..))
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.Monad.Trans.State (StateT(..), State, runState, execState)
import Control.Monad.Trans.Writer (Writer)
import Data.Binary (Binary(..))
import Data.Derive.Binary (makeBinary)
import Data.DeriveTH (derive)
import Data.Foldable (Foldable(..))
import Data.Functor.Identity (Identity(..))
import Data.IntMap (IntMap, (!))
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

mkOrigin :: State Origin Origin
mkOrigin = State.get <* State.modify (+1)

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

-- TODO: Better name
data RefMap = RefMap
  { _refMap :: IntMap RefData
  , _nextRef :: Int
  , _rules :: IntMap Rule
  , _nextRule :: Int
  , _nextOrigin :: Int
  } deriving (Typeable)
derive makeBinary ''RefMap

data InferState = InferState
  { _sRefMap :: RefMap
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
  deriving Show

data Error = Error
  { errRef :: Ref
  , errMismatch ::
    ( Data.Expression DataIRef.DefinitionIRef ()
    , Data.Expression DataIRef.DefinitionIRef ()
    )
  , errDetails :: ErrorDetails
  } deriving Show

newtype InferActions m = InferActions
  { reportError :: Error -> m ()
  }

LensTH.makeLenses ''RefData
LensTH.makeLenses ''RefMap
LensTH.makeLenses ''InferState

-- TODO: createTypeVal should use newNode, not vice versa.
-- For use in loading phase only!
-- We don't create additional Refs afterwards!
createTypedVal :: Monad m => StateT RefMap m TypedValue
createTypedVal = liftM2 TypedValue createRef createRef

createRef :: Monad m => StateT RefMap m Ref
createRef = do
  key <- Lens.use nextRef
  nextRef += 1
  return $ Ref key

newNodeWithScope :: Scope -> RefMap -> (RefMap, InferNode)
newNodeWithScope scope prevRefMap =
  (resultRefMap, InferNode tv scope)
  where
    (tv, resultRefMap) = runState createTypedVal prevRefMap

newTypedNodeWithScope :: Scope -> Ref -> RefMap -> (RefMap, InferNode)
newTypedNodeWithScope scope typ prevRefMap =
  (resultRefMap, InferNode (TypedValue newValRef typ) scope)
  where
    (newValRef, resultRefMap) = runState createRef prevRefMap

initial :: (RefMap, InferNode)
initial = newNodeWithScope mempty $ RefMap mempty 0 mempty 0 0

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

intMapMod :: Functor f => Int -> (v -> f v) -> IntMap v -> f (IntMap v)
intMapMod k =
  Lens.at k . Lens.iso from Just
  where
    from = fromMaybe . error $ unwords ["intMapMod: key", show k, "not in map"]

refMapAt ::
  Functor f => Ref -> (RefData -> f RefData) -> InferState -> f InferState
refMapAt k = sRefMap . refMap . intMapMod (unRef k)

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

initializeRefData :: Ref -> Data.Expression DataIRef.DefinitionIRef Origin -> State RefMap ()
initializeRefData ref expr =
  refMap . Lens.at (unRef ref) .=
  Just (RefData (fmap (RefExprPayload mempty) expr) [])

exprIntoRefMap ::
  Map DataIRef.DefinitionIRef (Data.Expression DataIRef.DefinitionIRef ()) -> Scope ->
  Data.Expression DataIRef.DefinitionIRef s -> TypedValue ->
  State RefMap (Data.Expression DataIRef.DefinitionIRef (InferNode, s))
exprIntoRefMap defTypes rootScope rootExpr rootTypedValue = do
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
  , lRefMap :: RefMap
  , lSavedRoot :: (Maybe RefData, Maybe RefData)
  }

preprocess :: Loaded a -> RefMap -> InferNode -> Preprocessed a
preprocess loaded initialRefMap (InferNode rootTv rootScope) =
  buildPreprocessed . (`runState` initialRefMap) $
  exprIntoRefMap (lDefinitionTypes loaded) scope
  (lRealExpr loaded) rootTv
  where
    TypedValue rootValR rootTypR = rootTv
    initialMRefData k =
      Lens.view (refMap . Lens.at (unRef k)) initialRefMap
    buildPreprocessed (node, resultRefMap) = Preprocessed
      { lExpr = node
      , lRefMap = resultRefMap
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
  RefMap -> Data.Expression DataIRef.DefinitionIRef (InferNode, a) -> (Expression a, RefMap)
postProcess resultRefMap expr =
  (fmap derefNode expr, resultRefMap)
  where
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
    deref (Ref x) = void $ ((resultRefMap ^. refMap) ! x) ^. rExpression

addRule :: Rule -> State InferState ()
addRule rule = do
  ruleId <- makeRule
  mapM_ (addRuleId ruleId) $ ruleInputs rule
  sBfsNextLayer . Lens.contains ruleId .= True
  where
    makeRule = do
      ruleId <- Lens.use (sRefMap . nextRule)
      sRefMap . nextRule += 1
      sRefMap . rules . Lens.at ruleId .= Just rule
      return ruleId
    addRuleId ruleId ref = refMapAt ref . rRules %= (ruleId :)

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

infer ::
  Monad m => InferActions m -> Loaded a -> RefMap -> InferNode ->
  m (Expression a, RefMap)
infer actions loaded initialRefMap node =
  liftM
  ( uncurry postProcess
  . first (Lens.view sRefMap)
  ) . runInferT actions ruleInferState $ do
    restoreRoot rootValR rootValMRefData
    restoreRoot rootTypR rootTypMRefData
    -- when we resume load,
    -- we want to trigger the existing rules for the loaded root
    touch rootValR
    touch rootTypR
    go
    return expr
  where
    Preprocessed expr loadedRefMap (rootValMRefData, rootTypMRefData) =
      preprocess loaded initialRefMap node
    ruleInferState =
      (`execState` InferState loadedRefMap mempty mempty) $
      mapM_ addRule =<<
      Lens.zoom (sRefMap . nextOrigin)
      (makeRules rootValMRefData (fmap fst expr))
    makeRules Nothing = makeAllRules
    makeRules (Just _) = makeResumptionRules
    TypedValue rootValR rootTypR = nRefs . fst $ expr ^. Data.ePayload
    restoreRoot _ Nothing = return ()
    restoreRoot ref (Just (RefData refExpr refRules)) = do
      liftState $ refMapAt ref . rRules %= (refRules ++)
      setRefExpr ref refExpr
    go = do
      curLayer <- liftState $ Lens.use sBfsNextLayer
      liftState $ sBfsCurLayer .= curLayer
      liftState $ sBfsNextLayer .= IntSet.empty
      unless (IntSet.null curLayer) $ do
        mapM_ processRule $ IntSet.toList curLayer
        go
    processRule key = do
      liftState $ sBfsCurLayer . Lens.contains key .= False
      Just (Rule deps ruleClosure) <-
        liftState $ Lens.use (sRefMap . rules . Lens.at key)
      refExps <- mapM getRefExpr deps
      mapM_ (uncurry setRefExpr) $ runRuleClosure ruleClosure refExps

{-# SPECIALIZE
  infer :: InferActions Maybe -> Loaded a -> RefMap -> InferNode ->
           Maybe (Expression a, RefMap) #-}
{-# SPECIALIZE
  infer :: Monoid w => InferActions (Writer w) -> Loaded a ->
           RefMap -> InferNode ->
           Writer w (Expression a, RefMap) #-}

getRefExpr :: Monad m => Ref -> InferT m RefExpression
getRefExpr ref = liftState $ Lens.use (refMapAt ref . rExpression)

{-# SPECIALIZE getRefExpr :: Ref -> InferT Maybe RefExpression #-}
{-# SPECIALIZE getRefExpr :: Monoid w => Ref -> InferT (Writer w) RefExpression #-}

setRefExpr :: Monad m => Ref -> RefExpression -> InferT m ()
setRefExpr ref newExpr = do
  curExpr <- liftState $ Lens.use (refMapAt ref . rExpression)
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
        liftState $ refMapAt ref . rExpression .= mergedExpr
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

{-# SPECIALIZE setRefExpr :: Ref -> RefExpression -> InferT Maybe () #-}
{-# SPECIALIZE setRefExpr :: Monoid w => Ref -> RefExpression -> InferT (Writer w) () #-}

touch :: Monad m => Ref -> InferT m ()
touch ref =
  liftState $ do
    nodeRules <- Lens.use (refMapAt ref . rRules)
    curLayer <- Lens.use sBfsCurLayer
    sBfsNextLayer %=
      ( mappend . IntSet.fromList
      . filter (not . (`IntSet.member` curLayer))
      ) nodeRules

{-# SPECIALIZE touch :: Ref -> InferT Maybe () #-}
{-# SPECIALIZE touch :: Monoid w => Ref -> InferT (Writer w) () #-}
