{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell, DeriveFunctor, DeriveFoldable, DeriveTraversable, DeriveDataTypeable,
             PatternGuards #-}
module Lamdu.Data.Expression.Infer
  ( Inferred(..), rExpression
  , Loaded, load, loadIndependent
  , inferLoaded, addRules
  , derefExpr, derefNode
  , IsRestrictedPoly(..)
  , Node(..), TypedValue(..)
  , Error(..), MismatchError(..), MismatchErrorDetails(..)
  , RefMap, Context, ExprRef, Scope
  , Loader(..), InferActions(..)
  , initial
  -- Used for inferring independent expressions in an inner infer context
  -- (See hole apply forms).
  , newNodeWithScope
  , createRefExpr
  ) where

import Control.Applicative (Applicative(..), (<$>), (<$))
import Control.DeepSeq (NFData(..))
import Control.Lens (LensLike')
import Control.Lens.Operators
import Control.Monad ((<=<), guard, unless, void, when)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Either (EitherT(..))
import Control.Monad.Trans.State (StateT(..), State, runState)
import Control.Monad.Trans.State.Utils (toStateT)
import Control.Monad.Trans.Writer (Writer)
import Control.MonadA (MonadA)
import Data.Binary (Binary(..), getWord8, putWord8)
import Data.Derive.Binary (makeBinary)
import Data.Derive.NFData (makeNFData)
import Data.DeriveTH (derive)
import Data.Foldable (traverse_)
import Data.Function (on)
import Data.Functor.Identity (Identity(..))
import Data.IntMap (IntMap)
import Data.IntSet (IntSet)
import Data.Map (Map)
import Data.Maybe (isJust, fromMaybe, fromJust)
import Data.Maybe.Utils(unsafeUnjust)
import Data.Monoid (Monoid(..))
import Data.Traversable (traverse)
import Data.Typeable (Typeable)
import Lamdu.Data.Expression.IRef (DefI)
import Lamdu.Data.Expression.Infer.Rules (Rule(..))
import Lamdu.Data.Expression.Infer.Types
import qualified Control.Lens as Lens
import qualified Control.Lens.Utils as LensUtils
import qualified Control.Monad.Trans.Either as Either
import qualified Control.Monad.Trans.State as State
import qualified Data.Foldable as Foldable
import qualified Data.IntSet as IntSet
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Lamdu.Data.Expression as Expr
import qualified Lamdu.Data.Expression.IRef as ExprIRef
import qualified Lamdu.Data.Expression.Infer.Rules as Rules
import qualified Lamdu.Data.Expression.Lens as ExprLens
import qualified Lamdu.Data.Expression.Utils as ExprUtil

newtype RuleRef = RuleRef { unRuleRef :: Int }
  deriving (Eq, Ord)

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

-- When recursing on an expression, we remember the parent expression origins,
-- And we make sure not to add a sub-expression with a parent origin (that's a recursive structure).

data RefData def = RefData
  { _rExpression :: RefExpression def
  , _rRules :: [RuleRef] -- Rule id
  } deriving (Eq, Ord)

--------------
--- RefMap:
data RefMap a = RefMap
  { _refs :: IntMap a
  , _nextRef :: Int
  } deriving (Eq, Ord)
Lens.makeLenses ''RefData
Lens.makeLenses ''RefMap

emptyRefMap :: RefMap a
emptyRefMap =
  RefMap
  { _refs = mempty
  , _nextRef = 0
  }

{-# INLINE createRef #-}
createRef :: MonadA m => a -> StateT (RefMap a) m Int
createRef val = do
  key <- Lens.use nextRef
  nextRef += 1
  refs . Lens.at key .= Just val
  return key

{-# INLINE refsAt #-}
refsAt :: Functor f => Int -> LensLike' f (RefMap a) (Maybe a)
refsAt k = refs . Lens.at k
-------------- InferActions

data MismatchErrorDetails def
  = MismatchIn
    (Expr.Expression def ())
    (Expr.Expression def ())
  | InfiniteExpression (Rule def (Expr.Expression def ()))
  deriving (Eq, Ord, Show)

instance Functor MismatchErrorDetails where
  fmap f (MismatchIn x y) =
    on MismatchIn (ExprLens.exprDef %~ f) x y
  fmap _ (InfiniteExpression _) =
    error "TODO: Functor MismatchErrorDetails case of InfiniteExpression"

data MismatchError def = MismatchError
  { errRef :: ExprRef
  , errMismatch ::
    ( Expr.Expression def ()
    , Expr.Expression def ()
    )
  , errDetails :: MismatchErrorDetails def
  } deriving (Show, Eq, Ord)

instance Functor MismatchError where
  fmap f (MismatchError ref mis details) =
    MismatchError ref
    (mis & Lens.both . ExprLens.exprDef %~ f)
    (f <$> details)

data Error def
  = ErrorMismatch (MismatchError def)
  | ErrorMissingDefType def
  deriving (Functor, Eq, Ord, Show)

newtype InferActions def m = InferActions
  { reportError :: Error def -> m ()
  }

--------------

data Context def = Context
  { _exprMap :: RefMap (RefData def)
  , _ruleMap :: RefMap (Rule def ExprRef)
  } deriving (Typeable, Eq, Ord)

data InferState def m = InferState
  { _sContext :: Context def
  , _sBfsNextLayer :: IntSet
  , _sBfsCurLayer :: IntSet
  , _sActions :: InferActions def m
  }
Lens.makeLenses ''Context
Lens.makeLenses ''InferState

fmap concat . sequence $
  derive
  <$> [makeBinary, makeNFData]
  <*> [''Context, ''MismatchErrorDetails, ''MismatchError, ''RuleRef, ''RefData, ''RefMap]

-- ExprRefMap:

toRefExpression :: Expr.Expression def () -> RefExpression def
toRefExpression = (RefExprPayload mempty mempty mempty <$)

createRefExpr :: MonadA m => StateT (Context def) m ExprRef
createRefExpr =
  fmap ExprRef . Lens.zoom exprMap . createRef $
  RefData (toRefExpression ExprUtil.pureHole) mempty

{-# INLINE exprRefsAt #-}
exprRefsAt :: Functor f => ExprRef -> LensLike' f (Context def) (RefData def)
exprRefsAt k =
  exprMap . refsAt (unExprRef k) .
  LensUtils._fromJust (unwords ["exprRefsAt: key", show k, "not in map"])

-- RuleRefMap

createRuleRef :: Rule def ExprRef -> State (Context def) RuleRef
createRuleRef = fmap RuleRef . Lens.zoom ruleMap . createRef

{-# INLINE ruleRefsAt #-}
ruleRefsAt :: Functor f => RuleRef -> LensLike' f (Context def) (Rule def ExprRef)
ruleRefsAt k =
  ruleMap . refsAt (unRuleRef k) .
  LensUtils._fromJust (unwords ["ruleRefsAt: key", show k, "not in map"])

-------------

createTypedVal :: MonadA m => StateT (Context def) m TypedValue
createTypedVal = TypedValue <$> createRefExpr <*> createRefExpr

newNodeWithScope :: MonadA m => Scope def -> StateT (Context def) m (Node def)
newNodeWithScope scope = (`Node` scope) <$> createTypedVal

initial :: Ord def => Maybe def -> (Context def, Node def)
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
              Map.singleton (Expr.DefinitionRef recursiveDefI) (tvType rootTv)
        return $ Node rootTv scope
    emptyContext =
      Context
      { _exprMap = emptyRefMap
      , _ruleMap = emptyRefMap
      }

--- InferT:

newtype InferT def m a =
  InferT { unInferT :: StateT (InferState def m) m a }
  deriving (Functor, Applicative, Monad)

askActions :: MonadA m => InferT def m (InferActions def m)
askActions = InferT $ Lens.use sActions

liftState :: Monad m => StateT (InferState def m) m a -> InferT def m a
liftState = InferT

{-# SPECIALIZE liftState :: StateT (InferState def Maybe) Maybe a -> InferT def Maybe a #-}
{-# SPECIALIZE liftState :: Monoid w => StateT (InferState def (Writer w)) (Writer w) a -> InferT def (Writer w) a #-}

instance MonadTrans (InferT def) where
  lift = liftState . lift

derefNode :: Context def -> Node def -> Maybe (Inferred def)
derefNode context inferNode =
  mkInferred
  <$> (deref . tvVal . nRefs) inferNode
  <*> (deref . tvType . nRefs) inferNode
  <*> ( nScope inferNode
        & traverse onScopeElement . Map.toList
        <&> Map.fromList . concat )
  where
    mkInferred val typ scope =
      Inferred
      { iValue = val
      , iType = typ
      , iScope = scope
      , iNode = inferNode
      }
    onScopeElement (Expr.ParameterRef guid, ref) = (: []) . (,) guid <$> deref ref
    onScopeElement _ = pure []
    toIsRestrictedPoly False = UnrestrictedPoly
    toIsRestrictedPoly True = RestrictedPoly
    deref ref =
      context ^? exprMap . refsAt (unExprRef ref) . Lens._Just . rExpression
      <&> Lens.mapped %~
          toIsRestrictedPoly . (^. rplRestrictedPoly . Lens.unwrapped)
{-# INLINE derefNode #-}

derefExpr ::
  Expr.Expression def (Node def, a) -> Context def ->
  Expr.Expression def (Inferred def, a)
derefExpr expr context =
  expr <&> Lens._1 %~ unsafeUnjust msg . derefNode context
  where
    msg = "derefExpr must be given valid expr/context pair"

getRefExpr :: MonadA m => ExprRef -> InferT def m (RefExpression def)
getRefExpr ref = liftState $ Lens.use (sContext . exprRefsAt ref . rExpression)

{-# SPECIALIZE getRefExpr :: ExprRef -> InferT (DefI t) Maybe (RefExpression (DefI t)) #-}
{-# SPECIALIZE getRefExpr :: Monoid w => ExprRef -> InferT (DefI t) (Writer w) (RefExpression (DefI t)) #-}

executeRules :: (Eq def, MonadA m) => InferT def m ()
executeRules = do
  curLayer <- liftState $ Lens.use sBfsNextLayer
  liftState $ sBfsCurLayer .= curLayer
  liftState $ sBfsNextLayer .= IntSet.empty
  unless (IntSet.null curLayer) $ do
    traverse_ processRule $ IntSet.toList curLayer
    executeRules
  where
    processRule key = do
      liftState $ sBfsCurLayer . Lens.contains key .= False
      ruleRefs <- liftState $ Lens.use (sContext . ruleRefsAt (RuleRef key))
      ruleExprs <- traverse getRefExpr ruleRefs
      traverse_ (uncurry (setRefExpr (Just (RuleRef key, ruleExprs)))) $ Rules.runRule ruleExprs

{-# SPECIALIZE executeRules :: InferT (DefI t) Maybe () #-}
{-# SPECIALIZE executeRules :: Monoid w => InferT (DefI t) (Writer w) () #-}

execInferT ::
  (MonadA m, Eq def) => InferActions def m ->
  InferT def m a -> StateT (Context def) m a
execInferT actions act = do
  inferState <- State.gets mkInferState
  (res, newState) <-
    lift . (`runStateT` inferState) . unInferT $ do
      res <- act
      executeRules
      return res
  State.put $ newState ^. sContext
  return res
  where
    mkInferState ctx = InferState ctx mempty mempty actions

{-# SPECIALIZE
  execInferT ::
    InferActions (DefI t) Maybe -> InferT (DefI t) Maybe a ->
    StateT (Context (DefI t)) Maybe a
  #-}

{-# SPECIALIZE
  execInferT ::
    Monoid w =>
    InferActions (DefI t) (Writer w) -> InferT (DefI t) (Writer w) a ->
    StateT (Context (DefI t)) (Writer w) a
  #-}

newtype Loader def m = Loader
  { loadPureDefinitionType :: def -> m (Expr.Expression def ())
  }

-- This is because platform's Either's MonadA instance sucks
runEither :: EitherT l Identity a -> Either l a
runEither = runIdentity . runEitherT

guardEither :: l -> Bool -> EitherT l Identity ()
guardEither err False = Either.left err
guardEither _ True = return ()

-- Merge two expressions:
-- If they do not match, return Nothing.
-- Holes match with anything, expand to the other expr.
-- Param guids and Origins come from the first expression.
-- If origins repeat, fail.
mergeExprs ::
  Eq def =>
  RefExpression def ->
  Maybe (RuleRef, Rule def (RefExpression def)) ->
  RefExpression def ->
  Either (MismatchErrorDetails def) (RefExpression def)
mergeExprs oldExp mRule newExp =
  runEither $ ExprUtil.matchExpression onMatch onMismatch oldExp newExp
  where
    mergePayloadInto src =
      mappendLens rplRestrictedPoly src .
      mappendLens rplSubstitutedArgs src .
      mappendLens rplOrigins src
    mappendLens lens src =
      Lens.cloneLens lens <>~ src ^. Lens.cloneLens lens
    onMatch x y = return $ y `mergePayloadInto` x
    mergePayloads s e =
      e
      & Expr.ePayload %~ (mappendLens rplRestrictedPoly s . mappendLens rplOrigins s)
      & Lens.mapped %~ mappendLens rplSubstitutedArgs s
    onMismatch e0 (Expr.Expression (Expr.BodyLeaf Expr.Hole) s1) =
      return $ mergePayloads s1 e0
    onMismatch (Expr.Expression (Expr.BodyLeaf Expr.Hole) s0) e1 = do
      guardEither ((InfiniteExpression . fmap void . snd . fromJust) mRule) .
        IntSet.null . IntSet.intersection origins .
        mconcat $ e1 ^.. Lens.traversed . rplOrigins
      return .
        mergePayloads s0 $
        e1 &
        Lens.filtered (Lens.has (ExprLens.exprLeaves . Expr._Hole)) .
        Expr.ePayload . rplOrigins <>~ origins
    onMismatch e0 e1 =
      Either.left $ MismatchIn (void e0) (void e1)
    origins = maybe mempty (IntSet.singleton . unRuleRef . fst) mRule

touch :: MonadA m => ExprRef -> InferT def m ()
touch ref =
  liftState $ do
    nodeRules <- Lens.use (sContext . exprRefsAt ref . rRules)
    curLayer <- Lens.use sBfsCurLayer
    sBfsNextLayer %=
      ( mappend . IntSet.fromList
      . filter (not . (`IntSet.member` curLayer))
      . map unRuleRef
      ) nodeRules

{-# SPECIALIZE touch :: ExprRef -> InferT (DefI t) Maybe () #-}
{-# SPECIALIZE touch :: Monoid w => ExprRef -> InferT (DefI t) (Writer w) () #-}

reportAnError :: MonadA m => Error def -> InferT def m ()
reportAnError err = do
  report <- reportError <$> askActions
  lift $ report err

setRefExpr ::
  (Eq def, MonadA m) =>
  Maybe (RuleRef, Rule def (RefExpression def)) ->
  ExprRef ->
  RefExpression def -> InferT def m ()
setRefExpr mRule ref newExpr = do
  curExpr <- liftState $ Lens.use (sContext . exprRefsAt ref . rExpression)
  case mergeExprs curExpr mRule newExpr of
    Right mergedExpr -> do
      let
        isChange = not $ equiv mergedExpr curExpr
        isHole = Lens.notNullOf ExprLens.exprHole mergedExpr
      when isChange $ touch ref
      when (isChange || isHole) $
        liftState $ sContext . exprRefsAt ref . rExpression .= mergedExpr
    Left details ->
      reportAnError $ ErrorMismatch MismatchError
      { errRef = ref
      , errMismatch = (void curExpr, void newExpr)
      , errDetails = details
      }
  where
    equiv x y =
      isJust $
      ExprUtil.matchExpression comparePl ((const . const) Nothing) x y
    comparePl x y =
      guard $
      (x ^. rplSubstitutedArgs) == (y ^. rplSubstitutedArgs) &&
      (x ^. rplRestrictedPoly) == (y ^. rplRestrictedPoly)

{-# SPECIALIZE setRefExpr :: Maybe (RuleRef, Rule (DefI t) (RefExpression (DefI t))) -> ExprRef -> RefExpression (DefI t) -> InferT (DefI t) Maybe () #-}
{-# SPECIALIZE setRefExpr :: Monoid w => Maybe (RuleRef, Rule (DefI t) (RefExpression (DefI t))) -> ExprRef -> RefExpression (DefI t) -> InferT (DefI t) (Writer w) () #-}

liftContextState :: MonadA m => State (Context def) a -> InferT def m a
liftContextState = liftState . Lens.zoom sContext . toStateT

exprIntoContext ::
  (MonadA m, Ord def) => Scope def ->
  Loaded def a ->
  InferT def m (Expr.Expression def (Node def, a))
exprIntoContext rootScope (Loaded rootExpr defTypes) = do
  defTypesRefs <-
    Map.traverseWithKey defTypeIntoContext defTypes
    <&> Map.mapKeys Expr.DefinitionRef
  -- mappend prefers left, so it is critical we put rootScope
  -- first. defTypesRefs may contain the loaded recursive defI because
  -- upon resumption, we load without giving the root defI, so its
  -- type does get (unnecessarily) loaded.
  go (rootScope `mappend` defTypesRefs) =<<
    liftContextState
    (traverse addTypedVal rootExpr)
  where
    defTypeIntoContext defI (fullType, defType) = do
      unless
        (fullType || Map.member (Expr.DefinitionRef defI) rootScope) .
        reportAnError $ ErrorMissingDefType defI
      ref <- liftContextState createRefExpr
      setRefExpr Nothing ref $ toRefExpression defType
      return ref
    addTypedVal x = fmap ((,) x) createTypedVal
    go scope (Expr.Expression body (s, createdTV)) = do
      inferNode <- toNode scope (void <$> body) createdTV
      newBody <-
        case body of
        Expr.BodyLam (Expr.Lam k paramGuid paramType result) -> do
          paramTypeDone <- go scope paramType
          let
            paramTypeRef = tvVal . nRefs . fst $ paramTypeDone ^. Expr.ePayload
            newScope = Map.insert (Expr.ParameterRef paramGuid) paramTypeRef scope
          resultDone <- go newScope result
          return $ ExprUtil.makeLam k paramGuid paramTypeDone resultDone
        _ -> traverse (go scope) body
      return $ Expr.Expression newBody (inferNode, s)
    toNode scope body tv = do
      let
        typedValue =
          tv
          { tvType =
              fromMaybe (tvType tv) $
              body ^?
                Expr._BodyLeaf . Expr._GetVariable .
                Lens.folding (`Map.lookup` scope)
          }
      return $ Node typedValue scope

ordNub :: Ord a => [a] -> [a]
ordNub = Set.toList . Set.fromList

data Loaded def a = Loaded
  { _lExpr :: Expr.Expression def a
  , _lDefTypes :: Map def (Bool, Expr.Expression def ())
  } deriving (Typeable, Functor, Eq, Ord)
-- Requires Ord instance for def, cannot derive
instance (Binary a, Binary def, Ord def) => Binary (Loaded def a) where
  get = Loaded <$> get <*> get
  put (Loaded a b) = put a >> put b

load ::
  (MonadA m, Ord def) =>
  Loader def m -> Maybe def -> Expr.Expression def a -> m (Loaded def a)
load loader mRecursiveDef expr =
  fmap (Loaded expr) loadDefTypes
  where
    loadDefTypes =
      expr ^..
      Lens.folding ExprUtil.subExpressions .
      ExprLens.exprDefinitionRef .
      Lens.filtered ((/= mRecursiveDef) . Just)
      & traverse loadType . ordNub
      <&> Map.fromList
    markFullTypes typeExpr =
      (Lens.nullOf ExprLens.holePayloads typeExpr, typeExpr)
    loadType defI =
      loadPureDefinitionType loader defI
      <&> (,) defI . markFullTypes

-- An Independent expression has no GetDefinition of any expression
-- except potentially the given recurse def. The given function should
-- yield a justification for the belief that it has no such
-- GetDefinitions in it.
loadIndependent :: Ord def => (def -> String) -> Maybe def -> Expr.Expression def a -> Loaded def a
loadIndependent errStr mRecursiveDef =
  either (error . errStr) id . load (Loader Left) mRecursiveDef

addRule :: Rule def ExprRef -> State (InferState def m) ()
addRule rule = do
  ruleRef <- makeRule
  traverse_ (addRuleRef ruleRef) $ Foldable.toList rule
  sBfsNextLayer . Lens.contains (unRuleRef ruleRef) .= True
  where
    makeRule = Lens.zoom sContext $ createRuleRef rule
    addRuleRef ruleRef ref = sContext . exprRefsAt ref . rRules %= (ruleRef :)

addRules ::
  (Eq def, MonadA m) => InferActions def m ->
  [Expr.Expression def (Node def)] ->
  StateT (Context def) m ()
addRules actions exprs =
  execInferT actions . liftState . toStateT .
  traverse_ addRule . concat .
  traverse Rules.makeForNode $ (map . fmap) nRefs exprs

inferLoaded ::
  (Ord def, MonadA m) =>
  InferActions def m -> Loaded def a ->
  Node def ->
  StateT (Context def) m (Expr.Expression def (Inferred def, a))
inferLoaded actions loadedExpr node =
  State.gets . derefExpr <=<
  execInferT actions $ do
    expr <- exprIntoContext (nScope node) loadedExpr
    liftState . toStateT $ do
      let
        addUnionRules f =
          traverse_ addRule $ on Rules.union (f . nRefs) node . fst $ expr ^. Expr.ePayload
      addUnionRules tvVal
      addUnionRules tvType
      traverse_ addRule . Rules.makeForAll $ nRefs . fst <$> expr
    return expr

{-# SPECIALIZE
  inferLoaded ::
    InferActions (DefI t) Maybe -> Loaded (DefI t) a ->
    Node (DefI t) ->
    StateT (Context (DefI t)) Maybe (ExprIRef.Expression t (Inferred (DefI t), a))
  #-}
{-# SPECIALIZE
  inferLoaded ::
    Monoid w => InferActions (DefI t) (Writer w) -> Loaded (DefI t) a ->
    Node (DefI t) ->
    StateT (Context (DefI t)) (Writer w) (ExprIRef.Expression t (Inferred (DefI t), a))
  #-}
