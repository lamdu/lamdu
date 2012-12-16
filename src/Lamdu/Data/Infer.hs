{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell, DeriveFunctor, DeriveFoldable, DeriveTraversable, DeriveDataTypeable,
             PatternGuards #-}
module Lamdu.Data.Infer
  ( Inferred(..), rExpression
  , Loaded, load
  , inferLoaded
  , addRules, derefExpr
  -- TODO: Expose only ref readers for InferNode (instead of .. and TypedValue)
  , IsRestrictedPoly(..)
  , InferNode(..), TypedValue(..)
  , Error(..), ErrorDetails(..)
  , RefMap, Context, ExprRef
  , Loader(..), InferActions(..)
  , initial
  -- Used for inferring independent expressions in an inner infer context
  -- (See hole apply forms).
  , newNodeWithScope
  , newTypedNodeWithScope
  , createRefExpr
  ) where

import Control.Applicative (Applicative(..), (<$), (<$>))
import Control.Lens ((%=), (.=), (^.), (+=))
import Control.Monad ((<=<), guard, unless, void, when)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Either (EitherT(..))
import Control.Monad.Trans.State (StateT(..), State, runState)
import Control.Monad.Trans.State.Utils (toStateT)
import Control.Monad.Trans.Writer (Writer)
import Control.MonadA (MonadA)
import Data.Binary (Binary(..), getWord8, putWord8)
import Data.Derive.Binary (makeBinary)
import Data.DeriveTH (derive)
import Data.Foldable (traverse_)
import Data.Function (on)
import Data.Functor.Identity (Identity(..))
import Data.IntMap (IntMap)
import Data.IntSet (IntSet)
import Data.Map (Map)
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Monoid (Monoid(..))
import Data.Traversable (traverse)
import Data.Typeable (Typeable)
import Lamdu.Data.IRef (DefI)
import Lamdu.Data.Infer.Rules (Rule(..))
import Lamdu.Data.Infer.Types
import qualified Control.Lens as Lens
import qualified Control.Lens.TH as LensTH
import qualified Control.Monad.Trans.Either as Either
import qualified Control.Monad.Trans.State as State
import qualified Data.Foldable as Foldable
import qualified Data.IntSet as IntSet
import qualified Data.Map as Map
import qualified Data.Monoid as Monoid
import qualified Data.Set as Set
import qualified Lamdu.Data as Data
import qualified Lamdu.Data.Infer.Rules as Rules

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

-- When recursing on an expression, we remember the parent expression origins,
-- And we make sure not to add a sub-expression with a parent origin (that's a recursive structure).

data RefData def = RefData
  { _rExpression :: RefExpression def
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

refsAt :: Functor f => Int -> Lens.SimpleLensLike f (RefMap a) a
refsAt k = refs . Lens.at k . Lens.iso from Just
  where
    from = fromMaybe $ error msg
    msg = unwords ["intMapMod: key", show k, "not in map"]

createRef :: a -> State (RefMap a) Int
createRef initialVal = do
  ref <- createEmptyRef
  refsAt ref .= initialVal
  return ref
-------------- InferActions

data ErrorDetails def
  = MismatchIn
    (Data.Expression def ())
    (Data.Expression def ())
  | InfiniteExpression (Data.Expression def ())
  deriving (Show, Eq, Ord)
derive makeBinary ''ErrorDetails
instance Functor ErrorDetails where
  fmap f (MismatchIn x y) =
    on MismatchIn (Lens.over Data.expressionDef f) x y
  fmap f (InfiniteExpression x) =
    InfiniteExpression $ Lens.over Data.expressionDef f x

data Error def = Error
  { errRef :: ExprRef
  , errMismatch ::
    ( Data.Expression def ()
    , Data.Expression def ()
    )
  , errDetails :: ErrorDetails def
  } deriving (Show, Eq, Ord)
derive makeBinary ''Error
instance Functor Error where
  fmap f (Error ref mis details) =
    Error ref
    (Lens.over (Lens.both . Data.expressionDef) f mis)
    (fmap f details)

newtype InferActions def m = InferActions
  { reportError :: Error def -> m ()
  }

--------------

data Context def = Context
  { _exprMap :: RefMap (RefData def)
  , _nextOrigin :: Int
  , _ruleMap :: RefMap (Rule def)
  } deriving (Typeable)
derive makeBinary ''Context

data InferState def m = InferState
  { _sContext :: Context def
  , _sBfsNextLayer :: IntSet
  , _sBfsCurLayer :: IntSet
  , _sActions :: InferActions def m
  }
LensTH.makeLenses ''Context
LensTH.makeLenses ''InferState

-- ExprRefMap:

toRefExpression :: Data.Expression def () -> State Origin (RefExpression def)
toRefExpression =
  traverse . const $
  RefExprPayload mempty (Monoid.Any False) <$> mkOrigin

createRefExpr :: State (Context def) ExprRef
createRefExpr = do
  holeRefExpr <- Lens.zoom nextOrigin $ toRefExpression Data.pureHole
  fmap ExprRef . Lens.zoom exprMap . createRef $ RefData holeRefExpr mempty

exprRefsAt :: Functor f => ExprRef -> Lens.SimpleLensLike f (Context def) (RefData def)
exprRefsAt k = exprMap . refsAt (unExprRef k)

-- RuleRefMap

createEmptyRefRule :: State (Context def) RuleRef
createEmptyRefRule = fmap RuleRef $ Lens.zoom ruleMap createEmptyRef

ruleRefsAt :: Functor f => RuleRef -> Lens.SimpleLensLike f (Context def) (Rule def)
ruleRefsAt k = ruleMap . refsAt (unRuleRef k)

-------------

-- TODO: createTypeVal should use newNode, not vice versa.
-- For use in loading phase only!
-- We don't create additional Refs afterwards!
createTypedVal :: State (Context def) TypedValue
createTypedVal = TypedValue <$> createRefExpr <*> createRefExpr

newNodeWithScope :: Scope def -> State (Context def) (InferNode def)
newNodeWithScope scope = (`InferNode` scope) <$> createTypedVal

newTypedNodeWithScope ::
  Scope def -> ExprRef -> State (Context def) (InferNode def)
newTypedNodeWithScope scope typ =
  (`InferNode` scope) . (`TypedValue` typ) <$> createRefExpr

initial :: Ord def => Maybe def -> (Context def, InferNode def)
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

derefExpr ::
  Data.Expression def (InferNode def, a) -> Context def ->
  Data.Expression def (Inferred def, a)
derefExpr expr context =
  derefNode <$> expr
  where
    derefNode (inferNode, s) =
      ( Inferred
        { iValue = deref . tvVal $ nRefs inferNode
        , iType = deref . tvType $ nRefs inferNode
        , iScope =
          Map.fromList . mapMaybe onScopeElement . Map.toList $ nScope inferNode
        , iPoint = inferNode
        }
      , s
      )
    onScopeElement (Data.ParameterRef guid, ref) = Just (guid, deref ref)
    onScopeElement _ = Nothing
    toIsRestrictedPoly False = UnrestrictedPoly
    toIsRestrictedPoly True = RestrictedPoly
    deref ref =
      toIsRestrictedPoly . Monoid.getAny . Lens.view rplRestrictedPoly <$>
      context ^. exprRefsAt ref . rExpression

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
      Rule deps ruleClosure <-
        liftState $ Lens.use (sContext . ruleRefsAt (RuleRef key))
      refExps <- traverse getRefExpr deps
      traverse_ (uncurry setRefExpr) $ Rules.runClosure ruleClosure refExps

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
  { loadPureDefinitionType :: def -> m (Data.Expression def ())
  }

-- Initial expression for inferred value and type of a stored entity.
-- Types are returned only in cases of expanding definitions.
initialValExpr ::
  Data.Expression def () ->
  State Origin (RefExpression def)
initialValExpr (Data.Expression body ()) =
  toRefExpression $
  case body of
  Data.ExpressionApply _ -> Data.pureHole
  _ -> circumcized body
  where
    circumcized = Data.pureExpression . (Data.pureHole <$)

-- This is because platform's Either's MonadA instance sucks
runEither :: EitherT l Identity a -> Either l a
runEither = runIdentity . runEitherT

guardEither :: l -> Bool -> EitherT l Identity ()
guardEither err False = Either.left err
guardEither _ True = return ()

originRepeat :: RefExpression def -> Bool
originRepeat =
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
-- Param guids and Origins come from the first expression.
-- If origins repeat, fail.
mergeExprs ::
  Eq def =>
  RefExpression def ->
  RefExpression def ->
  Either (ErrorDetails def) (RefExpression def)
mergeExprs p0 p1 =
  runEither $ do
    result <- Data.matchExpression onMatch onMismatch p0 p1
    guardEither (InfiniteExpression (void result)) . not $ originRepeat result
    return result
  where
    src `mergePayloadInto` dest =
      mappendLens rplRestrictedPoly src .
      mappendLens rplSubstitutedArgs src $
      dest
    mappendLens lens src =
      Lens.over (Lens.cloneLens lens)
      ((mappend . Lens.view (Lens.cloneLens lens)) src)
    onMatch x y = return $ y `mergePayloadInto` x
    onMismatch (Data.Expression (Data.ExpressionLeaf Data.Hole) s0) e1 =
      return $ (s0 `mergePayloadInto`) <$> e1
    onMismatch e0 (Data.Expression (Data.ExpressionLeaf Data.Hole) s1) =
      return $ (s1 `mergePayloadInto`) <$> e0
    onMismatch e0 e1 =
      Either.left $ MismatchIn (void e0) (void e1)

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

setRefExpr :: (Eq def, MonadA m) => ExprRef -> RefExpression def -> InferT def m ()
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
      report <- fmap reportError askActions
      lift $ report Error
        { errRef = ref
        , errMismatch = (void curExpr, void newExpr)
        , errDetails = details
        }
  where
    equiv x y =
      isJust $
      Data.matchExpression comparePl ((const . const) Nothing) x y
    comparePl x y =
      guard $
      (x ^. rplSubstitutedArgs) == (y ^. rplSubstitutedArgs) &&
      (x ^. rplRestrictedPoly) == (y ^. rplRestrictedPoly)

{-# SPECIALIZE setRefExpr :: ExprRef -> RefExpression (DefI t) -> InferT (DefI t) Maybe () #-}
{-# SPECIALIZE setRefExpr :: Monoid w => ExprRef -> RefExpression (DefI t) -> InferT (DefI t) (Writer w) () #-}

liftOriginState :: MonadA m => State Origin a -> InferT def m a
liftOriginState = liftState . Lens.zoom (sContext . nextOrigin) . toStateT

liftContextState :: MonadA m => State (Context def) a -> InferT def m a
liftContextState = liftState . Lens.zoom sContext . toStateT

exprIntoContext ::
  (MonadA m, Ord def) => Scope def ->
  Loaded def a ->
  InferT def m (Data.Expression def (InferNode def, a))
exprIntoContext rootScope (Loaded rootExpr defTypes) = do
  defTypesRefs <-
    traverse defTypeIntoContext $
    Map.mapKeys Data.DefinitionRef defTypes
  -- mappend prefers left, so it is critical we put rootScope
  -- first. defTypesRefs may contain the loaded recursive defI because
  -- upon resumption, we load without giving the root defI, so its
  -- type does get (unnecessarily) loaded.
  go (rootScope `mappend` defTypesRefs) =<<
    liftContextState
    (traverse addTypedVal rootExpr)
  where
    defTypeIntoContext defType = do
      ref <- liftContextState createRefExpr
      setRefExpr ref =<< liftOriginState (toRefExpression defType)
      return ref
    addTypedVal x = fmap ((,) x) createTypedVal
    go scope (Data.Expression body (s, createdTV)) = do
      inferNode <- toInferNode scope (void <$> body) createdTV
      newBody <-
        case body of
        Data.ExpressionLambda lam -> goLambda Data.makeLambda scope lam
        Data.ExpressionPi lam -> goLambda Data.makePi scope lam
        _ -> traverse (go scope) body
      return $ Data.Expression newBody (inferNode, s)
    goLambda cons scope (Data.Lambda paramGuid paramType result) = do
      paramTypeDone <- go scope paramType
      let
        paramTypeRef = tvVal . nRefs . fst $ paramTypeDone ^. Data.ePayload
        newScope = Map.insert (Data.ParameterRef paramGuid) paramTypeRef scope
      resultDone <- go newScope result
      return $ cons paramGuid paramTypeDone resultDone

    toInferNode scope body tv = do
      let
        typedValue@(TypedValue val _) =
          tv
          { tvType =
            case body of
            Data.ExpressionLeaf (Data.GetVariable varRef)
              | Just x <- Map.lookup varRef scope -> x
            _ -> tvType tv
          }
      initialVal <- liftOriginState . initialValExpr $ Data.pureExpression body
      setRefExpr val initialVal
      return $ InferNode typedValue scope

ordNub :: Ord a => [a] -> [a]
ordNub = Set.toList . Set.fromList

data Loaded def a = Loaded
  { _lExpr :: Data.Expression def a
  , _lDefTypes :: Map def (Data.Expression def ())
  } deriving (Typeable, Functor)
-- Requires Ord instance for def, cannot derive
instance (Binary a, Binary def, Ord def) => Binary (Loaded def a) where
  get = Loaded <$> get <*> get
  put (Loaded a b) = put a >> put b

load ::
  (MonadA m, Ord def) => Loader def m ->
  Maybe def -> Data.Expression def a ->
  m (Loaded def a)
load loader mRecursiveDef expr =
  fmap (Loaded expr) loadDefTypes
  where
    loadDefTypes =
      fmap Map.fromList .
      traverse loadType . ordNub $
      Lens.toListOf
      ( Lens.folding Data.subExpressions . Data.eValue
      . Data.expressionLeaf . Data.getVariable . Data.definitionRef
      . Lens.filtered ((/= mRecursiveDef) . Just)
      ) expr
    loadType defI = fmap ((,) defI) $ loadPureDefinitionType loader defI

addRule :: Rule def -> State (InferState def m) ()
addRule rule = do
  ruleRef <- makeRule
  traverse_ (addRuleId ruleRef) $ ruleInputs rule
  sBfsNextLayer . Lens.contains (unRuleRef ruleRef) .= True
  where
    makeRule = do
      ruleRef <- Lens.zoom sContext createEmptyRefRule
      sContext . ruleRefsAt ruleRef .= rule
      return ruleRef
    addRuleId ruleRef ref = sContext . exprRefsAt ref . rRules %= (ruleRef :)

addRules ::
  (Eq def, MonadA m) => InferActions def m ->
  [Data.Expression def (InferNode def)] ->
  StateT (Context def) m ()
addRules actions exprs =
  execInferT actions . liftState . toStateT $
  traverse_ addRule . concat =<<
  (Lens.zoom (sContext . nextOrigin) .
   traverse Rules.makeForNode . (map . fmap) nRefs) exprs

inferLoaded ::
  (Ord def, MonadA m) =>
  InferActions def m -> Loaded def a ->
  InferNode def ->
  StateT (Context def) m (Data.Expression def (Inferred def, a))
inferLoaded actions loadedExpr node =
  State.gets . derefExpr <=<
  execInferT actions $ do
    expr <- exprIntoContext (nScope node) loadedExpr
    liftState . toStateT $ do
      let
        addUnionRules f =
          traverse_ addRule $ on Rules.union (f . nRefs) node . fst $ expr ^. Data.ePayload
      addUnionRules tvVal
      addUnionRules tvType
      rules <-
        Lens.zoom (sContext . nextOrigin) .
        Rules.makeForAll $ nRefs . fst <$> expr
      traverse_ addRule rules
    return expr

{-# SPECIALIZE
  inferLoaded ::
    InferActions (DefI t) Maybe -> Loaded (DefI t) a ->
    InferNode (DefI t) ->
    StateT (Context (DefI t)) Maybe (Data.Expression (DefI t) (Inferred (DefI t), a))
  #-}
{-# SPECIALIZE
  inferLoaded ::
    Monoid w => InferActions (DefI t) (Writer w) -> Loaded (DefI t) a ->
    InferNode (DefI t) ->
    StateT (Context (DefI t)) (Writer w) (Data.Expression (DefI t) (Inferred (DefI t), a))
  #-}
