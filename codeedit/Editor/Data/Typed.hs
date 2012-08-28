{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving, TemplateHaskell, Rank2Types #-}
module Editor.Data.Typed
  ( Expression, Inferred(..), rExpression
  , InferNode(..), TypedValue(..)
  , Error(..), ErrorDetails(..)
  , RefMap, Ref
  , Loader(..), InferActions(..)
  , inferFromEntity, initial, newNodeWithScope
  ) where

import Control.Applicative ((<*>))
import Control.Arrow (first)
import Control.Lens ((%=), (.=), (^.), (+=))
import Control.Monad (guard, liftM, liftM2, void, when)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Either (EitherT(..))
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.Monad.Trans.State (StateT, runStateT)
import Data.Functor.Identity (Identity(..))
import Data.IntMap (IntMap, (!))
import Data.IntSet (IntSet)
import Data.Map (Map)
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Monoid (Monoid(..))
import Data.Set (Set)
import Data.Store.Guid (Guid)
import qualified Control.Lens as Lens
import qualified Control.Lens.TH as LensTH
import qualified Control.Monad.Trans.Either as Either
import qualified Control.Monad.Trans.Reader as Reader
import qualified Data.IntMap.Lens as IntMapLens
import qualified Data.IntSet as IntSet
import qualified Data.IntSet.Lens as IntSetLens
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Store.Guid as Guid
import qualified Data.Traversable as Traversable
import qualified Editor.Data as Data

newtype Ref = Ref { unRef :: Int } deriving (Eq, Ord)
instance Show Ref where
  show = ('R' :) . show . unRef

data TypedValue = TypedValue
  { tvVal :: Ref
  , tvType :: Ref
  }
instance Show TypedValue where
  show (TypedValue v t) = unwords [show v, ":", show t]

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

-- SimpleType Rule:
-- Type of a Lambda is a Pi, with same param type
-- Type of a Pi is Set
-- Type of Set is Set
-- Type of Builtin is what's stored in it
-- Type of a LiteralInteger is Integer

-- Apply Rule:
--
-- Apply =
-- Func                        Arg
-- ----                        ---
-- FuncT = ParamT -> ResultT   ArgT
-- --------------------------------
-- ApplyT
--
-- ParamT <=> ArgT
-- Recurse-Subst ResultT Arg ApplyT
-- If Arg is Get Param:
--   ApplyT <=> ResultT
-- Case Func of
--   Hole -> Do Nothing
--   \LParamT -> Body : BodyT:
--     LParamT <=> ParamT
--     BodyT <=> ResultT
--     Recurse-Subst Body Arg Apply
--   Other -> Copy (Func Arg) to Apply
--
-- Where Recurse-Subst PreSubst Arg PostSubst
-- TODO

data ApplyComponents = ApplyComponents
  { _acApply :: TypedValue
  , _acFunc :: TypedValue
  , _acArg :: TypedValue
  } deriving (Show)

-- LambdaOrPi Rule:
-- Each of expr parts:
--   Part <=> Stored

data LambdaComponents = LambdaComponents
  { _lcParent :: Ref
  , _lcParamType :: Ref
  , _lcResult :: Ref
  } deriving (Show)

-- LambdaBodyType Rule:
--
-- (? -> Body Type) => LambdaT
-- Result Type of LambdaT => Body Type

data LambdaBodyType = LambdaBodyType
  { _ltLambdaGuid :: Guid
  , _ltLambdaType :: Ref
  , _ltBodyType :: Ref
  } deriving Show

-- Union rule (type of get param, but also for recursive type)

newtype RefExprPayload = RefExprPayload
  { _pSubstitutedArgs :: IntSet
  } deriving (Show, Monoid)

type RefExpression = Data.Expression RefExprPayload

refExprFromPure :: Data.PureExpression -> RefExpression
refExprFromPure = fmap $ const mempty

newtype Rule = Rule { unRule :: forall m. Monad m => InferT m () }

data RefData = RefData
  { _rExpression :: RefExpression
  , _rRules :: [Rule]
  }

makeRefExpression :: Guid -> Data.ExpressionBody RefExpression -> RefExpression
makeRefExpression g expr =
  Data.Expression g expr mempty

augmentGuid :: String -> Guid -> Guid
augmentGuid = Guid.combine . Guid.fromString

makeHole :: String -> Guid -> RefExpression
makeHole s g =
  makeRefExpression (augmentGuid s g) $ Data.ExpressionLeaf Data.Hole

setExpr :: Data.PureExpression
setExpr =
  Data.pureExpression (Guid.fromString "SettySet") $ Data.ExpressionLeaf Data.Set

intTypeExpr :: Data.PureExpression
intTypeExpr =
  Data.pureExpression (Guid.fromString "IntyInt") $ Data.ExpressionLeaf Data.IntegerType

emptyRefData :: RefData
emptyRefData = RefData
  { _rRules = []
  , _rExpression =
      makeRefExpression (Guid.fromString "NotYetInit") $ Data.ExpressionLeaf Data.Hole
  }

data RefMap = RefMap
  { _refMap :: IntMap RefData
  , _nextInt :: Int
  }

data InferState = InferState
  { _sRefMap :: RefMap
  , _sTouchedRefs :: IntSet
  }

-- Map from params to their Param type,
-- also including the recursive ref to the definition.
-- (hence not just parameters)
type Scope = Map Data.VariableRef Ref

-- Used to refer to expressions in the inference state and resume inference.
data InferNode = InferNode
  { nRefs :: TypedValue
  , nScope :: Scope
  }

data Inferred a = Inferred
  { iStored :: a
  , iValue :: Data.PureExpression
  , iType :: Data.PureExpression
  , iScope :: Map Guid Data.PureExpression
  , iPoint :: InferNode
  } deriving Functor

type Expression a = Data.Expression (Inferred a)

data ErrorDetails
  = MismatchIn Data.PureExpression Data.PureExpression
  | InfiniteExpression Data.PureExpression
  deriving Show

data Error = Error
  { errRef :: Ref
  , errMismatch :: (Data.PureExpression, Data.PureExpression)
  , errDetails :: ErrorDetails
  } deriving Show

newtype InferActions m = InferActions
  { reportError :: Error -> m ()
  }

newtype InferT m a =
  InferT { unInferT :: ReaderT (InferActions m) (StateT InferState m) a }
  deriving (Monad)

LensTH.makeLenses ''RefExprPayload
LensTH.makeLenses ''RefData
LensTH.makeLenses ''RefMap
LensTH.makeLenses ''InferState

runInferT ::
  InferActions m -> InferState ->
  InferT m a -> m (a, InferState)
runInferT actions state =
  (`runStateT` state) . (`runReaderT` actions) . unInferT

liftActions :: ReaderT (InferActions m) (StateT InferState m) a -> InferT m a
liftActions = InferT

liftState :: Monad m => StateT InferState m a -> InferT m a
liftState = liftActions . lift

instance MonadTrans InferT where
  lift = liftState . lift

-- TODO: createTypeVal should use newNode, not vice versa.
-- For use in loading phase only!
-- We don't create additional Refs afterwards!
createTypedVal :: Monad m => InferT m TypedValue
createTypedVal =
  liftM2 TypedValue createRef createRef
  where
    createRef = liftState $ do
      key <- Lens.use (sRefMap . nextInt)
      sRefMap . nextInt += 1
      sRefMap . refMap . IntMapLens.at key .= Just emptyRefData
      return $ Ref key

newNodeWithScope :: Scope -> RefMap -> (RefMap, InferNode)
newNodeWithScope scope prevRefMap =
  ( resultRefMap
  , InferNode tv scope
  )
  where
    Identity (tv, InferState resultRefMap _) =
      runInferT (error "not expecting use of actions")
      (InferState prevRefMap mempty) createTypedVal

initial :: (RefMap, InferNode)
initial = newNodeWithScope mempty $ RefMap mempty 0

newtype Loader m = Loader
  { loadPureDefinitionType :: Data.DefinitionIRef -> m Data.PureExpression
  }

-- Initial expression for inferred value and type of a stored entity.
-- Types are returned only in cases of expanding definitions.
initialExprs ::
  Monad m =>
  Loader m ->
  Scope -> Data.Expression s ->
  m (Data.PureExpression, Data.PureExpression)
initialExprs loader scope entity =
  (liftM . first)
  (Data.pureExpression (Data.eGuid entity)) $
  case exprStructure of
  Data.ExpressionApply _ -> return (Data.ExpressionLeaf Data.Hole, holeType)
  Data.ExpressionLeaf (Data.GetVariable var@(Data.DefinitionRef ref))
    | not (Map.member var scope) ->
      liftM ((,) exprStructure) $
      loadPureDefinitionType loader ref
  _ -> return (exprStructure, holeType)
  where
    innerHole =
      Data.pureExpression (augmentGuid "innerHole" (Data.eGuid entity)) $
      Data.ExpressionLeaf Data.Hole
    exprStructure = fmap (const innerHole) $ Data.eValue entity
    holeType =
      Data.pureExpression (augmentGuid "type" (Data.eGuid entity)) $
      Data.ExpressionLeaf Data.Hole

intMapMod :: Functor f => Int -> (v -> f v) -> IntMap v -> f (IntMap v)
intMapMod k =
  IntMapLens.at k . Lens.iso from Just
  where
    from = fromMaybe . error $ unwords ["intMapMod: key", show k, "not in map"]

refMapAt ::
  Functor f => Ref -> (RefData -> f RefData) -> InferState -> f InferState
refMapAt k = sRefMap . refMap . intMapMod (unRef k)

data MergeExprState = MergeExprState
  { _mGuidMapping :: Map Guid Guid
  , _mForbiddenGuids :: Set Guid
  }
LensTH.makeLenses ''MergeExprState

-- This is because platform's Either's Monad instance sucks
runEither :: EitherT l Identity a -> Either l a
runEither = runIdentity . runEitherT

guardEither :: Monad m => l -> Bool -> EitherT l m ()
guardEither err False = Either.left err
guardEither _ True = return ()

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
  runEither . runReaderT (go p0 p1) $ MergeExprState mempty mempty
  where
    -- When first is hole, we take Guids from second expr
    go
      (Data.Expression _ (Data.ExpressionLeaf Data.Hole) s0) e =
      mapParamGuids $ Data.atEPayload (mappend s0) e
    -- In all other cases guid comes from first expr
    go (Data.Expression g0 e0 s0) (Data.Expression g1 e1 s1) =
      fmap (flip (Data.Expression g0) s) .
      Reader.local
      ( (Lens.over mForbiddenGuids . Set.insert) g0
      . Lens.over mGuidMapping (Map.insert g1 g0)
      ) $ f (g0, g1) e0 e1
      where
        s = mappend s0 s1
    f _ e (Data.ExpressionLeaf Data.Hole) =
      return e
    f gs
      e0@(Data.ExpressionLeaf (Data.GetVariable (Data.ParameterRef par0)))
      e1@(Data.ExpressionLeaf (Data.GetVariable (Data.ParameterRef par1))) = do
      lift . guardEither (mismatchIn gs e0 e1) .
        (par0 ==) . fromMaybe par1
        =<< Reader.asks (Map.lookup par1 . Lens.view mGuidMapping)
      return $ Data.makeParameterRef par0
    f gs e0 e1 =
      case Data.matchExpressionBody go e0 e1 of
      Nothing -> lift . Either.left $ mismatchIn gs e0 e1
      Just body -> Traversable.sequence body
    mismatchIn (g0, g1) e0 e1 =
      MismatchIn
      (Data.pureExpression g0 (fmap void e0))
      (Data.pureExpression g1 (fmap void e1))
    mapParamGuids e@(Data.Expression g body s) = do
      lift . guardEither (InfiniteExpression (void e)) .
        not . Set.member g =<< Reader.asks (Lens.view mForbiddenGuids)
      fmap (flip (Data.Expression g) s) $ case body of
        Data.ExpressionLeaf (Data.GetVariable (Data.ParameterRef p)) ->
          liftM (Data.makeParameterRef . fromMaybe p) $
          Reader.asks (Map.lookup p . Lens.view mGuidMapping)
        _ -> Traversable.mapM mapParamGuids body

touch :: Monad m => Ref -> InferT m ()
touch ref =
  liftState $
  sTouchedRefs . IntSetLens.contains (unRef ref) .= True

setRefExpr :: Monad m => Ref -> RefExpression -> InferT m ()
setRefExpr ref newExpr = do
  curExpr <- liftState $ Lens.use (refMapAt ref . rExpression)
  case mergeExprs curExpr newExpr of
    Right mergedExpr -> do
      let
        isChange = not $ equiv mergedExpr curExpr
        isHole =
          case Data.eValue mergedExpr of
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
      isJust $ Traversable.sequence =<< Data.matchExpression compareSubsts x y
    compareSubsts x y = guard $ (x ^. pSubstitutedArgs) == (y ^. pSubstitutedArgs)

setRefExprPure :: Monad m => Ref -> Data.PureExpression -> InferT m ()
setRefExprPure ref = setRefExpr ref . refExprFromPure

addRules ::
  Monad m =>
  Scope -> TypedValue -> Guid -> Data.ExpressionBody TypedValue ->
  InferT m ()
addRules scope typedVal g exprBody = do
  liftState $ refMapAt (tvVal typedVal) . rRules %= (Rule (applyRuleSimpleType typedVal) :)
  case exprBody of
    Data.ExpressionPi lambda@(Data.Lambda _ resultType) -> do
      setRefExprPure (tvType resultType) setExpr
      onLambda Data.ExpressionPi maybePi lambda
    Data.ExpressionLambda lambda@(Data.Lambda _ body) -> do
      addRuleToMany [tvType typedVal, tvType body] .
        ruleLambdaBodyType $ LambdaBodyType g (tvType typedVal) (tvType body)
      onLambda Data.ExpressionLambda maybeLambda lambda
    Data.ExpressionApply (Data.Apply func arg) -> do
      addRuleToMany ([tvVal, tvType] <*> [typedVal, func, arg]) .
        ruleApply $ ApplyComponents typedVal func arg
      -- make sure we invoke this rule
      touch $ tvVal typedVal
    Data.ExpressionLeaf (Data.GetVariable var) ->
      case Map.lookup var scope of
      Nothing -> return ()
      Just ref -> do
        addUnionRule ref $ tvType typedVal
        -- make sure we invoke this rule.
        touch $ tvType typedVal
    _ -> return ()
  where
    addUnionRule x y = addRuleToMany [x, y] $ ruleUnion x y
    addRule rule ref =
      liftState $ refMapAt ref . rRules %= (rule :)
    addRuleToMany refs rule = mapM_ (addRule rule) refs
    onLambda cons uncons (Data.Lambda paramType result) = do
      setRefExprPure (tvType paramType) setExpr
      addRuleToMany [tvVal typedVal, tvVal paramType, tvVal result] .
        structureRule cons uncons $
        LambdaComponents (tvVal typedVal) (tvVal paramType) (tvVal result)

nodeFromEntity ::
  Monad m =>
  Loader m -> Scope ->
  Data.Expression s -> TypedValue ->
  InferT m (Data.Expression (s, InferNode))
nodeFromEntity loader scope entity typedValue = do
  setInitialValues
  bodyWithChildrenTvs <- Traversable.mapM addTypedVal $ Data.eValue entity
  exprBody <-
    case bodyWithChildrenTvs of
    Data.ExpressionLambda lambda ->
      onLambda Data.ExpressionLambda lambda
    Data.ExpressionPi lambda ->
      onLambda Data.ExpressionPi lambda
    _ -> Traversable.mapM (go id) bodyWithChildrenTvs
  addRules scope typedValue (Data.eGuid entity) $ fmap snd bodyWithChildrenTvs
  return $
    Data.Expression (Data.eGuid entity) exprBody
    (Data.ePayload entity, InferNode typedValue scope)
  where
    onLambda cons (Data.Lambda paramType@(_, paramTypeTv) result) = do
      paramTypeR <- go id paramType
      let paramRef = Data.ParameterRef $ Data.eGuid entity
      liftM (cons . Data.Lambda paramTypeR) $
        go (Map.insert paramRef (tvVal paramTypeTv)) result
    go onScope = uncurry . nodeFromEntity loader $ onScope scope
    addTypedVal x =
      liftM ((,) x) createTypedVal
    setInitialValues = do
      (initialVal, initialType) <-
        lift $ initialExprs loader scope entity
      setRefExpr (tvVal typedValue) $ refExprFromPure initialVal
      setRefExpr (tvType typedValue) $ refExprFromPure initialType

popTouchedRef :: Monad m => InferT m (Maybe Ref)
popTouchedRef = do
  touched <- liftState $ Lens.use sTouchedRefs
  case IntSet.minView touched of
    Nothing -> return Nothing
    Just (key, newTouchedRefs) -> do
      liftState $ sTouchedRefs .= newTouchedRefs
      return . Just $ Ref key

infer :: Monad m => InferActions m -> InferState -> m RefMap
infer actions state =
  liftM (Lens.view sRefMap . snd) $ runInferT actions state go
  where
    go = maybe (return ()) goOn =<< popTouchedRef
    goOn ref = do
      mapM_ unRule =<< (liftState . Lens.use) (refMapAt ref . rRules)
      go

getRefExpr :: Monad m => Ref -> InferT m RefExpression
getRefExpr ref = liftState $ Lens.use (refMapAt ref . rExpression)

structureRule ::
  (Data.Lambda RefExpression -> Data.ExpressionBody RefExpression) ->
  (Data.ExpressionBody RefExpression -> Maybe (Data.Lambda RefExpression)) ->
  LambdaComponents ->
  Rule
structureRule cons uncons (LambdaComponents parentRef paramTypeRef resultRef) = Rule $ do
  Data.Expression g expr _ <- getRefExpr parentRef
  case uncons expr of
    Nothing -> return ()
    Just (Data.Lambda paramType result) -> do
      setRefExpr paramTypeRef paramType
      setRefExpr resultRef result
  setRefExpr parentRef . makeRefExpression g . cons =<<
    liftM2 Data.Lambda (getRefExpr paramTypeRef) (getRefExpr resultRef)

subst ::
  Guid -> Data.Expression a ->
  Data.Expression a -> Data.Expression a
subst from to expr =
  case Data.eValue expr of
  Data.ExpressionLeaf (Data.GetVariable (Data.ParameterRef g))
    | g == from -> to
  _ ->
    (Data.atEValue . fmap)
    (subst from to) expr

mergeToPiResult ::
  RefExpression -> RefExpression -> RefExpression
mergeToPiResult e0@(Data.Expression _ (Data.ExpressionLeaf Data.Hole) s) e1
  | IntSet.null substs = e0
  | otherwise = fmap (Lens.set pSubstitutedArgs substs) e1
  where
    substs = s ^. pSubstitutedArgs
mergeToPiResult e0@(Data.Expression g b s) e1 =
  case Data.matchExpressionBody mergeToPiResult b (Data.eValue e1) of
  Nothing -> e0
  Just newB -> Data.Expression g newB s

recurseSubst ::
  Monad m =>
  RefExpression -> Guid -> Ref -> Ref ->
  InferT m RefExpression
recurseSubst preSubstExpr paramGuid argRef postSubstRef = do
  -- PreSubst with Subst => PostSubst
  setRefExpr postSubstRef .
    flip (subst paramGuid) preSubstExpr .
    (fmap . Lens.over pSubstitutedArgs . IntSet.insert . unRef) argRef =<<
    getRefExpr argRef

  postSubstExpr <- getRefExpr postSubstRef
  -- Recurse over PreSubst and PostSubst together
  --   When PreSubst part refers to its param:
  --     PostSubst part <=> arg
  mergeToArg preSubstExpr postSubstExpr

  return $ mergeToPiResult preSubstExpr postSubstExpr
  where
    mergeToArg pre post =
      case Data.eValue pre of
      Data.ExpressionLeaf (Data.GetVariable (Data.ParameterRef g))
        | g == paramGuid ->
          setRefExpr argRef $
          (fmap . Lens.over pSubstitutedArgs . IntSet.delete . unRef) argRef post
      preExpr ->
        case Data.matchExpressionBody mergeToArg preExpr (Data.eValue post) of
        Just x -> do
          _ <- Traversable.sequence x
          return ()
        Nothing -> return ()

maybeLambda :: Data.ExpressionBody a -> Maybe (Data.Lambda a)
maybeLambda (Data.ExpressionLambda x) = Just x
maybeLambda _ = Nothing

maybePi :: Data.ExpressionBody a -> Maybe (Data.Lambda a)
maybePi (Data.ExpressionPi x) = Just x
maybePi _ = Nothing

ruleUnion :: Ref -> Ref -> Rule
ruleUnion r0 r1 = Rule $ do
  setRefExpr r0 =<< getRefExpr r1
  setRefExpr r1 =<< getRefExpr r0

applyRuleSimpleType :: Monad m => TypedValue -> InferT m ()
applyRuleSimpleType (TypedValue val typ) = do
  Data.Expression g valExpr _ <- getRefExpr val
  case valExpr of
    Data.ExpressionLeaf Data.Set -> setRefExprPure typ setExpr
    Data.ExpressionLeaf Data.IntegerType -> setRefExprPure typ setExpr
    Data.ExpressionLeaf (Data.LiteralInteger _) -> setRefExprPure typ intTypeExpr
    Data.ExpressionPi _ -> setRefExprPure typ setExpr
    Data.ExpressionLambda (Data.Lambda paramType _) ->
      setRefExpr typ . makeRefExpression g .
      Data.makePi paramType $ makeHole "lambdaBody" g
    _ -> return ()

ruleLambdaBodyType :: LambdaBodyType -> Rule
ruleLambdaBodyType (LambdaBodyType lambdaG lambdaType bodyType) = Rule $ do
  Data.Expression piG lambdaTExpr _ <- getRefExpr lambdaType

  setRefExpr lambdaType . makeRefExpression lambdaG .
    Data.makePi (makeHole "paramType" piG) =<< getRefExpr bodyType

  case lambdaTExpr of
    Data.ExpressionPi (Data.Lambda _ resultType) ->
      setRefExpr bodyType $ subst piG
      ( makeRefExpression (Guid.fromString "getVar")
        (Data.makeParameterRef lambdaG)
      )
      resultType
    _ -> return ()

ruleApply :: ApplyComponents -> Rule
ruleApply (ApplyComponents apply func arg) = Rule $ do
  applyTypeExpr <- getRefExpr $ tvType apply
  let baseGuid = Data.eGuid applyTypeExpr

  -- ArgT => ParamT
  setRefExpr (tvType func) . makeRefExpression (augmentGuid "ar0" baseGuid) .
    (`Data.makePi` makeHole "ar1" baseGuid) =<< getRefExpr (tvType arg)

  -- If Arg is GetParam
  -- ApplyT (Susbt Arg with Hole) => ResultT
  -- The other direction is handled anyhow below

  argExpr <- getRefExpr $ tvVal arg
  case Data.eValue argExpr of
    Data.ExpressionLeaf (Data.GetVariable (Data.ParameterRef par)) ->
      setRefExpr (tvType func) . makeRefExpression (augmentGuid "ar2" baseGuid) .
      Data.makePi (makeHole "ar3" baseGuid) $
      subst par (makeHole "ar7" baseGuid) applyTypeExpr
    _ -> return ()

  Data.Expression funcTGuid funcTExpr _ <-
    getRefExpr $ tvType func
  case funcTExpr of
    Data.ExpressionPi (Data.Lambda paramT resultT) -> do
      -- ParamT => ArgT
      setRefExpr (tvType arg) paramT
      -- Recurse-Subst ResultT Arg ApplyT
      setRefExpr (tvType func) . makeRefExpression funcTGuid .
        Data.makePi paramT =<<
        recurseSubst resultT funcTGuid (tvVal arg) (tvType apply)
    _ -> return ()

  funcPge@(Data.Expression funcGuid funcExpr _) <-
    getRefExpr $ tvVal func
  case funcExpr of
    Data.ExpressionLambda (Data.Lambda paramT body) -> do
      -- ParamT => ArgT
      setRefExpr (tvType arg) paramT
      -- ArgT => ParamT
      setRefExpr (tvVal func) . makeRefExpression (augmentGuid "ar4" baseGuid) .
        (`Data.makeLambda` makeHole "ar5" baseGuid) =<< getRefExpr (tvType arg)
      -- Recurse-Subst Body Arg Apply
      setRefExpr (tvVal func) . makeRefExpression funcGuid .
        Data.makeLambda paramT =<<
        recurseSubst body funcGuid (tvVal arg) (tvVal apply)
    Data.ExpressionLeaf Data.Hole -> return ()
    _ ->
      setRefExpr (tvVal apply) . makeRefExpression (augmentGuid "ar6" baseGuid) $
        Data.makeApply funcPge argExpr

inferFromEntity ::
  Monad m =>
  Loader m -> InferActions m ->
  RefMap -> InferNode ->
  Maybe Data.DefinitionIRef ->
  Data.Expression a ->
  m (Expression a, RefMap)
inferFromEntity loader actions initialRefMap (InferNode rootTv rootScope) mRecursiveDef expression = do
  (node, loadState) <- runInferT actions (InferState initialRefMap mempty) $
    nodeFromEntity loader scope expression rootTv
  resultRefMap <- infer actions loadState
  let
    derefNode (s, inferNode) =
      Inferred
      { iStored = s
      , iValue = deref . tvVal $ nRefs inferNode
      , iType = deref . tvType $ nRefs inferNode
      , iScope = Map.fromList . mapMaybe onScopeElement . Map.toList $ nScope inferNode
      , iPoint = inferNode
      }
    onScopeElement (Data.ParameterRef guid, ref) = Just (guid, deref ref)
    onScopeElement _ = Nothing
    deref (Ref x) = void $ ((resultRefMap ^. refMap) ! x) ^. rExpression
  return (fmap derefNode node, resultRefMap)
  where
    scope =
      case mRecursiveDef of
      Nothing -> rootScope
      Just iref ->
        Map.insert (Data.DefinitionRef iref) (tvType rootTv) rootScope
