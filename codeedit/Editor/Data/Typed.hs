{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving, TemplateHaskell #-}
module Editor.Data.Typed
  ( Expression, Inferred(..), rExpression
  , InferNode(..), TypedValue(..)
  , Conflict
  , RefMap, Ref
  , Loader(..), InferActions(..)
  , inferFromEntity, initial, newNodeWithScope
  ) where

import Control.Applicative ((<*>))
import Control.Arrow (first)
import Control.Lens ((%=), (.=), (^.))
import Control.Monad (guard, liftM, liftM2, void, when)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.Monad.Trans.State (StateT, runStateT)
import Data.Derive.Monoid (makeMonoid)
import Data.DeriveTH (derive)
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
import qualified Control.Monad.Trans.Reader as Reader
import qualified Data.IntMap as IntMap
import qualified Data.IntMap.Lens as IntMapLens
import qualified Data.IntSet as IntSet
import qualified Data.IntSet.Lens as IntSetLens
import qualified Data.List as List
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

data Rule
  = RuleSimpleType TypedValue
  | RuleUnion Ref Ref
  | RuleLambdaBodyType LambdaBodyType
  | RuleLambdaStructure LambdaComponents
  | RulePiStructure LambdaComponents
  | RuleApply ApplyComponents
  deriving (Show)

type Conflict = Data.PureExpression

data RefExprPayload = RefExprPayload
  { _pSubstitutedArgs :: IntSet
  , _pGuids :: Set Guid
  } deriving Show
LensTH.makeLenses ''RefExprPayload

derive makeMonoid ''RefExprPayload

type RefExpression = Data.Expression RefExprPayload

refExprFromPure :: Data.PureExpression -> RefExpression
refExprFromPure = fmap $ const mempty

data RefData = RefData
  { _rExpression :: RefExpression
  , _rRules :: [Rule]
  } deriving (Show)
LensTH.makeLenses ''RefData

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

newtype RefMap = RefMap { _refMap :: IntMap RefData }
LensTH.makeLenses ''RefMap

instance Show RefMap where
  show =
    List.intercalate ", " . map (showPair . first Ref) .
    IntMap.toList . Lens.view refMap
    where
      showPair (x, y) = show x ++ "=>" ++ show y

data InferState = InferState
  { _sRefMap :: RefMap
  , _sTouchedRefs :: IntSet
  } deriving Show
LensTH.makeLenses ''InferState

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

newtype InferActions m = InferActions
  { reportConflict :: Ref -> Data.PureExpression -> m ()
  }

newtype InferT m a =
  InferT { unInferT :: ReaderT (InferActions m) (StateT InferState m) a }
  deriving (Monad)

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
      key <- Lens.uses (sRefMap . refMap) IntMap.size
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
initial = newNodeWithScope mempty $ RefMap mempty

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

-- Merge two expressions:
-- If they do not match, return Nothing.
-- Holes match with anything, expand to the other expr.
-- Guids come from the first expression (where available).
-- If guids repeat, fail.
mergeExprs ::
  RefExpression ->
  RefExpression ->
  Maybe RefExpression
mergeExprs p0 p1 =
  runReaderT (go p0 p1) $ MergeExprState mempty mempty
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
      . (Lens.over mGuidMapping . mappend . Map.fromList . map (flip (,) g0) . Set.toList)
        (s ^. pGuids)
      ) $ f e0 e1
      where
        s = Lens.over pGuids (Set.insert g1) $ mappend s0 s1
    f e (Data.ExpressionLeaf Data.Hole) =
      return e
    f
      (Data.ExpressionLeaf (Data.GetVariable (Data.ParameterRef par0)))
      (Data.ExpressionLeaf (Data.GetVariable (Data.ParameterRef par1))) = do
      guard . (par0 ==) . fromMaybe par1
        =<< Reader.asks (Map.lookup par1 . Lens.view mGuidMapping)
      return $ Data.makeParameterRef par0
    f e0 e1 =
      Traversable.sequence =<< lift (Data.matchExpressionBody go e0 e1)
    mapParamGuids (Data.Expression g e s) = do
      guard . not . Set.member g =<< Reader.asks (Lens.view mForbiddenGuids)
      fmap (flip (Data.Expression g) s) $ case e of
        Data.ExpressionLeaf (Data.GetVariable (Data.ParameterRef p)) ->
          liftM (Data.makeParameterRef . fromMaybe p) $
          Reader.asks (Map.lookup p . Lens.view mGuidMapping)
        _ -> Traversable.mapM mapParamGuids e

touch :: Monad m => Ref -> InferT m ()
touch ref =
  liftState $
  sTouchedRefs . IntSetLens.contains (unRef ref) .= True

setRefExpr :: Monad m => Ref -> RefExpression -> InferT m ()
setRefExpr ref newExpr = do
  curExpr <- liftState $ Lens.use (refMapAt ref . rExpression)
  case mergeExprs curExpr newExpr of
    Just mergedExpr -> do
      let
        isChange = not $ alphaEq (void mergedExpr) (void curExpr)
        isHole =
          case Data.eValue mergedExpr of
          Data.ExpressionLeaf Data.Hole -> True
          _ -> False
      when isChange $ touch ref
      when (isChange || isHole) $
        liftState $ refMapAt ref . rExpression .= mergedExpr
    Nothing -> do
      report <- liftActions $ Reader.asks reportConflict
      lift . report ref . Data.canonizeGuids $ void newExpr
  where
    alphaEq x y = isJust $ Data.matchExpression ((const . const) ()) x y

setRefExprPure :: Monad m => Ref -> Data.PureExpression -> InferT m ()
setRefExprPure ref = setRefExpr ref . refExprFromPure

addRules ::
  Monad m =>
  Scope -> TypedValue -> Guid -> Data.ExpressionBody TypedValue ->
  InferT m ()
addRules scope typedVal g exprBody = do
  liftState $ refMapAt (tvVal typedVal) . rRules %= (RuleSimpleType typedVal :)
  case exprBody of
    Data.ExpressionPi lambda@(Data.Lambda _ resultType) -> do
      setRefExprPure (tvType resultType) setExpr
      onLambda RulePiStructure lambda
    Data.ExpressionLambda lambda@(Data.Lambda _ body) -> do
      addRuleToMany [tvType typedVal, tvType body] .
        RuleLambdaBodyType $
        LambdaBodyType g (tvType typedVal) (tvType body)
      onLambda RuleLambdaStructure lambda
    Data.ExpressionApply (Data.Apply func arg) -> do
      addRuleToMany ([tvVal, tvType] <*> [typedVal, func, arg]) .
        RuleApply $ ApplyComponents typedVal func arg
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
    addUnionRule x y = addRuleToMany [x, y] $ RuleUnion x y
    addRule rule ref =
      liftState $ refMapAt ref . rRules %= (rule :)
    addRuleToMany refs rule = mapM_ (addRule rule) refs
    onLambda cons (Data.Lambda paramType result) = do
      setRefExprPure (tvType paramType) setExpr
      addRuleToMany [tvVal typedVal, tvVal paramType, tvVal result] .
        cons $ LambdaComponents (tvVal typedVal) (tvVal paramType) (tvVal result)

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
      mapM_ applyRule =<< (liftState . Lens.use) (refMapAt ref . rRules)
      go

getRefExpr :: Monad m => Ref -> InferT m RefExpression
getRefExpr ref = liftState $ Lens.use (refMapAt ref . rExpression)

applyStructureRule ::
  Monad m =>
  (Data.Lambda RefExpression -> Data.ExpressionBody RefExpression) ->
  (Data.ExpressionBody RefExpression -> Maybe (Data.Lambda RefExpression)) ->
  LambdaComponents ->
  InferT m ()
applyStructureRule cons uncons (LambdaComponents parentRef paramTypeRef resultRef) = do
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
  | IntSet.null (s ^. pSubstitutedArgs) = e0
  -- do not propagate subst markings on the way up
  | otherwise = fmap (const mempty) e1
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
  -- TODO: Mark substituted holes..
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

applyRule :: Monad m => Rule -> InferT m ()
applyRule (RuleUnion r0 r1) = do
  setRefExpr r0 =<< getRefExpr r1
  setRefExpr r1 =<< getRefExpr r0
applyRule (RuleLambdaStructure lambda) =
  applyStructureRule Data.ExpressionLambda maybeLambda lambda
applyRule (RulePiStructure lambda) =
  applyStructureRule Data.ExpressionPi maybePi lambda
applyRule (RuleSimpleType (TypedValue val typ)) = do
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
applyRule (RuleLambdaBodyType (LambdaBodyType lambdaG lambdaType bodyType)) = do
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
applyRule (RuleApply (ApplyComponents apply func arg)) = do
  applyTypeExpr <- getRefExpr $ tvType apply
  let baseGuid = Data.eGuid applyTypeExpr

  -- ArgT => ParamT
  setRefExpr (tvType func) . makeRefExpression (augmentGuid "ar0" baseGuid) .
    (`Data.makePi` makeHole "ar1" baseGuid) =<< getRefExpr (tvType arg)
  -- If Arg is GetParam, ApplyT <=> ResultT
  argExpr <- getRefExpr $ tvVal arg
  case Data.eValue argExpr of
    Data.ExpressionLeaf (Data.GetVariable (Data.ParameterRef _)) -> do
      setRefExpr (tvType func) . makeRefExpression (augmentGuid "ar2" baseGuid) $
        Data.makePi (makeHole "ar3" baseGuid) applyTypeExpr
      funcTExpr <- getRefExpr $ tvType func
      case Data.eValue funcTExpr of
        Data.ExpressionPi (Data.Lambda _ resultT) ->
          setRefExpr (tvType apply) resultT
        _ -> return ()
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
      setRefExpr (tvVal apply) . makeRefExpression (augmentGuid "ar5" baseGuid) $
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
