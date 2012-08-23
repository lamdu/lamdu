{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving, TemplateHaskell #-}
module Editor.Data.Typed
  ( Expression, Inferred(..), rExpression
  , Conflict
  , RefMap, Ref
  , Loader(..), InferActions(..)
  , inferFromEntity
  ) where

import Control.Applicative ((<*>))
import Control.Arrow (first, second)
import Control.Lens ((%=), (.=), (^.))
import Control.Monad (guard, liftM, liftM2, void, when)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.Monad.Trans.State (StateT, runStateT)
import Data.IntMap (IntMap, (!))
import Data.IntSet (IntSet)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Monoid (mempty, mappend)
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

-- Each node has set of refs to substituted args.
type RefExpression = Data.Expression IntSet

refExprFromPure :: Data.PureExpression -> RefExpression
refExprFromPure = fmap $ const mempty

data RefData = RefData
  { _rExpression :: RefExpression
  , _rRules :: [Rule]
  } deriving (Show)
LensTH.makeLenses ''RefData

hole :: Data.PureExpression
hole =
  Data.pureExpression (Guid.fromString "HoleyHole") $ Data.ExpressionLeaf Data.Hole

holeRefExpr :: RefExpression
holeRefExpr = refExprFromPure hole

setExpr :: Data.PureExpression
setExpr =
  Data.pureExpression (Guid.fromString "SettySet") $ Data.ExpressionLeaf Data.Set

intTypeExpr :: Data.PureExpression
intTypeExpr =
  Data.pureExpression (Guid.fromString "IntyInt") $ Data.ExpressionLeaf Data.IntegerType

emptyRefData :: RefData
emptyRefData = RefData
  { _rRules = []
  , _rExpression = holeRefExpr
  }

newtype RefMap = RefMap { _refMap :: IntMap RefData }
LensTH.makeLenses ''RefMap

data InferState = InferState
  { _sRefMap :: RefMap
  , _sTouchedRefs :: IntSet
  }
LensTH.makeLenses ''InferState

instance Show RefMap where
  show =
    List.intercalate ", " . map (showPair . first Ref) .
    IntMap.toList . Lens.view refMap
    where
      showPair (x, y) = show x ++ "=>" ++ show y

-- Used to refer to expressions in the inference state and resume inference.
data InferNode = InferNode
  { nRefs :: TypedValue
  , nScope :: Scope
  }

data Inferred s = Inferred
  { iStored :: s
  -- Refs used to match with conflicts
  , iValue :: (Ref, Data.PureExpression)
  , iType :: (Ref, Data.PureExpression)
  -- Used to resume inference.
  , iScope :: Scope
  }

type Expression s = Data.Expression (Inferred s)

-- Map from params to their Param type,
-- also including the recursive ref to the definition.
-- (hence not just parameters)
type Scope = Map Data.VariableRef Ref

newtype Loader m = Loader
  { loadPureDefinitionType :: Data.DefinitionIRef -> m Data.PureExpression
  }

-- Initial expression for inferred value and type of a stored entity.
-- Types are returned only in cases of expanding definitions.
initialExprs ::
  Monad m =>
  Loader m ->
  Scope -> Data.Expression s ->
  m (Bool, (Data.PureExpression, Data.PureExpression))
initialExprs loader scope entity =
  (liftM . second . first)
  (Data.pureExpression (Data.eGuid entity)) $
  case exprStructure of
  Data.ExpressionApply _ -> return (True, (Data.ExpressionLeaf Data.Hole, hole))
  Data.ExpressionLeaf (Data.GetVariable var@(Data.DefinitionRef ref))
    | not (Map.member var scope) ->
      liftM ((,) False . (,) exprStructure) $
      loadPureDefinitionType loader ref
  _ -> return (False, (exprStructure, hole))
  where
    exprStructure = fmap (const hole) $ Data.eValue entity

intMapMod :: Functor f => Int -> (v -> f v) -> (IntMap v -> f (IntMap v))
intMapMod k =
  IntMapLens.at k . Lens.iso from Just
  where
    from = fromMaybe . error $ unwords ["intMapMod: key", show k, "not in map"]

refMapAt ::
  Functor f => Ref -> (RefData -> f RefData) -> (InferState -> f InferState)
refMapAt k = sRefMap . refMap . intMapMod (unRef k)

makeRefExpression :: Guid -> Data.ExpressionBody RefExpression -> RefExpression
makeRefExpression g expr = Data.Expression g expr mempty

-- Merge two expressions:
-- If they do not match, return Nothing.
-- Holes match with anything, expand to the other expr.
-- Guids come from the first expression (where available).
mergeExprs ::
  RefExpression ->
  RefExpression ->
  Maybe RefExpression
mergeExprs p0 p1 =
  runReaderT (go p0 p1) Map.empty
  where
    go e0 e1 =
      (fmap . Data.atEPayload . const)
      (mappend (Data.ePayload e0) (Data.ePayload e1)) $
      f e0 e1
    f e (Data.Expression _ (Data.ExpressionLeaf Data.Hole) _) =
      return e
    -- When first is hole, we take Guids from second expr
    f
      (Data.Expression _ (Data.ExpressionLeaf Data.Hole) _)
      (Data.Expression g e _) =
      fmap (makeRefExpression g) $
      -- Map Param Guids to those of first expression
      Traversable.mapM (go holeRefExpr) e
    f
      e0@(Data.Expression _ (Data.ExpressionLeaf (Data.GetVariable (Data.ParameterRef par0))) _)
      (Data.Expression _ (Data.ExpressionLeaf (Data.GetVariable (Data.ParameterRef par1))) _) = do
      guard . (par0 ==) =<< lift =<< Reader.asks (Map.lookup par1)
      return e0
    f
      (Data.Expression g0 e0 _)
      (Data.Expression g1 e1 _) =
      fmap (makeRefExpression g0) .
      Reader.local (Map.insert g1 g0) $
      Traversable.sequence =<< lift (Data.matchExpressionBody go e0 e1)

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

touch :: Monad m => Ref -> InferT m ()
touch ref =
  liftState $
  sTouchedRefs . IntSetLens.contains (unRef ref) .= True

setRefExpr :: Monad m => Ref -> RefExpression -> InferT m ()
setRefExpr ref newExpr = do
  curExpr <- liftState $ Lens.use (refMapAt ref . rExpression)
  case mergeExprs curExpr newExpr of
    Just mergedExpr ->
      when (mergedExpr /= curExpr) $ do
        touch ref
        liftState $ refMapAt ref . rExpression .= mergedExpr
    Nothing -> do
      report <- liftActions $ Reader.asks reportConflict
      lift . report ref . Data.canonizeGuids $ void newExpr

setRefExprPure :: Monad m => Ref -> Data.PureExpression -> InferT m ()
setRefExprPure ref = setRefExpr ref . refExprFromPure

addRules ::
  Monad m =>
  Scope -> TypedValue -> Guid -> Data.ExpressionBody TypedValue ->
  InferT m ()
addRules scope typedVal g exprBody = do
  liftState $ refMapAt (tvVal typedVal) . rRules %= (RuleSimpleType typedVal :)
  case exprBody of
    Data.ExpressionPi lambda ->
      onLambda RulePiStructure lambda
    Data.ExpressionLambda lambda@(Data.Lambda _ body) -> do
      addRuleToMany [tvType typedVal, tvType body] .
        RuleLambdaBodyType $
        LambdaBodyType g (tvType typedVal) (tvType body)
      onLambda RuleLambdaStructure lambda
    Data.ExpressionApply (Data.Apply func arg) ->
      addRuleToMany ([tvVal, tvType] <*> [typedVal, func, arg]) .
        RuleApply $ ApplyComponents typedVal func arg
    Data.ExpressionLeaf (Data.GetVariable var) ->
      case Map.lookup var scope of
      Nothing -> return ()
      Just ref -> addUnionRule ref $ tvType typedVal
    _ -> return ()
  where
    addUnionRule x y = addRuleToMany [x, y] $ RuleUnion x y
    addRule rule ref =
      liftState $ refMapAt ref . rRules %= (rule :)
    addRuleToMany refs rule = mapM_ (addRule rule) refs
    onLambda cons (Data.Lambda paramType result) =
      addRuleToMany [tvVal typedVal, tvVal paramType, tvVal result] .
      cons $ LambdaComponents (tvVal typedVal) (tvVal paramType) (tvVal result)

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

nodeFromEntity ::
  Monad m =>
  Loader m -> Scope ->
  Data.Expression s -> TypedValue ->
  InferT m (Data.Expression (s, InferNode))
nodeFromEntity loader scope entity typedValue = do
  setInitialValues
  bodyWithChildrenTvs <- Traversable.mapM addTypedVal $ Data.eValue entity
  addRules scope typedValue (Data.eGuid entity) $ fmap snd bodyWithChildrenTvs
  exprBody <-
    case bodyWithChildrenTvs of
    Data.ExpressionLambda lambda ->
      onLambda Data.ExpressionLambda lambda
    Data.ExpressionPi lambda@(Data.Lambda _ (_, resultTypeTv)) -> do
      setRefExprPure (tvType resultTypeTv) setExpr
      onLambda Data.ExpressionPi lambda
    _ -> Traversable.mapM (go id) bodyWithChildrenTvs
  return $
    Data.Expression (Data.eGuid entity) exprBody
    (Data.ePayload entity, InferNode typedValue scope)
  where
    onLambda cons (Data.Lambda paramType@(_, paramTypeTv) result) = do
      setRefExprPure (tvType paramTypeTv) setExpr
      paramTypeR <- go id paramType
      let paramRef = Data.ParameterRef $ Data.eGuid entity
      liftM (cons . Data.Lambda paramTypeR) $
        go (Map.insert paramRef (tvVal paramTypeTv)) result
    go onScope = uncurry . nodeFromEntity loader $ onScope scope
    addTypedVal x =
      liftM ((,) x) createTypedVal
    setInitialValues = do
      (isTouched, (initialVal, initialType)) <-
        lift $ initialExprs loader scope entity
      when isTouched . touch $ tvVal typedValue
      setRefExprPure (tvVal typedValue) initialVal
      setRefExprPure (tvType typedValue) initialType

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
  | IntSet.null s = e0
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
    (fmap . mappend . IntSet.singleton . unRef) argRef =<<
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
          (fmap . IntSet.delete . unRef) argRef post
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
      setRefExpr typ . makeRefExpression g $
      Data.makePi paramType holeRefExpr
    _ -> return ()
applyRule (RuleLambdaBodyType (LambdaBodyType lambdaG lambdaType bodyType)) = do
  setRefExpr lambdaType . makeRefExpression lambdaG .
    Data.makePi holeRefExpr =<< getRefExpr bodyType
  Data.Expression piG lambdaTExpr _ <- getRefExpr lambdaType
  case lambdaTExpr of
    Data.ExpressionPi (Data.Lambda _ resultType) ->
      setRefExpr bodyType $ subst piG
      ( makeRefExpression (Guid.fromString "getVar")
        (Data.makeParameterRef lambdaG)
      )
      resultType
    _ -> return ()
applyRule (RuleApply (ApplyComponents apply func arg)) = do
  -- ArgT => ParamT
  setRefExpr (tvType func) . makeRefExpression someGuid .
    (`Data.makePi` holeRefExpr) =<< getRefExpr (tvType arg)
  -- If Arg is GetParam, ApplyT <=> ResultT
  argExpr <- getRefExpr $ tvVal arg
  case Data.eValue argExpr of
    Data.ExpressionLeaf (Data.GetVariable (Data.ParameterRef _)) -> do
      setRefExpr (tvType func) . makeRefExpression someGuid .
        Data.makePi holeRefExpr =<< getRefExpr (tvType apply)
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
      setRefExpr (tvVal func) . makeRefExpression someGuid .
        (`Data.makeLambda` holeRefExpr) =<< getRefExpr (tvType arg)
      -- Recurse-Subst Body Arg Apply
      setRefExpr (tvVal func) . makeRefExpression funcGuid .
        Data.makeLambda paramT =<<
        recurseSubst body funcGuid (tvVal arg) (tvVal apply)
    Data.ExpressionLeaf Data.Hole -> return ()
    _ ->
      setRefExpr (tvVal apply) . makeRefExpression someGuid $
        Data.makeApply funcPge argExpr
  where
    someGuid = Guid.fromString "Arbitrary"

inferFromEntity ::
  Monad m =>
  Loader m -> InferActions m ->
  Maybe Data.DefinitionIRef ->
  Data.Expression s ->
  m (Expression s, RefMap)
inferFromEntity loader actions mRecursiveDef expression = do
  (node, loadState) <- runInferT actions (InferState (RefMap mempty) mempty) $ do
    rootTv <- createTypedVal
    let
      scope =
        case mRecursiveDef of
        Nothing -> mempty
        Just iref -> Map.singleton (Data.DefinitionRef iref) (tvType rootTv)
    nodeFromEntity loader scope expression rootTv
  resultRefMap <- infer actions loadState
  let
    derefNode (s, inferNode) =
      Inferred
      { iStored = s
      , iValue = withDeref . tvVal $ nRefs inferNode
      , iType = withDeref . tvType $ nRefs inferNode
      , iScope = nScope inferNode
      }
    withDeref ref = (ref, deref ref)
    deref (Ref x) = void $ ((resultRefMap ^. refMap) ! x) ^. rExpression
  return (fmap derefNode node, resultRefMap)
