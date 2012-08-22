{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts, TemplateHaskell, DeriveFunctor #-}
module Editor.Data.Typed
  ( Expression, Inferred(..)
  , InferredExpr(..), rExpression, rErrors
  , Conflict
  , RefMap, Loader(..)
  , inferFromEntity
  ) where

import Control.Applicative ((<*>))
import Control.Arrow (first, second)
import Control.Lens ((%=), (.=), (^.))
import Control.Monad (guard, liftM, liftM2, mzero, when)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.State.Class (MonadState)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.Monad.Trans.State (StateT, execState, runStateT)
import Data.IntMap (IntMap, (!))
import Data.IntSet (IntSet)
import Data.Map (Map)
import Data.Maybe (fromMaybe, isJust)
import Data.Monoid (mempty)
import Data.Store.Guid (Guid)
import qualified Control.Lens as Lens
import qualified Control.Lens.TH as LensTH
import qualified Control.Monad.Reader.Class as Reader
import qualified Data.IntMap as IntMap
import qualified Data.IntMap.Lens as IntMapLens
import qualified Data.IntSet as IntSet
import qualified Data.IntSet.Lens as IntSetLens
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Store.Guid as Guid
import qualified Data.Traversable as Traversable
import qualified Editor.Data as Data

newtype Ref = Ref { unRef :: Int }
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

data InferredExpr = InferredExpr
  { _rExpression :: Data.PureExpression
  , _rErrors :: [Conflict]
  } deriving Show
LensTH.makeLenses ''InferredExpr

data RefData = RefData
  { _rResult :: InferredExpr
  , _rRules :: [Rule]
  } deriving (Show)
LensTH.makeLenses ''RefData

hole :: Data.PureExpression
hole =
  Data.pureExpression (Guid.fromString "HoleyHole") $ Data.ExpressionLeaf Data.Hole

setExpr :: Data.PureExpression
setExpr =
  Data.pureExpression (Guid.fromString "SettySet") $ Data.ExpressionLeaf Data.Set

intTypeExpr :: Data.PureExpression
intTypeExpr =
  Data.pureExpression (Guid.fromString "IntyInt") $ Data.ExpressionLeaf Data.IntegerType

emptyRefData :: RefData
emptyRefData = RefData
  { _rRules = []
  , _rResult =
      InferredExpr
      { _rExpression = hole
      , _rErrors = []
      }
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
  , _nScope :: Scope
  }

data Inferred s = Inferred
  { iStored :: s
  , iValue :: InferredExpr
  , iType :: InferredExpr
  , iInferNode :: InferNode
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

-- Merge two expressions:
-- If they do not match, return Nothing.
-- Holes match with anything, expand to the other expr.
-- Guids come from the first expression (where available).
mergeExprs ::
  Data.PureExpression ->
  Data.PureExpression ->
  Maybe Data.PureExpression
mergeExprs p0 p1 =
  runReaderT (go p0 p1) Map.empty
  where
    go
      (Data.Expression g0 e0 ())
      (Data.Expression g1 e1 ()) =
      fmap (Data.pureExpression g0) .
      Reader.local (Map.insert g1 g0) $
      case Data.matchExpressionBody go e0 e1 of
      Just x -> Traversable.sequence x
      Nothing ->
        case (e0, e1) of
        (_, Data.ExpressionLeaf Data.Hole) -> return e0
        (Data.ExpressionLeaf Data.Hole, _) ->
          -- Map Guids to those of first expression
          Traversable.mapM (go hole) e1
        (Data.ExpressionLeaf (Data.GetVariable (Data.ParameterRef par0)),
         Data.ExpressionLeaf (Data.GetVariable (Data.ParameterRef par1))) -> do
          guard . (par0 ==) =<< lift =<< Reader.asks (Map.lookup par1)
          return e0
        _ -> mzero

touch :: MonadState InferState m => Ref -> m ()
touch ref = sTouchedRefs . IntSetLens.contains (unRef ref) .= True

addIfNotMatch :: (a -> a -> Bool) -> a -> [a] -> [a]
addIfNotMatch f x xs
  | any (f x) xs = xs
  | otherwise = x : xs

alphaEq :: Data.Expression a -> Data.Expression b -> Bool
alphaEq x = isJust . Data.matchExpression ((const . const) ()) x

setRefExpr ::
  MonadState InferState m =>
  Ref -> Data.PureExpression -> m ()
setRefExpr ref newExpr = do
  curExpr <- Lens.use $ refMapAt ref . rResult . rExpression
  case mergeExprs curExpr newExpr of
    Just mergedExpr ->
      when (mergedExpr /= curExpr) $ do
        touch ref
        refMapAt ref . rResult . rExpression .= mergedExpr
    Nothing -> refMapAt ref . rResult . rErrors %= addIfNotMatch alphaEq newExpr

addRules ::
  (MonadState InferState m, MonadReader Scope m) =>
  TypedValue -> Guid -> Data.ExpressionBody TypedValue -> m ()
addRules typedVal g exprBody = do
  refMapAt (tvVal typedVal) . rRules %= (RuleSimpleType typedVal :)
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
    Data.ExpressionLeaf (Data.GetVariable var) -> do
      mTypeRef <- Reader.asks $ Map.lookup var
      case mTypeRef of
        Nothing -> return ()
        Just ref -> addUnionRule ref $ tvType typedVal
    _ -> return ()
  where
    addUnionRule x y = addRuleToMany [x, y] $ RuleUnion x y
    addRule rule ref =
      refMapAt ref . rRules %= (rule :)
    addRuleToMany refs rule = mapM_ (addRule rule) refs
    onLambda cons (Data.Lambda paramType result) =
      addRuleToMany [tvVal typedVal, tvVal paramType, tvVal result] .
      cons $ LambdaComponents (tvVal typedVal) (tvVal paramType) (tvVal result)

-- For use in loading phase only!
-- We don't create additional Refs afterwards!
createTypedVal :: MonadState InferState m => m TypedValue
createTypedVal = liftM2 TypedValue createRef createRef
  where
    createRef = do
      key <- Lens.uses (sRefMap . refMap) IntMap.size
      sRefMap . refMap . IntMapLens.at key .= Just emptyRefData
      return $ Ref key

nodeFromEntity ::
  Monad m =>
  Loader m ->
  Data.Expression s -> TypedValue ->
  ReaderT (Map Data.VariableRef Ref) (StateT InferState m)
    (Data.Expression (s, InferNode))
nodeFromEntity loader entity typedValue = do
  setInitialValues
  bodyWithChildrenTvs <- Traversable.mapM addTypedVal $ Data.eValue entity
  addRules typedValue (Data.eGuid entity) $ fmap snd bodyWithChildrenTvs
  exprBody <-
    case bodyWithChildrenTvs of
    Data.ExpressionLambda lambda ->
      onLambda Data.ExpressionLambda lambda
    Data.ExpressionPi lambda@(Data.Lambda _ (_, resultTypeTv)) -> do
      setRefExpr (tvType resultTypeTv) setExpr
      onLambda Data.ExpressionPi lambda
    _ -> Traversable.mapM go bodyWithChildrenTvs
  liftM (Data.Expression (Data.eGuid entity) exprBody . (,) (Data.ePayload entity) . InferNode typedValue) Reader.ask
  where
    onLambda cons (Data.Lambda paramType@(_, paramTypeTv) result) = do
      setRefExpr (tvType paramTypeTv) setExpr
      paramTypeR <- go paramType
      let paramRef = Data.ParameterRef $ Data.eGuid entity
      liftM (cons . Data.Lambda paramTypeR) .
        Reader.local (Map.insert paramRef (tvVal paramTypeTv)) $
        go result
    go = uncurry (nodeFromEntity loader)
    addTypedVal x =
      liftM ((,) x) createTypedVal
    transaction = lift . lift
    setInitialValues = do
      scope <- Reader.ask
      (isTouched, (initialVal, initialType)) <-
        transaction $ initialExprs loader scope entity
      when isTouched . touch $ tvVal typedValue
      setRefExpr (tvVal typedValue) initialVal
      setRefExpr (tvType typedValue) initialType

fromEntity ::
  Monad m =>
  Loader m ->
  Maybe Data.DefinitionIRef ->
  Data.Expression s ->
  m (Data.Expression (s, InferNode), InferState)
fromEntity loader mRecursiveIRef rootEntity =
  (`runStateT` InferState (RefMap mempty) mempty) .
  (`runReaderT` mempty) $ do
    rootTv <- createTypedVal
    ( case mRecursiveIRef of
        Nothing -> id
        Just iref -> Reader.local $ Map.insert (Data.DefinitionRef iref) (tvType rootTv)
      ) $
      nodeFromEntity loader rootEntity rootTv

popTouchedRef :: MonadState InferState m => m (Maybe Ref)
popTouchedRef = do
  touched <- Lens.use sTouchedRefs
  case IntSet.minView touched of
    Nothing -> return Nothing
    Just (key, newTouchedRefs) -> do
      sTouchedRefs .= newTouchedRefs
      return . Just $ Ref key

infer :: InferState -> RefMap
infer =
  Lens.view sRefMap . execState go
  where
    go = maybe (return ()) goOn =<< popTouchedRef
    goOn ref = do
      mapM_ applyRule =<< Lens.use (refMapAt ref . rRules)
      go

getRefExpr :: MonadState InferState m => Ref -> m Data.PureExpression
getRefExpr ref = Lens.use (refMapAt ref . rResult . rExpression)

applyStructureRule ::
  MonadState InferState m =>
  (Data.Lambda Data.PureExpression -> Data.ExpressionBody Data.PureExpression) ->
  (Data.ExpressionBody Data.PureExpression -> Maybe (Data.Lambda Data.PureExpression)) ->
  LambdaComponents ->
  m ()
applyStructureRule cons uncons (LambdaComponents parentRef paramTypeRef resultRef) = do
  Data.Expression g expr () <- getRefExpr parentRef
  case uncons expr of
    Nothing -> return ()
    Just (Data.Lambda paramType result) -> do
      setRefExpr paramTypeRef paramType
      setRefExpr resultRef result
  setRefExpr parentRef . Data.pureExpression g . cons =<<
    liftM2 Data.Lambda (getRefExpr paramTypeRef) (getRefExpr resultRef)

subst ::
  Guid -> Data.PureExpression ->
  Data.PureExpression -> Data.PureExpression
subst from to expr =
  case Data.eValue expr of
  Data.ExpressionLeaf (Data.GetVariable (Data.ParameterRef g))
    | g == from -> to
  _ ->
    (Data.atEValue . fmap)
    (subst from to) expr

recurseSubst ::
  MonadState InferState m =>
  Data.PureExpression -> Guid -> Ref -> Ref -> m Data.PureExpression
recurseSubst preSubstExpr paramGuid argRef postSubstRef = do
  -- PreSubst with Subst => PostSubst
  -- TODO: Mark substituted holes..
  setRefExpr postSubstRef . flip (subst paramGuid) preSubstExpr =<<
    getRefExpr argRef
  -- Recurse over PreSubst and PostSubst together
  --   When PreSubst part refers to its param:
  --     PostSubst part <=> arg
  mergeToArg preSubstExpr =<< getRefExpr postSubstRef
  -- TODO: In some cases, we should merge into preSubstExpr
  return preSubstExpr
  where
    mergeToArg pre post =
      case Data.eValue pre of
      Data.ExpressionLeaf (Data.GetVariable (Data.ParameterRef g))
        | g == paramGuid -> setRefExpr argRef post
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

applyRule :: MonadState InferState m => Rule -> m ()
applyRule (RuleUnion r0 r1) = do
  setRefExpr r0 =<< getRefExpr r1
  setRefExpr r1 =<< getRefExpr r0
applyRule (RuleLambdaStructure lambda) =
  applyStructureRule Data.ExpressionLambda maybeLambda lambda
applyRule (RulePiStructure lambda) =
  applyStructureRule Data.ExpressionPi maybePi lambda
applyRule (RuleSimpleType (TypedValue val typ)) = do
  Data.Expression g valExpr () <- getRefExpr val
  case valExpr of
    Data.ExpressionLeaf Data.Set -> setRefExpr typ setExpr
    Data.ExpressionLeaf Data.IntegerType -> setRefExpr typ setExpr
    Data.ExpressionLeaf (Data.LiteralInteger _) -> setRefExpr typ intTypeExpr
    Data.ExpressionPi _ -> setRefExpr typ setExpr
    Data.ExpressionLambda (Data.Lambda paramType _) ->
      setRefExpr typ . Data.pureExpression g $
      Data.makePi paramType hole
    _ -> return ()
applyRule (RuleLambdaBodyType (LambdaBodyType lambdaG lambdaType bodyType)) = do
  setRefExpr lambdaType . Data.pureExpression lambdaG .
    Data.makePi hole =<< getRefExpr bodyType
  Data.Expression piG lambdaTExpr () <- getRefExpr lambdaType
  case lambdaTExpr of
    Data.ExpressionPi (Data.Lambda _ resultType) ->
      setRefExpr bodyType $ subst piG
      ( Data.pureExpression (Guid.fromString "getVar")
        (Data.makeParameterRef lambdaG)
      )
      resultType
    _ -> return ()
applyRule (RuleApply (ApplyComponents apply func arg)) = do
  -- ArgT => ParamT
  setRefExpr (tvType func) . Data.pureExpression someGuid .
    (`Data.makePi` hole) =<< getRefExpr (tvType arg)
  -- If Arg is GetParam, ApplyT <=> ResultT
  argExpr <- getRefExpr $ tvVal arg
  case Data.eValue argExpr of
    Data.ExpressionLeaf (Data.GetVariable (Data.ParameterRef _)) -> do
      setRefExpr (tvType func) . Data.pureExpression someGuid .
        Data.makePi hole =<< getRefExpr (tvType apply)
      funcTExpr <- getRefExpr $ tvType func
      case Data.eValue funcTExpr of
        Data.ExpressionPi (Data.Lambda _ resultT) ->
          setRefExpr (tvType apply) resultT
        _ -> return ()
    _ -> return ()

  Data.Expression funcTGuid funcTExpr () <-
    getRefExpr $ tvType func
  case funcTExpr of
    Data.ExpressionPi (Data.Lambda paramT resultT) -> do
      -- ParamT => ArgT
      setRefExpr (tvType arg) paramT
      -- Recurse-Subst ResultT Arg ApplyT
      setRefExpr (tvType func) . Data.pureExpression funcTGuid .
        Data.makePi paramT =<<
        recurseSubst resultT funcTGuid (tvVal arg) (tvType apply)
    _ -> return ()

  funcPge@(Data.Expression funcGuid funcExpr ()) <-
    getRefExpr $ tvVal func
  case funcExpr of
    Data.ExpressionLambda (Data.Lambda paramT body) -> do
      -- ParamT => ArgT
      setRefExpr (tvType arg) paramT
      -- ArgT => ParamT
      setRefExpr (tvVal func) . Data.pureExpression someGuid .
        (`Data.makeLambda` hole) =<< getRefExpr (tvType arg)
      -- Recurse-Subst Body Arg Apply
      setRefExpr (tvVal func) . Data.pureExpression funcGuid .
        Data.makeLambda paramT =<<
        recurseSubst body funcGuid (tvVal arg) (tvVal apply)
    Data.ExpressionLeaf Data.Hole -> return ()
    _ ->
      setRefExpr (tvVal apply) . Data.pureExpression someGuid $
        Data.makeApply funcPge argExpr
  where
    someGuid = Guid.fromString "Arbitrary"

inferFromEntity ::
  Monad m =>
  Loader m ->
  Maybe Data.DefinitionIRef ->
  Data.Expression s ->
  m (Expression s, RefMap)
inferFromEntity loader mRecursiveDef =
  liftM (derefNodes . second infer) . fromEntity loader mRecursiveDef
  where
    derefNodes (expr, state) =
      (fmap (derefNode state) expr, state)
    derefNode state (s, inferNode) =
      Inferred
      { iStored = s
      , iValue = deref state . tvVal $ nRefs inferNode
      , iType = deref state . tvType $ nRefs inferNode
      , iInferNode = inferNode
      }

deref :: RefMap -> Ref -> InferredExpr
deref (RefMap m) (Ref x) = (m ! x) ^. rResult
