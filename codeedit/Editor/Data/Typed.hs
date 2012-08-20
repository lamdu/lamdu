{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts, TemplateHaskell, DeriveFunctor #-}
module Editor.Data.Typed
  ( Expression(..), TypedValue(..)
  , RefMap, Ref, deref, Conflict
  , ExpressionEntity(..)
  , Loader(..)
  , inferFromEntity
  , alphaEq
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
import Data.Maybe (fromMaybe)
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
-- Add SimpleType, Apply, LambdaOrPi, Union rules
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

-- Union rule (type of get param, but also for recursive type)

data Rule
  = RuleSimpleType TypedValue
  | RuleUnion Ref Ref
  | RuleLambdaStructure LambdaComponents
  | RulePiStructure LambdaComponents
  | RuleApply ApplyComponents
  deriving (Show)

type Conflict = Data.PureGuidExpression

data RefData = RefData
  { _rExpression :: Data.PureGuidExpression
  , _rRules :: [Rule]
  , _rErrors :: [Conflict]
  } deriving (Show)
LensTH.makeLenses ''RefData

hole :: Data.PureGuidExpression
hole =
  Data.pureGuidExpression (Guid.fromString "HoleyHole") Data.ExpressionHole

setExpr :: Data.PureGuidExpression
setExpr =
  Data.pureGuidExpression (Guid.fromString "SettySet") Data.ExpressionSet

intTypeExpr :: Data.PureGuidExpression
intTypeExpr =
  Data.pureGuidExpression (Guid.fromString "IntyInt") .
  Data.ExpressionBuiltin $
  Data.Builtin (Data.FFIName ["Prelude"] "Integer") setExpr

emptyRefData :: RefData
emptyRefData = RefData
  { _rExpression = hole
  , _rRules = []
  , _rErrors = []
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

data Expression s = Expression
  { eStored :: s
  , eValue :: Data.GuidExpression (Expression s)
  , eInferred :: TypedValue
  } deriving (Functor)


instance Show (Expression s) where
  show (Expression _ value inferred) =
    unwords
    [ "("
    , show value, "="
    , show inferred
    , ")"
    ]

-- Map from params to their Param type,
-- also including the recursive ref to the definition.
-- (hence not just parameters)
type Scope = Map Data.VariableRef Ref

newtype Loader m = Loader
  { loadPureDefinitionType :: Data.DefinitionIRef -> m Data.PureGuidExpression
  }

data ExpressionEntity s = ExpressionEntity
  { eeStored :: s
  , eeGuidExpr :: Data.GuidExpression (ExpressionEntity s)
  }

eeGuid :: ExpressionEntity s -> Guid
eeGuid = Data.geGuid . eeGuidExpr

eeValue :: ExpressionEntity s -> Data.Expression (ExpressionEntity s)
eeValue = Data.geValue . eeGuidExpr

-- Initial expression for inferred value and type of a stored entity.
-- Types are returned only in cases of expanding definitions.
initialExprs ::
  Monad m =>
  Loader m ->
  Scope -> ExpressionEntity s ->
  m (Bool, (Data.PureGuidExpression, Data.PureGuidExpression))
initialExprs loader scope entity =
  (liftM . second . first)
  (Data.pureGuidExpression (eeGuid entity)) $
  case exprStructure of
  Data.ExpressionApply _ -> return (True, (Data.ExpressionHole, hole))
  Data.ExpressionGetVariable var@(Data.DefinitionRef ref)
    | not (Map.member var scope) ->
      liftM ((,) False . (,) exprStructure) $
      loadPureDefinitionType loader ref
  _ -> return (False, (exprStructure, hole))
  where
    exprStructure = fmap (const hole) $ eeValue entity

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
  Data.PureGuidExpression ->
  Data.PureGuidExpression ->
  Maybe Data.PureGuidExpression
mergeExprs p0 p1 =
  runReaderT (go p0 p1) Map.empty
  where
    go
      (Data.PureGuidExpression (Data.GuidExpression g0 e0))
      (Data.PureGuidExpression (Data.GuidExpression g1 e1)) =
      fmap (Data.pureGuidExpression g0) .
      Reader.local (Map.insert g1 g0) $
      case Data.matchExpression go e0 e1 of
      Just x -> Traversable.sequence x
      Nothing ->
        case (e0, e1) of
        (_, Data.ExpressionHole) -> return e0
        (Data.ExpressionHole, _) ->
          -- Map Guids to those of first expression
          Traversable.mapM (go hole) e1
        (Data.ExpressionGetVariable (Data.ParameterRef par0),
         Data.ExpressionGetVariable (Data.ParameterRef par1)) -> do
          par1Mapped <- Reader.asks $ Map.lookup par1
          guard $ Just par0 == par1Mapped
          return e0
        _ -> mzero

alphaEq :: Data.PureGuidExpression -> Data.PureGuidExpression -> Bool
alphaEq x y = mergeExprs x y == Just x

touch :: MonadState InferState m => Ref -> m ()
touch ref = sTouchedRefs . IntSetLens.contains (unRef ref) .= True

setRefExpr ::
  MonadState InferState m =>
  Ref -> Data.PureGuidExpression -> m ()
setRefExpr ref newExpr = do
  curExpr <- Lens.use $ refMapAt ref . rExpression
  case mergeExprs curExpr newExpr of
    Just mergedExpr ->
      when (mergedExpr /= curExpr) $ do
        touch ref
        refMapAt ref . rExpression .= mergedExpr
    Nothing -> refMapAt ref . rErrors %= (newExpr :)

addRules ::
  (MonadState InferState m, MonadReader Scope m) =>
  TypedValue -> Data.Expression TypedValue -> m ()
addRules typedVal expr = do
  refMapAt (tvVal typedVal) . rRules %= (RuleSimpleType typedVal :)
  case expr of
    Data.ExpressionPi lambda ->
      onLambda RulePiStructure lambda
    Data.ExpressionLambda lambda ->
      onLambda RuleLambdaStructure lambda
    Data.ExpressionApply (Data.Apply func arg) ->
      addRuleToMany ([tvVal, tvType] <*> [typedVal, func, arg]) .
        RuleApply $ ApplyComponents typedVal func arg
    Data.ExpressionGetVariable var -> do
      mTypeRef <- Reader.asks $ Map.lookup var
      case mTypeRef of
        Nothing -> return ()
        Just ref ->
          refMapAt ref . rRules %= (RuleUnion ref (tvType typedVal) :)
    _ -> return ()
  where
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
  ExpressionEntity s -> TypedValue ->
  ReaderT (Map Data.VariableRef Ref) (StateT InferState m)
    (Expression s)
nodeFromEntity loader entity typedValue = do
  setInitialValues
  withChildrenTvs <-
    Traversable.mapM addTypedVal $ eeValue entity
  addRules typedValue $ fmap snd withChildrenTvs
  expr <-
    case withChildrenTvs of
    Data.ExpressionLambda lambda ->
      liftM Data.ExpressionLambda $ onLambda lambda
    Data.ExpressionPi lambda@(Data.Lambda _ (_, resultTypeTv)) -> do
      setRefExpr (tvType resultTypeTv) setExpr
      liftM Data.ExpressionPi $ onLambda lambda
    _ -> Traversable.mapM go withChildrenTvs
  return $ Expression (eeStored entity) (Data.GuidExpression (eeGuid entity) expr) typedValue
  where
    onLambda (Data.Lambda paramType@(_, paramTypeTv) result) = do
      setRefExpr (tvType paramTypeTv) setExpr
      paramTypeR <- go paramType
      let paramRef = Data.ParameterRef $ eeGuid entity
      resultR <-
        Reader.local (Map.insert paramRef (tvVal paramTypeTv)) $
        go result
      return $ Data.Lambda paramTypeR resultR
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
  ExpressionEntity s ->
  m (Expression s, InferState)
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

getRefExpr :: MonadState InferState m => Ref -> m Data.PureGuidExpression
getRefExpr ref = Lens.use (refMapAt ref . rExpression)

applyStructureRule ::
  MonadState InferState m =>
  (Data.Lambda Data.PureGuidExpression -> Data.Expression Data.PureGuidExpression) ->
  (Data.Expression Data.PureGuidExpression -> Maybe (Data.Lambda Data.PureGuidExpression)) ->
  LambdaComponents ->
  m ()
applyStructureRule cons uncons (LambdaComponents parentRef paramTypeRef resultRef) = do
  pExpr <- getRefExpr parentRef
  let Data.GuidExpression g expr = Data.unPureGuidExpression pExpr
  case uncons expr of
    Nothing -> return ()
    Just (Data.Lambda paramType result) -> do
      setRefExpr paramTypeRef paramType
      setRefExpr resultRef result
  setRefExpr parentRef . Data.pureGuidExpression g . cons =<<
    liftM2 Data.Lambda (getRefExpr paramTypeRef) (getRefExpr resultRef)

subst ::
  Guid -> Data.PureGuidExpression ->
  Data.PureGuidExpression -> Data.PureGuidExpression
subst from to expr =
  case pgeExpr expr of
  Data.ExpressionGetVariable (Data.ParameterRef g)
    | g == from -> to
  _ ->
    (Data.atPureGuidExpression . Data.atGeValue . fmap)
    (subst from to) expr

recurseSubst ::
  MonadState InferState m =>
  Data.PureGuidExpression -> Guid -> Ref -> Ref -> m Data.PureGuidExpression
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
      case pgeExpr pre of
      Data.ExpressionGetVariable (Data.ParameterRef g)
        | g == paramGuid -> setRefExpr argRef post
      preExpr ->
        case Data.matchExpression mergeToArg preExpr (pgeExpr post) of
        Just x -> do
          _ <- Traversable.sequence x
          return ()
        Nothing -> return ()

maybeLambda :: Data.Expression a -> Maybe (Data.Lambda a)
maybeLambda (Data.ExpressionLambda x) = Just x
maybeLambda _ = Nothing

maybePi :: Data.Expression a -> Maybe (Data.Lambda a)
maybePi (Data.ExpressionPi x) = Just x
maybePi _ = Nothing

pgeExpr :: Data.PureGuidExpression -> Data.Expression Data.PureGuidExpression
pgeExpr = Data.geValue . Data.unPureGuidExpression

applyRule :: MonadState InferState m => Rule -> m ()
applyRule (RuleUnion r0 r1) = do
  setRefExpr r0 =<< getRefExpr r1
  setRefExpr r1 =<< getRefExpr r0
applyRule (RuleLambdaStructure lambda) =
  applyStructureRule Data.ExpressionLambda maybeLambda lambda
applyRule (RulePiStructure lambda) =
  applyStructureRule Data.ExpressionPi maybePi lambda
applyRule (RuleSimpleType (TypedValue val typ)) = do
  valGExpr <- getRefExpr val
  let Data.GuidExpression g valExpr = Data.unPureGuidExpression valGExpr
  case valExpr of
    Data.ExpressionSet -> setRefExpr typ setExpr
    Data.ExpressionPi _ -> setRefExpr typ setExpr
    Data.ExpressionBuiltin b -> setRefExpr typ $ Data.bType b
    Data.ExpressionLiteralInteger _ -> setRefExpr typ intTypeExpr
    Data.ExpressionLambda (Data.Lambda paramType _) ->
      setRefExpr typ . Data.pureGuidExpression g .
      Data.ExpressionPi $ Data.Lambda paramType hole
    _ -> return ()
applyRule (RuleApply (ApplyComponents apply func arg)) = do
  -- ArgT => ParamT
  setRefExpr (tvType func) . Data.pureGuidExpression someGuid .
    Data.ExpressionPi . (`Data.Lambda` hole) =<< getRefExpr (tvType arg)
  -- If Arg is GetParam, ApplyT <=> ResultT
  argExpr <- getRefExpr $ tvVal arg
  case pgeExpr argExpr of
    Data.ExpressionGetVariable (Data.ParameterRef _) -> do
      setRefExpr (tvType func) . Data.pureGuidExpression someGuid .
        Data.ExpressionPi . Data.Lambda hole =<< getRefExpr (tvType apply)
      funcTExpr <- getRefExpr $ tvType func
      case pgeExpr funcTExpr of
        Data.ExpressionPi (Data.Lambda _ resultT) ->
          setRefExpr (tvType apply) resultT
        _ -> return ()
    _ -> return ()

  Data.PureGuidExpression
    (Data.GuidExpression funcTGuid funcTExpr) <-
    getRefExpr $ tvType func
  case funcTExpr of
    Data.ExpressionPi (Data.Lambda paramT resultT) -> do
      -- ParamT => ArgT
      setRefExpr (tvType arg) paramT
      -- Recurse-Subst ResultT Arg ApplyT
      setRefExpr (tvType func) . Data.pureGuidExpression funcTGuid .
        Data.ExpressionPi . Data.Lambda paramT =<<
        recurseSubst resultT funcTGuid (tvVal arg) (tvType apply)
    _ -> return ()

  funcPge@(Data.PureGuidExpression
    (Data.GuidExpression funcGuid funcExpr)) <-
    getRefExpr $ tvVal func
  case funcExpr of
    Data.ExpressionLambda (Data.Lambda paramT body) -> do
      -- ParamT => ArgT
      setRefExpr (tvType arg) paramT
      -- ArgT => ParamT
      setRefExpr (tvVal func) . Data.pureGuidExpression someGuid .
        Data.ExpressionLambda . (`Data.Lambda` hole) =<< getRefExpr (tvType arg)
      -- Recurse-Subst Body Arg Apply
      setRefExpr (tvVal func) . Data.pureGuidExpression funcGuid .
        Data.ExpressionLambda . Data.Lambda paramT =<<
        recurseSubst body funcGuid (tvVal arg) (tvVal apply)
    Data.ExpressionHole -> return ()
    _ ->
      setRefExpr (tvVal apply) . Data.pureGuidExpression someGuid .
        Data.ExpressionApply $ Data.Apply funcPge argExpr
  where
    someGuid = Guid.fromString "Arbitrary"

inferFromEntity ::
  Monad m =>
  Loader m ->
  Maybe Data.DefinitionIRef ->
  ExpressionEntity s ->
  m (Expression s, RefMap)
inferFromEntity loader mRecursiveDef =
  (liftM . second) infer . fromEntity loader mRecursiveDef

deref :: RefMap -> Ref -> ([Conflict], Data.PureGuidExpression)
deref (RefMap m) (Ref x) =
  ( refData ^. rErrors
  , refData ^. rExpression
  )
  where
    refData = m ! x
