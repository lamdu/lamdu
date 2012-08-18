{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts, TemplateHaskell #-}
module Editor.Data.Typed
  ( StoredExpression(..)
  , Ref
  , RefMap
  ) where

import Control.Applicative ((<*>), liftA2)
import Control.Arrow (first)
import Control.Lens ((%=), (.=), (^.))
import Control.Monad (mzero, liftM, liftM2, when)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.State.Class (MonadState)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.Monad.Trans.State (StateT, execState, runStateT)
import Data.IntMap (IntMap)
import Data.IntSet (IntSet)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Monoid (mempty)
import Data.Store.Guid (Guid)
import Data.Store.Transaction (Transaction)
import Editor.Anchors (ViewTag)
import qualified Control.Lens as Lens
import qualified Control.Lens.TH as LensTH
import qualified Control.Monad.Reader.Class as Reader
import qualified Data.IntMap as IntMap
import qualified Data.IntMap.Lens as IntMapLens
import qualified Data.IntSet as IntSet
import qualified Data.IntSet.Lens as IntSetLens
import qualified Data.Map as Map
import qualified Data.Store.Guid as Guid
import qualified Data.Traversable as Traversable
import qualified Editor.Data as Data
import qualified Editor.Data.Load as DataLoad

newtype Ref = Ref { unRef :: Int }
instance Show Ref where
  show = ('P' :) . show . unRef

data TypedValue = TypedValue
  { tvVal :: Ref
  , tvType :: Ref
  }
instance Show TypedValue where
  show (TypedValue v t) = concat [show v, ":", show t]

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
-- Recurse-Subst ResultT ApplyT Arg
-- If Arg is Get Param:
--   ApplyT <=> ResultT
-- Case Func of
--   Hole -> Do Nothing
--   \LParamT -> Body : BodyT:
--     LParamT <=> ParamT
--     BodyT <=> ResultT
--     Recurse-Subst Body Apply Arg
--   Other -> Copy (Func Arg) to Apply
--
-- Where Recurse-Subst PreSubst PostSubst Arg
--   Recurse over PreSubst and PostSubst together:
--     When PostSubst part is hole:
--       Replace it with structure from PreSubst and resume recursion
--     When PreSubst part refers to its param:
--       PostSubst part <=> Arg

data ApplyComponents = ApplyComponents
  { _acApply :: TypedValue
  , _acFunc :: TypedValue
  , _acArg :: TypedValue
  }

-- LambdaOrPi Rule:
-- Each of expr parts:
--   Part <=> Stored

data LambdaComponents = LambdaComponents
  { _lcParent :: Ref
  , _lcParamType :: Ref
  , _lcResult :: Ref
  }

-- Union rule (type of get param, but also for recursive type)

data Rule
  = RuleSimpleType TypedValue
  | RuleUnion Ref Ref
  | RuleLambdaStructure LambdaComponents
  | RulePiStructure LambdaComponents
  | RuleApply ApplyComponents

type Conflict = Data.PureGuidExpression

data RefData = RefData
  { _rExpression :: Data.PureGuidExpression
  , _rRules :: [Rule]
  , _rErrors :: [Conflict]
  }
LensTH.makeLenses ''RefData

hole :: Data.PureGuidExpression
hole = Data.pureGuidExpression (Guid.fromString "HoleyHole") Data.ExpressionHole

setExpr :: Data.PureGuidExpression
setExpr = Data.pureGuidExpression (Guid.fromString "SettySet") Data.ExpressionSet

emptyRefData :: RefData
emptyRefData = RefData
  { _rExpression = hole
  , _rRules = []
  , _rErrors = []
  }

type RefMap = IntMap RefData
data InferState = InferState
  { _sRefMap :: RefMap
  , _sTouchedRefs :: IntSet
  }
LensTH.makeLenses ''InferState

data StoredExpression m = StoredExpression
  { _eProp :: Data.ExpressionIRefProperty m
  , _eValue :: Data.Expression (StoredExpression m)
  , _eInferred :: TypedValue
  }
LensTH.makeLenses ''StoredExpression

instance Show (StoredExpression s) where
  show (StoredExpression prop value inferred) =
    unwords
    [ "("
    , show (Data.eipGuid prop), ":"
    , show value, "="
    , show inferred
    , ")"
    ]

toPureExpression ::
  Monad m => StoredExpression m -> Data.PureGuidExpression
toPureExpression expr =
  Data.pureGuidExpression
  (Data.eipGuid (expr ^. eProp)) .
  fmap toPureExpression $ expr ^. eValue

type T = Transaction ViewTag

-- Map from params to their Param type,
-- also including the recursive ref to the definition.
-- (hence not just parameters)
type Scope = Map Data.VariableRef Ref

-- Initial expression for inferred value and type of a stored entity.
-- Types are returned only in cases of expanding definitions.
initialExprs ::
  Monad m =>
  Scope -> DataLoad.ExpressionEntity (T m) ->
  T m (Data.PureGuidExpression, Data.PureGuidExpression)
initialExprs scope entity =
  (liftM . first)
  (Data.pureGuidExpression (DataLoad.entityGuid entity)) $
  case exprStructure of
  Data.ExpressionApply _ -> return (Data.ExpressionHole, hole)
  Data.ExpressionGetVariable var@(Data.DefinitionRef ref)
    | not (Map.member var scope) ->
      liftM ((,) exprStructure) $ DataLoad.loadPureDefinitionType ref
  _ -> return (exprStructure, hole)
  where
    exprStructure = fmap (const hole) $ DataLoad.entityValue entity

intMapMod :: Functor f => Int -> (v -> f v) -> (IntMap v -> f (IntMap v))
intMapMod k =
  IntMapLens.at k .
  Lens.iso from Just
  where
    from = fromMaybe . error $ unwords ["intMapMod: key", show k, "not in map"]

refMapAt :: Functor f => Ref -> (RefData -> f RefData) -> (InferState -> f InferState)
refMapAt k = sRefMap . intMapMod (unRef k)

-- Merge two expressions:
-- If they do not match, return Nothing.
-- Holes match with anything, expand to the other expr.
-- Results with the Guids of the first expression (where available).
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
        (Data.ExpressionGetVariable (Data.ParameterRef p0),
         Data.ExpressionGetVariable (Data.ParameterRef p1)) -> do
          p1Mapped <- Reader.asks $ Map.lookup p1
          if Just p0 == p1Mapped
            then return e0
            else mzero
        _ -> mzero

setRefExpr ::
  MonadState InferState m =>
  Ref -> Data.PureGuidExpression -> m ()
setRefExpr ref newExpr = do
  curExpr <- Lens.use $ refMapAt ref . rExpression
  case mergeExprs curExpr newExpr of
    Just mergedExpr ->
      when (mergedExpr /= curExpr) $ do
        sTouchedRefs . IntSetLens.contains (unRef ref) .= True
        refMapAt ref . rExpression .= mergedExpr
    Nothing -> refMapAt ref . rErrors %= (newExpr :)

addRules ::
  (MonadState InferState m, MonadReader Scope m) =>
  TypedValue -> Data.Expression TypedValue -> m ()
addRules typedVal expr = do
  refMapAt (tvVal typedVal) . rRules %= (RuleSimpleType typedVal :)
  case expr of
    Data.ExpressionPi lambda@(Data.Lambda _ resultType) ->
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
    onLambda cons lambda@(Data.Lambda paramType result) =
      addRuleToMany [tvVal typedVal, tvVal paramType, tvVal result] .
      cons $ LambdaComponents (tvVal typedVal) (tvVal paramType) (tvVal result)

-- For use in loading phase only!
-- We don't create additional Refs afterwards!
createTypedVal :: MonadState InferState m => m TypedValue
createTypedVal = do
  valRef <- createRef
  typeRef <- createRef
  return $ TypedValue valRef typeRef
  where
    createRef = do
      key <- liftM IntMap.size $ Lens.use sRefMap
      sRefMap . IntMapLens.at key .= Just emptyRefData
      return $ Ref key

loadNode ::
  Monad m =>
  DataLoad.ExpressionEntity (T m) -> TypedValue ->
  ReaderT (Map Data.VariableRef Ref) (StateT InferState (T m)) (StoredExpression (T m))
loadNode entity typedValue = do
  setInitialValues entity typedValue
  withChildrenTvs <-
    Traversable.mapM addTypedVal $ DataLoad.entityValue entity
  addRules typedValue $ fmap snd withChildrenTvs
  expr <-
    case withChildrenTvs of
    Data.ExpressionLambda lambda ->
      liftM Data.ExpressionLambda $ onLambda lambda
    Data.ExpressionPi lambda@(Data.Lambda _ (_, resultTypeTv)) -> do
      setRefExpr (tvType resultTypeTv) setExpr
      liftM Data.ExpressionPi $ onLambda lambda
    _ -> Traversable.mapM go withChildrenTvs
  return $ StoredExpression (DataLoad.entityStored entity) expr typedValue
  where
    onLambda (Data.Lambda paramType@(_, paramTypeTv) result) = do
      setRefExpr (tvType paramTypeTv) setExpr
      paramTypeR <- go paramType
      let paramRef = Data.ParameterRef $ DataLoad.entityGuid entity
      resultR <-
        Reader.local (Map.insert paramRef (tvVal paramTypeTv)) $
        go result
      return $ Data.Lambda paramTypeR resultR
    go = uncurry loadNode
    addTypedVal x =
      liftM ((,) x) createTypedVal
    transaction = lift . lift
    setInitialValues entity typedValue = do
      scope <- Reader.ask
      (initialVal, initialType) <- transaction $ initialExprs scope entity
      setRefExpr (tvVal typedValue) initialVal
      setRefExpr (tvType typedValue) initialType

fromLoaded ::
  Monad m =>
  Maybe Data.DefinitionIRef ->
  DataLoad.ExpressionEntity (T m) -> T m (StoredExpression (T m), InferState)
fromLoaded mRecursiveIRef rootEntity =
  (`runStateT` InferState mempty mempty) .
  (`runReaderT` mempty) $ do
    rootTv <- createTypedVal
    ( case mRecursiveIRef of
        Nothing -> id
        Just iref -> Reader.local $ Map.insert (Data.DefinitionRef iref) (tvType rootTv)
      ) $
      loadNode rootEntity rootTv

popTouchedRef :: MonadState InferState m => m (Maybe Ref)
popTouchedRef = do
  touched <- Lens.use sTouchedRefs
  case IntSet.minView touched of
    Nothing -> return Nothing
    Just (key, newTouchedRefs) -> do
      sTouchedRefs .= newTouchedRefs
      return . Just $ Ref key

infer :: InferState -> InferState
infer =
  execState go
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

recurseSubst ::
  MonadState InferState m =>
  Data.PureGuidExpression -> Guid -> Ref -> Ref -> m Data.PureGuidExpression
recurseSubst preSubstExpr paramGuid argRef postSubstRef = undefined

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
      -- Recurse-Subst ResultT ApplyT Arg
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
      -- Recurse-Subst Body Apply Arg
      setRefExpr (tvVal func) . Data.pureGuidExpression funcGuid .
        Data.ExpressionLambda . Data.Lambda paramT =<<
        recurseSubst body funcGuid (tvVal arg) (tvVal apply)
    Data.ExpressionHole -> return ()
    _ ->
      setRefExpr (tvVal apply) . Data.pureGuidExpression someGuid .
        Data.ExpressionApply $ Data.Apply funcPge argExpr
  where
    someGuid = Guid.fromString "Arbitrary"
