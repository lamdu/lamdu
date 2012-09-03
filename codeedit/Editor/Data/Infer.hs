{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving, TemplateHaskell #-}
module Editor.Data.Infer
  ( Expression, Inferred(..), rExpression
  , Loaded, load, infer
  , InferNode(..), TypedValue(..)
  , Error(..), ErrorDetails(..)
  , RefMap, Ref
  , Loader(..), InferActions(..)
  , initial, newNodeWithScope
  ) where

import Control.Arrow (first)
import Control.Lens ((%=), (.=), (^.), (+=))
import Control.Monad (guard, liftM, liftM2, unless, void, when)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Either (EitherT(..))
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.Monad.Trans.Writer (Writer)
import Control.Monad.Trans.State (StateT(..), State, runState, execState)
import Data.Functor.Identity (Identity(..))
import Data.IntMap (IntMap, (!))
import Data.IntSet (IntSet)
import Data.Map (Map)
import Data.Maybe (fromMaybe, isJust, mapMaybe, maybeToList)
import Data.Monoid (Monoid(..))
import Data.Set (Set)
import Data.Store.Guid (Guid)
import qualified Control.Lens as Lens
import qualified Control.Lens.TH as LensTH
import qualified Control.Monad.Trans.Either as Either
import qualified Control.Monad.Trans.Reader as Reader
import qualified Data.Foldable as Foldable
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

newtype RefExprPayload = RefExprPayload
  { _pSubstitutedArgs :: IntSet
  } deriving (Show, Monoid)

type RefExpression = Data.Expression RefExprPayload

refExprFromPure :: Data.PureExpression -> RefExpression
refExprFromPure = fmap $ const mempty

data Rule = Rule
  { ruleInputs :: [Ref]
  , _ruleCompute :: [RefExpression] -> [(Ref, RefExpression)]
  }

data RefData = RefData
  { _rExpression :: RefExpression
  , _rRules :: [Int] -- Rule id
  }

makeRefExpression :: Guid -> Data.ExpressionBody RefExpression -> RefExpression
makeRefExpression g expr =
  Data.Expression g expr mempty

augmentGuid :: String -> Guid -> Guid
augmentGuid = Guid.combine . Guid.fromString

makeHole :: String -> Guid -> RefExpression
makeHole s g =
  makeRefExpression (augmentGuid s g) $ Data.ExpressionLeaf Data.Hole

setExpr :: RefExpression
setExpr =
  makeRefExpression (Guid.fromString "SettySet") $
  Data.ExpressionLeaf Data.Set

intTypeExpr :: RefExpression
intTypeExpr =
  makeRefExpression (Guid.fromString "IntyInt") $
  Data.ExpressionLeaf Data.IntegerType

data RefMap = RefMap
  { _refMap :: IntMap RefData
  , _nextRef :: Int
  , _rules :: IntMap Rule
  , _nextRule :: Int
  }

data InferState = InferState
  { _sRefMap :: RefMap
  , _sBfsNextLayer :: IntSet
  , _sBfsCurLayer :: IntSet
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

LensTH.makeLenses ''RefExprPayload
LensTH.makeLenses ''RefData
LensTH.makeLenses ''RefMap
LensTH.makeLenses ''InferState

-- TODO: createTypeVal should use newNode, not vice versa.
-- For use in loading phase only!
-- We don't create additional Refs afterwards!
createTypedVal :: Monad m => StateT RefMap m TypedValue
createTypedVal =
  liftM2 TypedValue createRef createRef
  where
    createRef = do
      key <- Lens.use nextRef
      nextRef += 1
      return $ Ref key

newNodeWithScope :: Scope -> RefMap -> (RefMap, InferNode)
newNodeWithScope scope prevRefMap =
  (resultRefMap, InferNode tv scope)
  where
    (tv, resultRefMap) = runState createTypedVal prevRefMap

initial :: (RefMap, InferNode)
initial = newNodeWithScope mempty $ RefMap mempty 0 mempty 0

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

guardEither :: l -> Bool -> EitherT l Identity ()
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
      -- TODO: Yair says Data.atEPayload should be fmap.
      -- The hole-on-right case should be handled in the same
      -- way by "go" and not in "f" without mappending anything
      -- Make tests that reproduce a problem...
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

lambdaRules :: Guid -> TypedValue -> Ref -> [Rule]
lambdaRules g (TypedValue lambdaValueRef lambdaTypeRef) bodyTypeRef =
  [ -- Lambda body type -> Pi result type
    Rule [bodyTypeRef] $ \[bodyTypeExpr] ->
    [( lambdaTypeRef
     , makeRefExpression g $ Data.makePi (makeHole "paramType" g) bodyTypeExpr
     )]
  , Rule [lambdaTypeRef] $ \[Data.Expression piG body _] -> do
      Data.Lambda paramType resultType <- maybeToList $ maybePi body
      [ -- Pi result type -> Body type
        ( bodyTypeRef
        , subst piG
          ( makeRefExpression (Guid.fromString "getVar")
            (Data.makeParameterRef g)
          )
          resultType
        )
        , -- Pi param type -> Lambda param type
        ( lambdaValueRef
        , makeRefExpression g . Data.makeLambda paramType $ makeHole "body" g
        )
        ]
  ]

unionRules :: Ref -> Ref -> [Rule]
unionRules x y =
  [ Rule [x] $ \[xExpr] -> [(y, xExpr)]
  , Rule [y] $ \[yExpr] -> [(x, yExpr)]
  ]

lambdaStructureRules ::
  (Data.Lambda RefExpression -> Data.ExpressionBody RefExpression) ->
  (Data.ExpressionBody RefExpression -> Maybe (Data.Lambda RefExpression)) ->
  Guid -> Ref -> Data.Lambda Ref -> [Rule]
lambdaStructureRules cons uncons g lamRef (Data.Lambda paramTypeRef resultRef) =
  [ -- Copy the structure from the parent to the paramType and
    -- result
    Rule [lamRef] $ \ [expr] -> do
      Data.Lambda paramTypeE resultE <-
        maybeToList . uncons $ Data.eValue expr
      [(paramTypeRef, paramTypeE), (resultRef, resultE)]
  , -- Copy the structure from the children to the parent
    Rule [paramTypeRef, resultRef] $ \ [paramTypeExpr, resultExpr] ->
    [( lamRef
     , makeRefExpression g . cons $ Data.Lambda paramTypeExpr resultExpr
     )]
  ]

makeNodeRules :: Data.Expression InferNode -> [Rule]
makeNodeRules (Data.Expression g exprBody (InferNode typedVal scope)) =
  case fmap (nRefs . Data.ePayload) exprBody of
  Data.ExpressionPi lambda@(Data.Lambda _ resultType) ->
    setRule (tvType resultType) :
    onLambda Data.ExpressionPi maybePi lambda
  Data.ExpressionLambda lambda@(Data.Lambda _ body) ->
    lambdaRules g typedVal (tvType body) ++
    onLambda Data.ExpressionLambda maybeLambda lambda
  Data.ExpressionApply apply -> applyRules g typedVal apply
  Data.ExpressionLeaf (Data.GetVariable var) -> do
    ref <- maybeToList $ Map.lookup var scope
    unionRules ref $ tvType typedVal
  _ -> []
  where
    setRule ref = Rule [] $ \ [] -> [(ref, setExpr)]
    onLambda cons uncons lam@(Data.Lambda paramType _) =
      setRule (tvType paramType) :
      lambdaStructureRules cons uncons g (tvVal typedVal) (fmap tvVal lam)

commonRules :: Data.Expression InferNode -> [Rule]
commonRules expr =
  [ ruleSimpleType . nRefs $ Data.ePayload expr
  -- TODO:
  -- If a value is complete (has no holes),
  -- and it's value contains parameters that the value does not,
  -- this must be a type error.
  -- Useful for checking on definitions with unknown types.
  -- This would require rules to be able to report errors.
  -- Example:
  --   f a = id a someDef
  ]

makeRules :: Bool -> Data.Expression InferNode -> [Rule]
makeRules resumption expr =
  (if resumption then [] else commonRules expr) ++
  makeNodeRules expr ++
  (Foldable.concat . fmap (makeRules False)) (Data.eValue expr)

loadNode ::
  Monad m =>
  Loader m -> Scope ->
  Data.Expression s -> TypedValue ->
  StateT RefMap m (Data.Expression (s, InferNode))
loadNode loader scope entity typedValue = do
  setInitialValues
  bodyWithChildrenTvs <- Traversable.mapM addTypedVal $ Data.eValue entity
  exprBody <-
    case bodyWithChildrenTvs of
    Data.ExpressionLambda lambda ->
      onLambda Data.ExpressionLambda lambda
    Data.ExpressionPi lambda ->
      onLambda Data.ExpressionPi lambda
    _ -> Traversable.mapM (go id) bodyWithChildrenTvs
  return $
    Data.Expression (Data.eGuid entity) exprBody
    (Data.ePayload entity, InferNode typedValue scope)
  where
    onLambda cons (Data.Lambda paramType@(_, paramTypeTv) result) = do
      paramTypeR <- go id paramType
      let paramRef = Data.ParameterRef $ Data.eGuid entity
      liftM (cons . Data.Lambda paramTypeR) $
        go (Map.insert paramRef (tvVal paramTypeTv)) result
    go onScope = uncurry . loadNode loader $ onScope scope
    addTypedVal x =
      liftM ((,) x) createTypedVal
    initializeRefData ref expr =
      refMap . IntMapLens.at (unRef ref) .=
      Just (RefData (refExprFromPure expr) [])
    setInitialValues = do
      (initialVal, initialType) <-
        lift $ initialExprs loader scope entity
      initializeRefData (tvVal typedValue) initialVal
      initializeRefData (tvType typedValue) initialType

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

maybeLambda :: Data.ExpressionBody a -> Maybe (Data.Lambda a)
maybeLambda (Data.ExpressionLambda x) = Just x
maybeLambda _ = Nothing

maybePi :: Data.ExpressionBody a -> Maybe (Data.Lambda a)
maybePi (Data.ExpressionPi x) = Just x
maybePi _ = Nothing

ruleSimpleType :: TypedValue -> Rule
ruleSimpleType (TypedValue val typ) =
  Rule [val] $ \[valExpr] -> case valExpr of
    Data.Expression g valExprBody _ -> case valExprBody of
      Data.ExpressionLeaf Data.Set -> [(typ, setExpr)]
      Data.ExpressionLeaf Data.IntegerType -> [(typ, setExpr)]
      Data.ExpressionLeaf (Data.LiteralInteger _) -> [(typ, intTypeExpr)]
      Data.ExpressionPi _ -> [(typ, setExpr)]
      Data.ExpressionLambda (Data.Lambda paramType _) ->
        [( typ
         , makeRefExpression g . Data.makePi paramType $
           makeHole "lambdaBody" g
         )]
      _ -> []

recurseSubstRules ::
  (Data.Lambda RefExpression -> Data.ExpressionBody RefExpression) ->
  (Data.ExpressionBody RefExpression -> Maybe (Data.Lambda RefExpression)) ->
  Ref -> Data.Apply Ref -> [Rule]
recurseSubstRules cons uncons apply (Data.Apply func arg) =
  [ -- PreSubst with Subst => PostSubst
    Rule [func, arg] $ \ [Data.Expression funcGuid funcBody _, argExpr] -> do
      Data.Lambda _ result <- maybeToList $ uncons funcBody
      return
        ( apply
        , subst funcGuid
          ((fmap . Lens.over pSubstitutedArgs . IntSet.insert) (unRef arg) argExpr)
          result
        )

  , -- Recurse over PreSubst and PostSubst together
    --   When PreSubst part refers to its param:
    --     PostSubst part <=> arg
    Rule [apply, func] $ \ [applyExpr, Data.Expression funcGuid funcBody _] -> do
      Data.Lambda _ result <- maybeToList $ uncons funcBody
      mergeToArg funcGuid result applyExpr

  , -- Propagate data from Apply's to the Func where appropriate.
    -- (Not on non-substituted holes)
    Rule [apply, func] $ \ [applyExpr, Data.Expression funcGuid funcExpr _] -> do
      Data.Lambda paramT result <- maybeToList $ uncons funcExpr
      return
        ( func
        , makeRefExpression funcGuid .
          cons . Data.Lambda paramT $
          mergeToPiResult result applyExpr
        )
  ]
  where
    mergeToArg paramGuid pre post =
      case Data.eValue pre of
      Data.ExpressionLeaf (Data.GetVariable (Data.ParameterRef g))
        | g == paramGuid ->
          [( arg
           , (fmap . Lens.over pSubstitutedArgs . IntSet.delete) (unRef arg) post
           )]
      preExpr ->
        Foldable.concat =<< maybeToList
        (Data.matchExpressionBody (mergeToArg paramGuid) preExpr
         (Data.eValue post))

applyRules :: Guid -> TypedValue -> Data.Apply TypedValue -> [Rule]
applyRules baseGuid apply (Data.Apply func arg) =
  [ -- ArgT => Pi ParamT
    Rule [tvType arg] $ \ [argTypeExpr] ->
    [( tvType func
     , makeRefExpression (augmentGuid "ar0" baseGuid) .
       Data.makePi argTypeExpr $ makeHole "ar1" baseGuid
     )]

  , -- If Arg is GetParam
    -- ApplyT (Susbt Arg with Hole) => ResultT
    Rule [tvType apply, tvVal arg] $ \ [applyTypeExpr, argExpr] ->
    case Data.eValue argExpr of
    Data.ExpressionLeaf (Data.GetVariable (Data.ParameterRef par)) ->
      [ ( tvType func
        , makeRefExpression (augmentGuid "ar2" baseGuid) .
          Data.makePi (makeHole "ar3" baseGuid) $
          subst par (makeHole "ar7" baseGuid) applyTypeExpr
        )
      ]
    _ -> []

  , -- If func type is Pi
    -- Pi's ParamT => ArgT
    Rule [tvType func] $ \ [Data.Expression _ funcTExpr _] -> do
      Data.Lambda paramT _ <- maybeToList $ maybePi funcTExpr
      return (tvType arg, paramT)

  , -- If func is Lambda
    -- Lambda's ParamT => ArgT
    Rule [tvVal func] $ \ [Data.Expression _ funcExpr _] -> do
      Data.Lambda paramT _ <- maybeToList $ maybeLambda funcExpr
      return (tvType arg, paramT)

  , -- If func is Lambda,
    -- ArgT => Lambda's ParamT
    Rule [tvVal func, tvType arg] $
    \ [Data.Expression funcGuid funcExpr _, argTExpr] -> do
      _ <- maybeToList $ maybeLambda funcExpr
      return
        ( tvVal func
        , makeRefExpression funcGuid .
          Data.makeLambda argTExpr $ makeHole "ar5" baseGuid
        )

  , -- If func is surely not a lambda (a hole too could be a lambda),
    -- Func Arg => Outer
    Rule [tvVal func, tvVal arg] $ \ [funcExpr, argExpr] ->
    case Data.eValue funcExpr of
    Data.ExpressionLambda _ -> []
    Data.ExpressionLeaf Data.Hole -> []
    _ ->
      [ ( tvVal apply
        , makeRefExpression (augmentGuid "ar6" baseGuid) $
          Data.makeApply funcExpr argExpr
        )
      ]
  ]
  ++ recurseSubstRules Data.ExpressionPi maybePi
    (tvType apply) (Data.Apply (tvType func) (tvVal arg))
  ++ recurseSubstRules Data.ExpressionLambda maybeLambda
    (tvVal apply) (Data.Apply (tvVal func) (tvVal arg))

data Loaded a = Loaded
  { lExpr :: Data.Expression (a, InferNode)
  , lRefMap :: RefMap
  , lSavedRoot :: (Maybe RefData, Maybe RefData)
  }

load ::
  Monad m => Loader m -> RefMap -> InferNode ->
  Maybe Data.DefinitionIRef -> Data.Expression a ->
  m (Loaded a)
load
  loader initialRefMap
  (InferNode rootTv@(TypedValue rootValR rootTypR) rootScope)
  mRecursiveDef expression =
    liftM buildLoaded . (`runStateT` initialRefMap) $
    loadNode loader scope expression rootTv
  where
    initialMRefData k =
      Lens.view (refMap . IntMapLens.at (unRef k)) initialRefMap
    buildLoaded (node, resultRefMap) = Loaded
      { lExpr = node
      , lRefMap = resultRefMap
      , lSavedRoot =
        ( initialMRefData rootValR
        , initialMRefData rootTypR
        )
      }
    scope =
      case mRecursiveDef of
      Nothing -> rootScope
      Just iref ->
        Map.insert (Data.DefinitionRef iref) (tvType rootTv) rootScope

postProcess ::
  (Data.Expression (a, InferNode), InferState) -> (Expression a, RefMap)
postProcess (expr, InferState resultRefMap _ _) =
  (fmap derefNode expr, resultRefMap)
  where
    derefNode (s, inferNode) =
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
  sBfsNextLayer . IntSetLens.contains ruleId .= True
  where
    makeRule = do
      ruleId <- Lens.use (sRefMap . nextRule)
      sRefMap . nextRule += 1
      sRefMap . rules . IntMapLens.at ruleId .= Just rule
      return ruleId
    addRuleId ruleId ref = refMapAt ref . rRules %= (ruleId :)

--- InferT:

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

{-# SPECIALIZE liftState :: StateT InferState Maybe a -> InferT Maybe a #-}
{-# SPECIALIZE liftState :: Monoid w => StateT InferState (Writer w) a -> InferT (Writer w) a #-}

instance MonadTrans InferT where
  lift = liftState . lift

infer :: Monad m => InferActions m -> Loaded a -> m (Expression a, RefMap)
infer actions (Loaded expr loadedRefMap (rootValMRefData, rootTypMRefData)) =
  liftM postProcess .
  runInferT actions ruleInferState $ do
    restoreRoot rootValR rootValMRefData
    restoreRoot rootTypR rootTypMRefData
    -- when we resume load,
    -- we want to trigger the existing rules for the loaded root
    touch rootValR
    touch rootTypR
    go
    return expr
  where
    ruleInferState =
      (`execState` InferState loadedRefMap mempty mempty) .
      mapM_ addRule .
      makeRules (isJust rootValMRefData) $ fmap snd expr
    TypedValue rootValR rootTypR = nRefs . snd $ Data.ePayload expr
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
      liftState $ sBfsCurLayer . IntSetLens.contains key .= False
      Just (Rule deps ruleAction) <-
        liftState $ Lens.use (sRefMap . rules . IntMapLens.at key)
      refExps <- mapM getRefExpr deps
      mapM_ (uncurry setRefExpr) $ ruleAction refExps

{-# SPECIALIZE
  infer :: InferActions Maybe -> Loaded a -> Maybe (Expression a, RefMap) #-}
{-# SPECIALIZE
  infer :: Monoid w => InferActions (Writer w) -> Loaded a -> Writer w (Expression a, RefMap) #-}

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
