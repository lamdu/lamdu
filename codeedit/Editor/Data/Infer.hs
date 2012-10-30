{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving, TemplateHaskell #-}
module Editor.Data.Infer
  ( Expression, Inferred(..), rExpression
  , Loaded, load, infer
  , InferNode(..), TypedValue(..)
  , Error(..), ErrorDetails(..)
  , RefMap, Ref
  , Loader(..), InferActions(..)
  , initial, newNodeWithScope, newTypedNodeWithScope
  ) where

import Control.Applicative (Applicative(..))
import Control.Arrow (first)
import Control.Lens ((%=), (.=), (^.), (+=))
import Control.Monad (guard, liftM, liftM2, unless, void, when)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Either (EitherT(..))
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.Monad.Trans.State (StateT(..), State, runState, execState)
import Control.Monad.Trans.Writer (Writer, execWriter)
import Control.Monad.Unit (Unit(..))
import Data.Derive.Foldable (makeFoldable)
import Data.Derive.Traversable (makeTraversable)
import Data.DeriveTH (derive)
import Data.Foldable (Foldable(..))
import Data.Functor.Identity (Identity(..))
import Data.IntMap (IntMap, (!))
import Data.IntSet (IntSet)
import Data.Map (Map)
import Data.Maybe (fromMaybe, isJust, mapMaybe, maybeToList)
import Data.Monoid (Monoid(..))
import Data.Store.Guid (Guid)
import Data.Traversable (Traversable(traverse))
import qualified Control.Compose as Compose
import qualified Control.Lens as Lens
import qualified Control.Lens.TH as LensTH
import qualified Control.Monad.Trans.Either as Either
import qualified Control.Monad.Trans.Reader as Reader
import qualified Control.Monad.Trans.Writer as Writer
import qualified Data.Foldable as Foldable
import qualified Data.IntSet as IntSet
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
  } deriving (Functor)
derive makeFoldable ''Inferred
derive makeTraversable ''Inferred

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

guidRepeat :: Data.Expression a -> Bool
guidRepeat =
  go Set.empty
  where
    go forbidden (Data.Expression g body _)
      | Set.member g forbidden = True
      | otherwise =
        Foldable.any (go (Set.insert g forbidden)) body

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
    onMatch x y = return $ mappend x y
    onMismatch (Data.Expression _ (Data.ExpressionLeaf Data.Hole) s0) e1 =
      return $ fmap (mappend s0) e1
    onMismatch e0 (Data.Expression _ (Data.ExpressionLeaf Data.Hole) s1) =
      return $ fmap (`mappend` s1) e0
    onMismatch e0 e1 =
      Either.left $ MismatchIn (void e0) (void e1)

-- Lambda body type -> Pi result type
lambdaBodyTypeToPiResultTypeRule :: Guid -> Ref -> Ref -> Rule
lambdaBodyTypeToPiResultTypeRule g lambdaTypeRef bodyTypeRef =
  Rule [bodyTypeRef] $ \[bodyTypeExpr] ->
  [( lambdaTypeRef
   , makeRefExpression g $ Data.makePi (makeHole "paramType" g) bodyTypeExpr
   )]

piToLambdaRule :: Guid -> TypedValue -> Ref -> Rule
piToLambdaRule g (TypedValue lambdaValueRef lambdaTypeRef) bodyTypeRef =
  Rule [lambdaTypeRef] $ \[Data.Expression piG body _] -> do
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

lambdaRules :: Guid -> TypedValue -> Ref -> [Rule]
lambdaRules g lambdaTv@(TypedValue _ lambdaTypeRef) bodyTypeRef =
  [ lambdaBodyTypeToPiResultTypeRule g lambdaTypeRef bodyTypeRef
  , piToLambdaRule g lambdaTv bodyTypeRef
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
      refMap . Lens.at (unRef ref) .=
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
mergeToPiResult =
  fmap runIdentity .
  Data.matchExpression onMatch ((fmap . fmap) return onMismatch)
  where
    onMatch x _ = return x
    onMismatch destHole@(Data.Expression _ (Data.ExpressionLeaf Data.Hole) destPayload) src
      | IntSet.null substs = destHole
      | otherwise = fmap (Lens.set pSubstitutedArgs substs) src
      where
        substs = destPayload ^. pSubstitutedArgs
    -- TODO: This seems like it should report an error,
    -- verify/document that it is OK because the other direction of
    -- information flow will catch any error:
    onMismatch dest _ = dest

maybeLambda :: Data.ExpressionBody a -> Maybe (Data.Lambda a)
maybeLambda (Data.ExpressionLambda x) = Just x
maybeLambda _ = Nothing

maybePi :: Data.ExpressionBody a -> Maybe (Data.Lambda a)
maybePi (Data.ExpressionPi x) = Just x
maybePi _ = Nothing

maybeApply :: Data.ExpressionBody a -> Maybe (Data.Apply a)
maybeApply (Data.ExpressionApply x) = Just x
maybeApply _ = Nothing

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

intoApplyResultRule ::
  (Data.ExpressionBody RefExpression ->
   Maybe (Data.Lambda RefExpression)) ->
  Ref -> Data.Apply Ref -> Rule
intoApplyResultRule uncons applyRef (Data.Apply func arg) =
  -- PreSubst with Subst => PostSubst
  -- (func, arg) -> apply
  Rule [func, arg] $ \ [Data.Expression funcGuid funcBody _, argExpr] -> do
    Data.Lambda _ result <- maybeToList $ uncons funcBody
    return
      ( applyRef
      , subst funcGuid
        ((fmap . Lens.over pSubstitutedArgs . IntSet.insert) (unRef arg) argExpr)
        result
      )

intoArgRule ::
  (Data.ExpressionBody RefExpression ->
   Maybe (Data.Lambda RefExpression)) ->
  Ref -> Data.Apply Ref -> Rule
intoArgRule uncons applyRef (Data.Apply func arg) =
  -- Recurse over PreSubst and PostSubst together
  --   When PreSubst part refers to its param:
  --     PostSubst part <=> arg
  -- (apply, func) -> arg
  Rule [applyRef, func] $ \ [applyExpr, Data.Expression funcGuid funcBody _] -> do
    Data.Lambda _ result <- maybeToList $ uncons funcBody
    mergeToArg funcGuid arg result applyExpr

intoFuncResultTypeRule ::
  (Data.Lambda RefExpression ->
   Data.ExpressionBody RefExpression) ->
  (Data.ExpressionBody RefExpression ->
   Maybe (Data.Lambda RefExpression)) ->
  Ref -> Ref -> Rule
intoFuncResultTypeRule cons uncons applyRef func =
  -- Propagate data from Apply's to the Func where appropriate.
  -- (Not on non-substituted holes)
  -- apply -> func result
  Rule [applyRef, func] $ \ [applyExpr, Data.Expression funcGuid funcBody _] -> do
    Data.Lambda paramT result <- maybeToList $ uncons funcBody
    return
      ( func
      , makeRefExpression funcGuid .
        cons . Data.Lambda paramT $
        mergeToPiResult result applyExpr
      )

recurseSubstRules ::
  (Data.Lambda RefExpression -> Data.ExpressionBody RefExpression) ->
  (Data.ExpressionBody RefExpression -> Maybe (Data.Lambda RefExpression)) ->
  Ref -> Data.Apply Ref -> [Rule]
recurseSubstRules cons uncons applyRef apply@(Data.Apply func _) =
  [ intoApplyResultRule uncons applyRef apply
  , intoArgRule uncons applyRef apply
  , intoFuncResultTypeRule cons uncons applyRef func
  ]

mergeToArg :: Guid -> Ref -> RefExpression -> RefExpression -> [(Ref, RefExpression)]
mergeToArg paramGuid arg =
  (fmap . fmap) (execWriter . Compose.unO) $
  Data.matchExpression onMatch onMismatch
  where
    unit = Compose.O (pure Unit)
    onMatch _ _ = unit
    onMismatch
      Data.Expression { Data.eValue = Data.ExpressionLeaf (Data.GetVariable (Data.ParameterRef g)) } post
      | g == paramGuid =
        Compose.O . (fmap . const) Unit $ Writer.tell
        [( arg
         , (fmap . Lens.over pSubstitutedArgs . IntSet.delete) (unRef arg) post
         )]
    onMismatch _ _ = unit

argTypeToPiParamTypeRule :: Guid -> Data.Apply TypedValue -> Rule
argTypeToPiParamTypeRule baseGuid (Data.Apply func arg) =
  -- ArgT => Pi ParamT
  Rule [tvType arg] $ \ [argTypeExpr] ->
  [( tvType func
   , makeRefExpression (augmentGuid "ar0" baseGuid) .
     Data.makePi argTypeExpr $ makeHole "ar1" baseGuid
   )]

rigidArgApplyTypeToResultTypeRule ::
  Guid -> TypedValue -> Data.Apply TypedValue -> Rule
rigidArgApplyTypeToResultTypeRule baseGuid applyTv (Data.Apply func arg) =
  -- If Arg is GetParam
  -- ApplyT (Susbt Arg with Hole) => ResultT
  Rule [tvType applyTv, tvVal arg] $ \ [applyTypeExpr, argExpr] ->
  case Data.eValue argExpr of
  Data.ExpressionLeaf (Data.GetVariable (Data.ParameterRef par)) ->
    [ ( tvType func
      , makeRefExpression (augmentGuid "ar2" baseGuid) .
        Data.makePi (makeHole "ar3" baseGuid) $
        subst par (makeHole "ar7" baseGuid) applyTypeExpr
      )
    ]
  _ -> []

piParamTypeToArgTypeRule :: Data.Apply TypedValue -> Rule
piParamTypeToArgTypeRule (Data.Apply func arg) =
  -- If func type is Pi
  -- Pi's ParamT => ArgT
  Rule [tvType func] $ \ [Data.Expression _ funcTExpr _] -> do
    Data.Lambda paramT _ <- maybeToList $ maybePi funcTExpr
    return (tvType arg, paramT)

lambdaParamTypeToArgTypeRule :: Data.Apply TypedValue -> Rule
lambdaParamTypeToArgTypeRule (Data.Apply func arg) =
  -- If func is Lambda
  -- Lambda's ParamT => ArgT
  Rule [tvVal func] $ \ [Data.Expression _ funcExpr _] -> do
    Data.Lambda paramT _ <- maybeToList $ maybeLambda funcExpr
    return (tvType arg, paramT)

argTypeToLambdaParamTypeRule :: Guid -> Data.Apply TypedValue -> Rule
argTypeToLambdaParamTypeRule baseGuid (Data.Apply func arg) =
  -- If func is Lambda,
  -- ArgT => Lambda's ParamT
  Rule [tvVal func, tvType arg] $
  \ [Data.Expression funcGuid funcExpr _, argTExpr] -> do
    _ <- maybeToList $ maybeLambda funcExpr
    return
      ( tvVal func
      , makeRefExpression funcGuid .
        Data.makeLambda argTExpr $ makeHole "ar5" baseGuid
      )

nonLambdaToApplyValueRule :: Guid -> TypedValue -> Data.Apply TypedValue -> Rule
nonLambdaToApplyValueRule baseGuid applyTv (Data.Apply func arg) =
  -- If func is surely not a lambda (a hole too could be a lambda).
  --
  -- Applies have a special case in the inferred value handling for
  -- redexes, and this rule is about detecting that an application is
  -- *not* a redex, so we can safely set the inferred value to the
  -- application itself.
  --
  -- Func Arg => Outer
  Rule [tvVal func, tvVal arg] $ \ [funcExpr, argExpr] ->
  case Data.eValue funcExpr of
  Data.ExpressionLambda _ -> []
  Data.ExpressionLeaf Data.Hole -> []
  _ ->
    [ ( tvVal applyTv
      , makeRefExpression (augmentGuid "ar6" baseGuid) $
        Data.makeApply funcExpr argExpr
      )
    ]

applyArgToFuncArgRule ::
  TypedValue -> Data.Apply TypedValue -> Rule
applyArgToFuncArgRule applyTv (Data.Apply func arg) =
  -- If Func is same as in Apply,
  -- Apply-Arg => Arg
  Rule [tvVal applyTv, tvVal func] $ \ [applyExpr, funcExpr] -> maybeToList $ do
    Data.Apply aFunc aArg <- maybeApply $ Data.eValue applyExpr
    _ <-
      Data.matchExpression
      ((const . const) (Just ()))
      ((const . const) Nothing)
      aFunc funcExpr
    return (tvVal arg, aArg)

applyRules :: Guid -> TypedValue -> Data.Apply TypedValue -> [Rule]
applyRules baseGuid applyTv apply@(Data.Apply func arg) =
  [ argTypeToPiParamTypeRule baseGuid apply
  , rigidArgApplyTypeToResultTypeRule baseGuid applyTv apply
  , piParamTypeToArgTypeRule apply
  , lambdaParamTypeToArgTypeRule apply
  , argTypeToLambdaParamTypeRule baseGuid apply
  , nonLambdaToApplyValueRule baseGuid applyTv apply
  , applyArgToFuncArgRule applyTv apply
  ]
  ++ recurseSubstRules Data.ExpressionPi maybePi
  -- TODO: This Data.Apply is a lie
    (tvType applyTv) (Data.Apply (tvType func) (tvVal arg))
  ++ recurseSubstRules Data.ExpressionLambda maybeLambda
    (tvVal applyTv) (Data.Apply (tvVal func) (tvVal arg))

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
      Lens.view (refMap . Lens.at (unRef k)) initialRefMap
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
      liftState $ sBfsCurLayer . Lens.contains key .= False
      Just (Rule deps ruleAction) <-
        liftState $ Lens.use (sRefMap . rules . Lens.at key)
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
      isJust $
      Data.matchExpression compareSubsts ((const . const) Nothing) x y
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
