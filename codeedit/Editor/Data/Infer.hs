{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell, DeriveFunctor, DeriveFoldable, DeriveTraversable, DeriveDataTypeable #-}
module Editor.Data.Infer
  ( Expression, Inferred(..), rExpression
  , Loaded, load, infer
  , InferNode(..), TypedValue(..)
  , Error(..), ErrorDetails(..)
  , RefMap, Ref
  , Loader(..), InferActions(..)
  , initial, newNodeWithScope, newTypedNodeWithScope
  ) where

import Control.Applicative (Applicative(..), (<$))
import Control.Arrow (first, second)
import Control.Lens ((%=), (.=), (^.), (+=))
import Control.Monad (guard, liftM, liftM2, unless, void, when)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Either (EitherT(..))
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.Monad.Trans.State (StateT(..), State, runState, execState)
import Control.Monad.Trans.Writer (Writer, execWriter)
import Control.Monad.Unit (Unit(..))
import Data.Binary (Binary(..), getWord8, putWord8)
import Data.Derive.Binary (makeBinary)
import Data.DeriveTH (derive)
import Data.Foldable (Foldable(..))
import Data.Functor.Identity (Identity(..))
import Data.IntMap (IntMap, (!))
import Data.IntSet (IntSet)
import Data.Map (Map)
import Data.Maybe (fromMaybe, isJust, mapMaybe, maybeToList)
import Data.Monoid (Monoid(..))
import Data.Store.Guid (Guid)
import Data.Traversable (Traversable)
import Data.Typeable (Typeable)
import qualified Control.Compose as Compose
import qualified Control.Lens as Lens
import qualified Control.Lens.TH as LensTH
import qualified Control.Monad.Trans.Either as Either
import qualified Control.Monad.Trans.Reader as Reader
import qualified Control.Monad.Trans.Writer as Writer
import qualified Data.Binary.Utils as BinaryUtils
import qualified Data.Foldable as Foldable
import qualified Data.IntSet as IntSet
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Store.Guid as Guid
import qualified Data.Store.IRef as IRef
import qualified Data.Traversable as Traversable
import qualified Editor.Data as Data
import qualified System.Random as Random

newtype Ref = Ref { unRef :: Int } deriving (Eq, Ord)
derive makeBinary ''Ref

instance Show Ref where
  show = ('R' :) . show . unRef

data TypedValue = TypedValue
  { tvVal :: Ref
  , tvType :: Ref
  }
derive makeBinary ''TypedValue
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

data RefExprPayload = RefExprPayload
  { _rplSubstitutedArgs :: IntSet
  , _rplId :: Guid -- For cycle-detection. TODO: Use non-Guid?
  } deriving (Show)
derive makeBinary ''RefExprPayload

type RefExpression = Data.Expression RefExprPayload

refExprFromPure :: Data.Expression Guid -> RefExpression
refExprFromPure = fmap (RefExprPayload mempty)

type RuleFunction = [RefExpression] -> [(Ref, RefExpression)]

maybeLambda :: Data.ExpressionBody a -> Maybe (Data.Lambda a)
maybeLambda (Data.ExpressionLambda x) = Just x
maybeLambda _ = Nothing

maybePi :: Data.ExpressionBody a -> Maybe (Data.Lambda a)
maybePi (Data.ExpressionPi x) = Just x
maybePi _ = Nothing

data ExprLambdaWrapper = ExprLambda | ExprPi
derive makeBinary ''ExprLambdaWrapper

exprLambdaCons :: ExprLambdaWrapper -> Data.Lambda expr -> Data.ExpressionBody expr
exprLambdaCons ExprLambda = Data.ExpressionLambda
exprLambdaCons ExprPi = Data.ExpressionPi

exprLambdaUncons :: ExprLambdaWrapper -> Data.ExpressionBody expr -> Maybe (Data.Lambda expr)
exprLambdaUncons ExprLambda = maybeLambda
exprLambdaUncons ExprPi = maybePi

-- Boilerplate to work around lack of serialization of functions
-- Represents a serialization of RuleFunction:
data RuleClosure
  = LambdaBodyTypeToPiResultTypeClosure (Guid, Ref)
  | PiToLambdaClosure (Guid, Ref, Ref)
  | CopyClosure Ref
  | LambdaParentToChildrenClosure (ExprLambdaWrapper, Ref, Ref)
  | LambdaChildrenToParentClosure (ExprLambdaWrapper, Guid, Guid, Ref)
  | SetClosure [(Ref, RefExpression)]
  | SimpleTypeClosure Ref
  | IntoApplyResultClosure (ExprLambdaWrapper, Ref, Ref)
  | IntoArgClosure (ExprLambdaWrapper, Ref)
  | IntoFuncResultTypeClosure (ExprLambdaWrapper, Ref)
  | ArgTypeToPiParamTypeClosure (Guid, Ref)
  | RigidArgApplyTypeToResultTypeClosure (Guid, Ref)
  | PiParamTypeToArgTypeClosure Ref
  | LambdaParamTypeToArgTypeClosure Ref
  | ArgTypeToLambdaParamTypeClosure (Guid, Ref)
  | NonLambdaToApplyValueClosure (Guid, Ref)
  | ApplyArgToFuncArgClosure Ref
derive makeBinary ''RuleClosure

data Rule = Rule
  { ruleInputs :: [Ref]
  , _ruleCompute :: RuleClosure
  }
derive makeBinary ''Rule

data RefData = RefData
  { _rExpression :: RefExpression
  , _rRules :: [Int] -- Rule id
  }
derive makeBinary ''RefData

makeRefExpression :: Guid -> Data.ExpressionBody RefExpression -> RefExpression
makeRefExpression g expr = Data.Expression expr $ RefExprPayload mempty g

makeHole :: String -> Guid -> RefExpression
makeHole s g =
  makeRefExpression (Guid.augment s g) $ Data.ExpressionLeaf Data.Hole

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
  } deriving (Typeable)
derive makeBinary ''RefMap

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
  } deriving (Typeable)
derive makeBinary ''InferNode

data Inferred a = Inferred
  { iStored :: a
  , iValue :: Data.PureExpression
  , iType :: Data.PureExpression
  , iScope :: Map Guid Data.PureExpression
  , iPoint :: InferNode
  } deriving (Functor, Foldable, Traversable)
derive makeBinary ''Inferred

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
  Map Data.DefinitionIRef Data.PureExpression ->
  Scope -> Data.Expression Guid ->
  (Data.Expression Guid, Data.Expression Guid)
initialExprs defTypes scope entity =
  first (`Data.Expression` entityGuid) $
  case exprStructure of
  Data.ExpressionApply _ -> (Data.ExpressionLeaf Data.Hole, holeType)
  Data.ExpressionLeaf (Data.GetVariable var@(Data.DefinitionRef ref))
    | not (Map.member var scope) ->
      (,) exprStructure $ Map.mapWithKey inventGuid defTypes Map.! ref
  _ -> (exprStructure, holeType)
  where
    gen = Random.mkStdGen . BinaryUtils.decodeS . Guid.bs . IRef.guid
    inventGuid defI defType = Data.randomizeExpr (gen defI) $ id <$ defType
    entityGuid = Data.ePayload entity
    mkGuid name = Guid.augment name entityGuid
    exprStructure = fmap (const innerHole) $ Data.eValue entity
    hole name = Data.Expression (Data.ExpressionLeaf Data.Hole) $ mkGuid name
    innerHole = hole "innerHole"
    holeType = hole "type"

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

guidRepeat :: RefExpression -> Bool
guidRepeat =
  go Set.empty
  where
    go forbidden (Data.Expression body pl)
      | Set.member g forbidden = True
      | otherwise =
        Foldable.any (go (Set.insert g forbidden)) body
      where
        g = Lens.view rplId pl

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
    addSubstituted addition =
      Lens.over rplSubstitutedArgs
      ((mappend . Lens.view rplSubstitutedArgs) addition)
    onMatch x y = return $ addSubstituted y x
    onMismatch (Data.Expression (Data.ExpressionLeaf Data.Hole) s0) e1 =
      return $ fmap (addSubstituted s0) e1
    onMismatch e0 (Data.Expression (Data.ExpressionLeaf Data.Hole) s1) =
      return $ fmap (addSubstituted s1) e0
    onMismatch e0 e1 =
      Either.left $ MismatchIn (void e0) (void e1)

runLambdaBodyTypeToPiResultTypeClosure :: (Guid, Ref) -> RuleFunction
runLambdaBodyTypeToPiResultTypeClosure (param, lambdaTypeRef) ~[bodyTypeExpr] =
  [( lambdaTypeRef
   , makeRefExpression param $ Data.makePi param (makeHole "paramType" param) bodyTypeExpr
   )]

runPiToLambdaClosure :: (Guid, Ref, Ref) -> RuleFunction
runPiToLambdaClosure (param, lambdaValueRef, bodyTypeRef) ~[Data.Expression piBody _] = do
  Data.Lambda piParam paramType resultType <- maybeToList $ maybePi piBody
  [ -- Pi result type -> Body type
    ( bodyTypeRef
    , subst piParam
      ( makeRefExpression (Guid.fromString "getVar")
        (Data.makeParameterRef param)
      )
      resultType
    )
    , -- Pi param type -> Lambda param type
      ( lambdaValueRef
      , makeRefExpression param . Data.makeLambda piParam paramType $ makeHole "body" param
      )
    ]

lambdaRules :: Guid -> TypedValue -> Ref -> [Rule]
lambdaRules param (TypedValue lambdaValueRef lambdaTypeRef) bodyTypeRef =
  [ Rule [bodyTypeRef] $ LambdaBodyTypeToPiResultTypeClosure (param, lambdaTypeRef)
  , Rule [lambdaTypeRef] $ PiToLambdaClosure (param, lambdaValueRef, bodyTypeRef)
  ]

runCopyClosure :: Ref -> RuleFunction
runCopyClosure dest ~[srcExpr] = [(dest, srcExpr)]

unionRules :: Ref -> Ref -> [Rule]
unionRules x y =
  [ Rule [x] $ CopyClosure y
  , Rule [y] $ CopyClosure x
  ]

-- Parent lambda to children
runLambdaParentToChildrenClosure :: (ExprLambdaWrapper, Ref, Ref) -> RuleFunction
runLambdaParentToChildrenClosure (cons, paramTypeRef, resultRef) ~[expr] = do
  Data.Lambda _ paramTypeE resultE <-
    maybeToList . exprLambdaUncons cons $ Data.eValue expr
  [(paramTypeRef, paramTypeE), (resultRef, resultE)]

-- Children of lambda to lambda parent
runLambdaChildrenToParentClosure :: (ExprLambdaWrapper, Guid, Guid, Ref) -> RuleFunction
runLambdaChildrenToParentClosure (cons, exprGuid, param, lamRef) ~[paramTypeExpr, resultExpr] =
  [( lamRef
   , makeRefExpression exprGuid . exprLambdaCons cons $
     Data.Lambda param paramTypeExpr resultExpr
   )]

lambdaStructureRules :: ExprLambdaWrapper -> Guid -> Ref -> Data.Lambda Ref -> [Rule]
lambdaStructureRules cons exprGuid lamRef (Data.Lambda param paramTypeRef resultRef) =
  [ Rule [lamRef] $
    LambdaParentToChildrenClosure (cons, paramTypeRef, resultRef)
  , -- Copy the structure from the children to the parent
    Rule [paramTypeRef, resultRef] $
    LambdaChildrenToParentClosure (cons, exprGuid, param, lamRef)
  ]

runSetClosure :: [(Ref, RefExpression)] -> RuleFunction
runSetClosure outputs ~[] = outputs

makeNodeRules :: Guid -> Data.Expression InferNode -> [Rule]
makeNodeRules exprGuid (Data.Expression exprBody (InferNode typedVal scope)) =
  case fmap (nRefs . Data.ePayload) exprBody of
  Data.ExpressionPi lambda@(Data.Lambda _ _ resultType) ->
    setRule (tvType resultType) :
    onLambda ExprPi lambda
  Data.ExpressionLambda lambda@(Data.Lambda param _ body) ->
    lambdaRules param typedVal (tvType body) ++
    onLambda ExprLambda lambda
  Data.ExpressionApply apply -> applyRules exprGuid typedVal apply
  Data.ExpressionLeaf (Data.GetVariable var) -> do
    ref <- maybeToList $ Map.lookup var scope
    unionRules ref $ tvType typedVal
  _ -> []
  where
    setRule ref = Rule [] $ SetClosure [(ref, setExpr)]
    onLambda cons lam@(Data.Lambda _ paramType _) =
      setRule (tvType paramType) :
      lambdaStructureRules cons exprGuid (tvVal typedVal) (fmap tvVal lam)

makeResumptionRules :: Data.Expression (Guid, InferNode) -> [Rule]
makeResumptionRules expr =
  makeNodeRules (fst (Data.ePayload expr)) (fmap snd expr) ++
  (Foldable.concat . fmap makeAllRules . Data.eValue) expr

makeAllRules :: Data.Expression (Guid, InferNode) -> [Rule]
makeAllRules expr =
  (ruleSimpleType . nRefs . snd . Data.ePayload) expr :
  makeResumptionRules expr

loadNode ::
  Map Data.DefinitionIRef Data.PureExpression -> Scope ->
  Data.Expression (Guid, s) -> TypedValue ->
  State RefMap (Data.Expression (InferNode, (Guid, s)))
loadNode defTypes scope entity typedValue = do
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
    Data.Expression exprBody
    (InferNode typedValue scope, Data.ePayload entity)
  where
    onLambda cons (Data.Lambda param paramType@(_, paramTypeTv) result) = do
      paramTypeR <- go id paramType
      let paramRef = Data.ParameterRef param
      liftM (cons . Data.Lambda param paramTypeR) $
        go (Map.insert paramRef (tvVal paramTypeTv)) result
    go onScope = uncurry . loadNode defTypes $ onScope scope
    addTypedVal x =
      liftM ((,) x) createTypedVal
    initializeRefData ref expr =
      refMap . Lens.at (unRef ref) .=
      Just (RefData (refExprFromPure expr) [])
    (initialVal, initialType) =
      initialExprs defTypes scope $ fmap fst entity
    setInitialValues = do
      initializeRefData (tvVal typedValue) initialVal
      initializeRefData (tvType typedValue) initialType

subst ::
  Guid -> Data.Expression a ->
  Data.Expression a -> Data.Expression a
subst from to expr =
  case Data.eValue expr of
  Data.ExpressionLeaf (Data.GetVariable (Data.ParameterRef paramRef))
    | paramRef == from -> to
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
    onMismatch destHole@(Data.Expression (Data.ExpressionLeaf Data.Hole) destPayload) src
      | IntSet.null substs = destHole
      | otherwise = fmap (Lens.set rplSubstitutedArgs substs) src
      where
        substs = destPayload ^. rplSubstitutedArgs
    -- TODO: This seems like it should report an error,
    -- verify/document that it is OK because the other direction of
    -- information flow will catch any error:
    onMismatch dest _ = dest

maybeApply :: Data.ExpressionBody a -> Maybe (Data.Apply a)
maybeApply (Data.ExpressionApply x) = Just x
maybeApply _ = Nothing

runSimpleTypeClosure :: Ref -> RuleFunction
runSimpleTypeClosure typ ~[valExpr] =
  case valExpr of
  Data.Expression valExprBody (RefExprPayload { _rplId = exprGuid }) ->
    case valExprBody of
    Data.ExpressionLeaf Data.Set -> [(typ, setExpr)]
    Data.ExpressionLeaf Data.IntegerType -> [(typ, setExpr)]
    Data.ExpressionLeaf (Data.LiteralInteger _) -> [(typ, intTypeExpr)]
    Data.ExpressionPi _ -> [(typ, setExpr)]
    Data.ExpressionLambda (Data.Lambda param paramType _) ->
      [( typ
       , makeRefExpression exprGuid . Data.makePi param paramType $
         makeHole "lambdaBody" exprGuid
       )]
    _ -> []

ruleSimpleType :: TypedValue -> Rule
ruleSimpleType (TypedValue val typ) = Rule [val] $ SimpleTypeClosure typ

runIntoApplyResultClosure :: (ExprLambdaWrapper, Ref, Ref) -> RuleFunction
runIntoApplyResultClosure (cons, applyRef, arg) ~[Data.Expression funcBody _, argExpr] = do
  Data.Lambda param _ result <- maybeToList $ exprLambdaUncons cons funcBody
  return
    ( applyRef
    , subst param
      ((Lens.over (Lens.mapped . rplSubstitutedArgs) . IntSet.insert) (unRef arg) argExpr)
      result
    )

intoApplyResultRule :: ExprLambdaWrapper -> Ref -> Ref -> Ref -> Rule
intoApplyResultRule cons applyRef func arg =
  -- PreSubst with Subst => PostSubst
  -- (func, arg) -> apply
  Rule [func, arg] $ IntoApplyResultClosure (cons, applyRef, arg)

runIntoArgClosure :: (ExprLambdaWrapper, Ref) -> RuleFunction
runIntoArgClosure (cons, arg) ~[applyExpr, Data.Expression funcBody _] = do
  -- Recurse over PreSubst and PostSubst together
  --   When PreSubst part refers to its param:
  --     PostSubst part <=> arg
  -- (apply, func) -> arg
  Data.Lambda param _ result <- maybeToList $ exprLambdaUncons cons funcBody
  mergeToArg param arg result applyExpr

intoArgRule :: ExprLambdaWrapper -> Ref -> Ref -> Ref -> Rule
intoArgRule cons applyRef func arg =
  Rule [applyRef, func] $ IntoArgClosure (cons, arg)

runIntoFuncResultTypeClosure :: (ExprLambdaWrapper, Ref) -> RuleFunction
runIntoFuncResultTypeClosure (cons, func) ~[applyExpr, Data.Expression funcBody funcPl] = do
  Data.Lambda param paramT result <- maybeToList $ exprLambdaUncons cons funcBody
  return
    ( func
    , makeRefExpression (Lens.view rplId funcPl) .
      exprLambdaCons cons . Data.Lambda param paramT $
      mergeToPiResult result applyExpr
    )

intoFuncResultTypeRule :: ExprLambdaWrapper -> Ref -> Ref -> Rule
intoFuncResultTypeRule cons applyRef func =
  -- Propagate data from Apply's to the Func where appropriate.
  -- (Not on non-substituted holes)
  -- apply -> func result
  Rule [applyRef, func] $ IntoFuncResultTypeClosure (cons, func)

recurseSubstRules :: ExprLambdaWrapper -> Ref -> Ref -> Ref -> [Rule]
recurseSubstRules cons applyRef func arg =
  [ intoApplyResultRule cons applyRef func arg
  , intoArgRule cons applyRef func arg
  , intoFuncResultTypeRule cons applyRef func
  ]

mergeToArg :: Guid -> Ref -> RefExpression -> RefExpression -> [(Ref, RefExpression)]
mergeToArg param arg =
  (fmap . fmap) (execWriter . Compose.unO) $
  Data.matchExpression onMatch onMismatch
  where
    unit = Compose.O (pure Unit)
    onMatch _ _ = unit
    onMismatch
      Data.Expression { Data.eValue = Data.ExpressionLeaf (Data.GetVariable (Data.ParameterRef g)) } post
      | g == param =
        Compose.O . (fmap . const) Unit $ Writer.tell
        [( arg
         , (Lens.over (Lens.mapped . rplSubstitutedArgs) . IntSet.delete) (unRef arg) post
         )]
    onMismatch _ _ = unit

runArgTypeToPiParamTypeClosure :: (Guid, Ref) -> RuleFunction
runArgTypeToPiParamTypeClosure (baseGuid, funcTypeRef) ~[argTypeExpr] =
  [( funcTypeRef
   , makeRefExpression piGuid .
     Data.makePi piGuid argTypeExpr $ makeHole "ar1" baseGuid
   )]
  where
    piGuid = Guid.augment "ar0" baseGuid

argTypeToPiParamTypeRule :: Guid -> Data.Apply TypedValue -> Rule
argTypeToPiParamTypeRule baseGuid (Data.Apply func arg) =
  -- ArgT => Pi ParamT
  Rule [tvType arg] $ ArgTypeToPiParamTypeClosure (baseGuid, tvType func)

runRigidArgApplyTypeToResultTypeClosure :: (Guid, Ref) -> RuleFunction
runRigidArgApplyTypeToResultTypeClosure (baseGuid, funcTypeRef) ~[applyTypeExpr, argExpr] =
  case Data.eValue argExpr of
  Data.ExpressionLeaf (Data.GetVariable (Data.ParameterRef par)) ->
    [ ( funcTypeRef
      , makeRefExpression piGuid .
        Data.makePi piGuid (makeHole "ar3" baseGuid) $
        subst par (makeHole "ar7" baseGuid) applyTypeExpr
      )
    ]
  _ -> []
  where
    piGuid = Guid.augment "ar2" baseGuid

rigidArgApplyTypeToResultTypeRule ::
  Guid -> TypedValue -> Data.Apply TypedValue -> Rule
rigidArgApplyTypeToResultTypeRule baseGuid applyTv (Data.Apply func arg) =
  -- If Arg is GetParam
  -- ApplyT (Susbt Arg with Hole) => ResultT
  Rule [tvType applyTv, tvVal arg] $
  RigidArgApplyTypeToResultTypeClosure (baseGuid, tvType func)

runPiParamTypeToArgTypeClosure :: Ref -> RuleFunction
runPiParamTypeToArgTypeClosure argTypeRef ~[Data.Expression funcTExpr _] = do
  -- If func type is Pi
  -- Pi's ParamT => ArgT
  Data.Lambda _ paramT _ <- maybeToList $ maybePi funcTExpr
  return (argTypeRef, paramT)

piParamTypeToArgTypeRule :: Data.Apply TypedValue -> Rule
piParamTypeToArgTypeRule (Data.Apply func arg) =
  Rule [tvType func] $ PiParamTypeToArgTypeClosure (tvType arg)

runLambdaParamTypeToArgTypeClosure :: Ref -> RuleFunction
runLambdaParamTypeToArgTypeClosure argTypeRef ~[Data.Expression funcExpr _] = do
  -- If func is Lambda
  -- Lambda's ParamT => ArgT
  Data.Lambda _ paramT _ <- maybeToList $ maybeLambda funcExpr
  return (argTypeRef, paramT)

lambdaParamTypeToArgTypeRule :: Data.Apply TypedValue -> Rule
lambdaParamTypeToArgTypeRule (Data.Apply func arg) =
  Rule [tvVal func] $ LambdaParamTypeToArgTypeClosure (tvType arg)

runArgTypeToLambdaParamTypeClosure :: (Guid, Ref) -> RuleFunction
runArgTypeToLambdaParamTypeClosure (baseGuid, funcValRef) ~[Data.Expression funcExpr funcPl, argTExpr] = do
  -- If func is Lambda,
  -- ArgT => Lambda's ParamT
  Data.Lambda param _ _ <- maybeToList $ maybeLambda funcExpr
  return
    ( funcValRef
    , makeRefExpression (Lens.view rplId funcPl) .
      Data.makeLambda param argTExpr $ makeHole "ar5" baseGuid
    )

argTypeToLambdaParamTypeRule :: Guid -> Data.Apply TypedValue -> Rule
argTypeToLambdaParamTypeRule baseGuid (Data.Apply func arg) =
  Rule [tvVal func, tvType arg] $ ArgTypeToLambdaParamTypeClosure (baseGuid, tvVal func)

runNonLambdaToApplyValueClosure :: (Guid, Ref) -> RuleFunction
runNonLambdaToApplyValueClosure (baseGuid, applyValRef) ~[funcExpr, argExpr] =
  -- If func is surely not a lambda (a hole too could be a lambda).
  --
  -- Applies have a special case in the inferred value handling for
  -- redexes, and this rule is about detecting that an application is
  -- *not* a redex, so we can safely set the inferred value to the
  -- application itself.
  --
  -- Func Arg => Outer
  case Data.eValue funcExpr of
  Data.ExpressionLambda _ -> []
  Data.ExpressionLeaf Data.Hole -> []
  _ ->
    [ ( applyValRef
      , makeRefExpression (Guid.augment "ar6" baseGuid) $
        Data.makeApply funcExpr argExpr
      )
    ]

nonLambdaToApplyValueRule :: Guid -> TypedValue -> Data.Apply TypedValue -> Rule
nonLambdaToApplyValueRule baseGuid applyTv (Data.Apply func arg) =
  Rule [tvVal func, tvVal arg] $
  NonLambdaToApplyValueClosure (baseGuid, tvVal applyTv)

runApplyArgToFuncArgClosure :: Ref -> RuleFunction
runApplyArgToFuncArgClosure argValRef ~[applyExpr, funcExpr] = maybeToList $ do
  -- If Func is same as in Apply,
  -- Apply-Arg => Arg
  Data.Apply aFunc aArg <- maybeApply $ Data.eValue applyExpr
  _ <-
    Data.matchExpression
    ((const . const) (Just ()))
    ((const . const) Nothing)
    aFunc funcExpr
  return (argValRef, aArg)

runRuleClosure :: RuleClosure -> RuleFunction
runRuleClosure closure =
  case closure of
  LambdaBodyTypeToPiResultTypeClosure x ->
    runLambdaBodyTypeToPiResultTypeClosure x
  PiToLambdaClosure x ->
    runPiToLambdaClosure x
  CopyClosure x ->
    runCopyClosure x
  LambdaParentToChildrenClosure x ->
    runLambdaParentToChildrenClosure x
  LambdaChildrenToParentClosure x ->
    runLambdaChildrenToParentClosure x
  SetClosure x ->
    runSetClosure x
  SimpleTypeClosure x ->
    runSimpleTypeClosure x
  IntoApplyResultClosure x ->
    runIntoApplyResultClosure x
  IntoArgClosure x ->
    runIntoArgClosure x
  IntoFuncResultTypeClosure x ->
    runIntoFuncResultTypeClosure x
  ArgTypeToPiParamTypeClosure x ->
    runArgTypeToPiParamTypeClosure x
  RigidArgApplyTypeToResultTypeClosure x ->
    runRigidArgApplyTypeToResultTypeClosure x
  PiParamTypeToArgTypeClosure x ->
    runPiParamTypeToArgTypeClosure x
  LambdaParamTypeToArgTypeClosure x ->
    runLambdaParamTypeToArgTypeClosure x
  ArgTypeToLambdaParamTypeClosure x ->
    runArgTypeToLambdaParamTypeClosure x
  NonLambdaToApplyValueClosure x ->
    runNonLambdaToApplyValueClosure x
  ApplyArgToFuncArgClosure x ->
    runApplyArgToFuncArgClosure x

applyArgToFuncArgRule ::
  TypedValue -> Data.Apply TypedValue -> Rule
applyArgToFuncArgRule applyTv (Data.Apply func arg) =
  Rule [tvVal applyTv, tvVal func] $ ApplyArgToFuncArgClosure (tvVal arg)

applyRules :: Guid -> TypedValue -> Data.Apply TypedValue -> [Rule]
applyRules baseGuid applyTv apply@(Data.Apply func arg) =
  -- TODO: make all of these functions have a standard signature and
  -- just apply them all to the same args?
  [ argTypeToPiParamTypeRule baseGuid apply
  , rigidArgApplyTypeToResultTypeRule baseGuid applyTv apply
  , piParamTypeToArgTypeRule apply
  , lambdaParamTypeToArgTypeRule apply
  , argTypeToLambdaParamTypeRule baseGuid apply
  , nonLambdaToApplyValueRule baseGuid applyTv apply
  , applyArgToFuncArgRule applyTv apply
  ]
  ++ recurseSubstRules ExprPi
    (tvType applyTv) (tvType func) (tvVal arg)
  ++ recurseSubstRules ExprLambda
    (tvVal applyTv) (tvVal func) (tvVal arg)

data Loaded a = Loaded
  { lRealExpr :: Data.Expression a
  , lMRecursiveDef :: Maybe Data.DefinitionIRef
  , lDefinitionTypes :: Map Data.DefinitionIRef Data.PureExpression
  } deriving (Typeable)
derive makeBinary ''Loaded

ordNub :: Ord a => [a] -> [a]
ordNub = Set.toList . Set.fromList

load ::
  Monad m => Loader m ->
  Maybe Data.DefinitionIRef -> Data.Expression a ->
  m (Loaded a)
load loader mRecursiveDef expr =
  liftM (Loaded expr mRecursiveDef . Map.fromList) .
  mapM loadType $ ordNub
  [ defI
  | Data.ExpressionLeaf (Data.GetVariable (Data.DefinitionRef defI)) <-
    map Data.eValue $ Data.subExpressions expr
  , Just defI /= mRecursiveDef
  ]
  where
    loadType defI = liftM ((,) defI) $ loadPureDefinitionType loader defI

-- TODO: Preprocessed used to be "Loaded". Now it is just a step in
-- "infer". Does it still make sense to keep it separately the way it
-- is?
data Preprocessed a = Preprocessed
  { lExpr :: Data.Expression (InferNode, (Guid, a))
  , lRefMap :: RefMap
  , lSavedRoot :: (Maybe RefData, Maybe RefData)
  }

preprocess :: Loaded a -> RefMap -> InferNode -> Preprocessed a
preprocess loaded initialRefMap (InferNode rootTv rootScope) =
  buildLoaded . (`runState` initialRefMap) $
  loadNode (lDefinitionTypes loaded) scope guidExpr rootTv
  where
    TypedValue rootValR rootTypR = rootTv
    gen = Random.mkStdGen $ Lens.view nextRef initialRefMap
    guidExpr =
      Data.randomizeExpr gen . fmap (flip (,)) $ lRealExpr loaded
    initialMRefData k =
      Lens.view (refMap . Lens.at (unRef k)) initialRefMap
    buildLoaded (node, resultRefMap) = Preprocessed
      { lExpr = node
      , lRefMap = resultRefMap
      , lSavedRoot =
        ( initialMRefData rootValR
        , initialMRefData rootTypR
        )
      }
    scope =
      case lMRecursiveDef loaded of
      Nothing -> rootScope
      Just iref -> Map.insert (Data.DefinitionRef iref) (tvType rootTv) rootScope

postProcess ::
  RefMap -> Data.Expression (InferNode, a) -> (Expression a, RefMap)
postProcess resultRefMap expr =
  (fmap derefNode expr, resultRefMap)
  where
    derefNode (inferNode, s) =
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

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

runInferT ::
  Monad m => InferActions m -> InferState ->
  InferT m a -> m (InferState, a)
runInferT actions state =
  liftM swap . (`runStateT` state) . (`runReaderT` actions) . unInferT

liftActions :: ReaderT (InferActions m) (StateT InferState m) a -> InferT m a
liftActions = InferT

liftState :: Monad m => StateT InferState m a -> InferT m a
liftState = liftActions . lift

{-# SPECIALIZE liftState :: StateT InferState Maybe a -> InferT Maybe a #-}
{-# SPECIALIZE liftState :: Monoid w => StateT InferState (Writer w) a -> InferT (Writer w) a #-}

instance MonadTrans InferT where
  lift = liftState . lift

infer ::
  Monad m => InferActions m -> Loaded a -> RefMap -> InferNode ->
  m (Expression a, RefMap)
infer actions loaded initialRefMap node =
  liftM
  ( uncurry postProcess
  . first (Lens.view sRefMap)
  . (second . fmap . second) snd -- Get rid of (Guid,)
  ) . runInferT actions ruleInferState $ do
    restoreRoot rootValR rootValMRefData
    restoreRoot rootTypR rootTypMRefData
    -- when we resume load,
    -- we want to trigger the existing rules for the loaded root
    touch rootValR
    touch rootTypR
    go
    return expr
  where
    Preprocessed expr loadedRefMap (rootValMRefData, rootTypMRefData) =
      preprocess loaded initialRefMap node
    ruleInferState =
      (`execState` InferState loadedRefMap mempty mempty) .
      mapM_ addRule .
      makeRules rootValMRefData $ fmap (first fst . swap) expr
    makeRules Nothing = makeAllRules
    makeRules (Just _) = makeResumptionRules
    TypedValue rootValR rootTypR = nRefs . fst $ Data.ePayload expr
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
      Just (Rule deps ruleClosure) <-
        liftState $ Lens.use (sRefMap . rules . Lens.at key)
      refExps <- mapM getRefExpr deps
      mapM_ (uncurry setRefExpr) $ runRuleClosure ruleClosure refExps

{-# SPECIALIZE
  infer :: InferActions Maybe -> Loaded a -> RefMap -> InferNode ->
           Maybe (Expression a, RefMap) #-}
{-# SPECIALIZE
  infer :: Monoid w => InferActions (Writer w) -> Loaded a ->
           RefMap -> InferNode ->
           Writer w (Expression a, RefMap) #-}

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
    compareSubsts x y = guard $ (x ^. rplSubstitutedArgs) == (y ^. rplSubstitutedArgs)

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
