{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving #-}
module Editor.Data.Typed
  ( LoopGuidExpression(..)
  , pureGuidFromLoop

  , Expression(..), StoredExpression
  , pureInferExpressionWithinContext
  , atEeInferredType, atEeValue
  , loadInferExpression, inferExpression
  , toPureExpression
  , alphaEq

  , StoredDefinition(..)
  , atDeIRef, atDeValue
  , deGuid
  , loadInferDefinition
  , loadDefTypeWithinContext

  , Ref
  , TypeContext, emptyTypeContext
  , Infer, resumeInfer
  , unify, derefRef, derefResumedInfer
  , makeSingletonRef
  ) where

import Control.Applicative (Applicative, liftA2)
import Control.Monad (liftM, liftM2, (<=<), when, unless, filterM)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Random (nextRandom, runRandomT)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.Monad.Trans.State (execStateT)
import Control.Monad.Trans.UnionFind (UnionFindT, resumeUnionFindT)
import Data.Function (on)
import Data.Functor.Identity (Identity(..))
import Data.Maybe (fromMaybe)
import Data.Monoid (Any(..), mconcat)
import Data.Store.Guid (Guid)
import Data.Store.Transaction (Transaction)
import Data.UnionFind.IntMap (UnionFind, newUnionFind)
import Editor.Anchors (ViewTag)
import System.Random (RandomGen)
import qualified Control.Monad.Trans.Reader as Reader
import qualified Control.Monad.Trans.State as State
import qualified Control.Monad.Trans.UnionFind as UnionFindT
import qualified Data.AtFieldTH as AtFieldTH
import qualified Data.Map as Map
import qualified Data.Store.Guid as Guid
import qualified Data.Store.IRef as IRef
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Data.UnionFind.IntMap as UnionFind
import qualified Editor.Anchors as Anchors
import qualified Editor.Data as Data
import qualified Editor.Data.Load as DataLoad
import qualified System.Random as Random
import qualified System.Random.Utils as RandomUtils

type T = Transaction ViewTag

eipGuid :: Data.ExpressionIRefProperty m -> Guid
eipGuid = IRef.guid . Data.unExpressionIRef . Property.value

-- Rules about inferring apply type/value relationships:
--
-- If you have:
--
-- func:funcT  arg:argT
-- --------------------
--     apply:applyT
--
-- then:
--
-- resultT <=> new independent typeref
-- funcT <=> Pi (x:argT) -> resultT
-- applyT <= foreach Pi in funcT, result of the Pi: subst: (x => arg)
-- apply <= foreach Lam in func, result(body) subst'd: (x => arg)
--
-- Whenever a Pi result component is a hole, it can be polymorphic,
-- which means it can refer to the Pi arg[s] before it, in which case
-- it would not be valid to infer its type as monomorphic (unify into
-- it).

data ArrowType = Pi | Lam
  deriving (Show, Eq)

data UnifyRule = UnifyRule
  { urArrowType :: ArrowType
  , urDest :: Ref
  , urArg :: Ref
  } deriving (Show)

data Constraints = Constraints
  { tcExprs :: [Data.GuidExpression Ref]
  , tcRules :: [UnifyRule]
  } deriving (Show)

emptyConstraints :: Constraints
emptyConstraints = Constraints [] []

type Ref = UnionFindT.Point Constraints

data Expression s = Expression
  { eeRef :: s
  , eeGuid :: Guid
  , eeInferredType :: Ref
  , eeInferredValue :: Ref
  , eeValue :: Data.Expression (Expression s)
  }

type StoredExpression f = Expression (Data.ExpressionIRefProperty f)

data LoopGuidExpression
  = NoLoop (Data.GuidExpression LoopGuidExpression)
  | Loop Guid
  deriving (Show, Eq)

type TypeContext = UnionFind Constraints

emptyTypeContext :: TypeContext
emptyTypeContext = newUnionFind

data StoredDefinition m = StoredDefinition
  { deIRef :: Data.DefinitionIRef
  , deInferredType :: Ref
  , deValue :: Data.Definition (StoredExpression m)
  , deTypeContext :: TypeContext
  }

deGuid :: StoredDefinition m -> Guid
deGuid = IRef.guid . deIRef

AtFieldTH.make ''Expression
AtFieldTH.make ''StoredDefinition

--------------- Infer Stack boilerplate:

newtype Infer m a = Infer
  { unInfer :: UnionFindT Constraints m a
  } deriving (Functor, Applicative, Monad, MonadTrans)
AtFieldTH.make ''Infer

----------------- Infer operations:

makeRef :: Monad m => Constraints -> Infer m Ref
makeRef = Infer . UnionFindT.new

getRef :: Monad m => Ref -> Infer m Constraints
getRef = Infer . UnionFindT.descr

runInfer :: Monad m => Infer m a -> m (TypeContext, a)
runInfer = resumeInfer emptyTypeContext

resumeInfer :: Monad m => TypeContext -> Infer m a -> m (TypeContext, a)
resumeInfer typeContext = resumeUnionFindT typeContext . unInfer

makeNoConstraints :: Monad m => Infer m Ref
makeNoConstraints = makeRef emptyConstraints

makeRuleRef :: Monad m => UnifyRule -> Infer m Ref
makeRuleRef rule = makeRef emptyConstraints { tcRules = [rule] }

makeSingletonRef :: Monad m => Guid -> Data.Expression Ref -> Infer m Ref
makeSingletonRef _ Data.ExpressionHole = makeNoConstraints
makeSingletonRef guid expr =
  makeRef emptyConstraints
  { tcExprs = [Data.GuidExpression guid expr] }

--------------

-- Entities whose Guid does not matter until canonization can just use
-- a zero guid:
zeroGuid :: Guid
zeroGuid = Guid.fromString "ZeroGuid"

fromExpression
  :: Monad m => (Guid -> Data.Expression ref -> m ref)
  -> Expression s -> m ref
fromExpression mk =
  Data.mapMExpression f
  where
    f (Expression _ g _ _ val) = (return val, mk g)

pureGuidFromLoop :: LoopGuidExpression -> Maybe Data.PureGuidExpression
pureGuidFromLoop =
  Data.mapMExpression f
  where
    f Loop {} = ( Nothing, const Nothing )
    f (NoLoop (Data.GuidExpression guid itlExpr)) =
      ( Just itlExpr, Just . Data.PureGuidExpression . Data.GuidExpression guid )

toPureExpression :: Expression s -> Data.PureGuidExpression
toPureExpression =
  fmap runIdentity $ fromExpression f
  where
    f guid = return . Data.PureGuidExpression . Data.GuidExpression guid

expand :: Monad m => Data.PureGuidExpression -> T m Data.PureGuidExpression
expand =
  (`runReaderT` Map.empty) . recurse
  where
    recurse e@(Data.PureGuidExpression (Data.GuidExpression guid val)) =
      case val of
      Data.ExpressionGetVariable (Data.DefinitionRef defI) ->
        -- TODO: expand the result recursively (with some recursive
        -- constraint)
        lift $ DataLoad.loadPureDefinitionBody defI
      Data.ExpressionGetVariable (Data.ParameterRef guidRef) -> do
        mValue <- Reader.asks (Map.lookup guidRef)
        return $ fromMaybe e mValue
      Data.ExpressionApply
        (Data.Apply
         (Data.PureGuidExpression
          (Data.GuidExpression lamGuid
           -- TODO: Don't ignore paramType, we do want to recursively
           -- type-check this:
           (Data.ExpressionLambda (Data.Lambda _paramType body))))
         arg) -> do
          newArg <- recurse arg
          Reader.local (Map.insert lamGuid newArg) $
            recurse body
      Data.ExpressionLambda (Data.Lambda paramType body) ->
        recurseLambda Data.ExpressionLambda paramType body
      Data.ExpressionPi (Data.Lambda paramType body) -> do
        newParamType <- recurse paramType
        recurseLambda Data.ExpressionPi newParamType body
      _ -> return e
      where
        recurseLambda cons paramType =
          liftM
          (Data.PureGuidExpression . Data.GuidExpression guid . cons .
           Data.Lambda paramType) .
          recurse

refFromPure :: Monad m => Data.PureGuidExpression -> Infer (T m) Ref
refFromPure =
  Data.mapMExpression f <=< lift . expand
  where
    f (Data.PureGuidExpression (Data.GuidExpression guid val)) =
      ( return val
      , makeSingletonRef guid
      )

toExpr
  :: Monad m
  => (expr -> (Guid, Data.Expression expr, s))
  -> expr
  -> Infer (T m) (Expression s)
toExpr extract =
  Data.mapMExpression f
  where
    f wrapped =
      ( return expr
      , \newVal -> do
          typeRef <- makeNoConstraints
          valRef <-
            case newVal of
            Data.ExpressionGetVariable (Data.DefinitionRef ref) ->
               refFromPure =<< lift (DataLoad.loadPureDefinitionBody ref)
            _ -> makeSingletonRef g $ fmap eeInferredValue newVal
          return $ Expression prop g typeRef valRef newVal
      )
      where
        (g, expr, prop) = extract wrapped

fromPure :: Monad m => Data.PureGuidExpression -> Infer (T m) (Expression ())
fromPure =
  toExpr extract
  where
    extract (Data.PureGuidExpression (Data.GuidExpression g expr)) =
      ( g, expr, () )

fromLoaded
  :: Monad m
  => DataLoad.ExpressionEntity f
  -> Infer (T m) (StoredExpression f)
fromLoaded =
  toExpr extract
  where
    extract (DataLoad.ExpressionEntity irefProp val) =
      ( eipGuid irefProp, val, irefProp )

derefResumedInfer
  :: (Monad m, RandomGen g)
  => g -> Anchors.BuiltinsMap -> TypeContext
  -> Infer m a -> m (Ref -> [LoopGuidExpression], a)
derefResumedInfer gen builtinsMap typeContext action = do
  (newTypeContext, x) <- resumeInfer typeContext action
  return (derefRef gen builtinsMap newTypeContext, x)

derefRef
  :: RandomGen g
  => g -> Anchors.BuiltinsMap
  -> TypeContext -> Ref -> [LoopGuidExpression]
derefRef stdGen builtinsMap typeContext rootRef =
  (canonizeInferredExpression stdGen .
   map (builtinsToGlobals builtinsMap)) .
  (`runReaderT` []) $ go rootRef
  where
    canonizeInferredExpression = zipWith canonizeIdentifiers . RandomUtils.splits
    go ref = do
      visited <- Reader.ask
      if any (UnionFind.equivalent typeContext ref) visited
        then return $ Loop zeroGuid
        else
        onType ref <=<
        lift . tcExprs $
        UnionFind.descr typeContext ref
    onType ref (Data.GuidExpression guid expr) =
      liftM (NoLoop . Data.GuidExpression guid) .
      Data.sequenceExpression $ fmap recurse expr
      where
        recurse =
          Reader.mapReaderT holify .
          Reader.local (ref :) .
          go
        holify [] =
          [NoLoop
           (Data.GuidExpression zeroGuid Data.ExpressionHole)]
        holify xs = xs

inferExpression
  :: Monad m
  => [(Guid, Ref)]
  -> (Data.DefinitionIRef -> Infer (T m) Ref)
  -> Expression s
  -> Infer (T m) ()
inferExpression scope defRef (Expression _ g typeRef valueRef value) =
  case value of
  Data.ExpressionLambda (Data.Lambda paramType body) -> do
    let paramRef = eeInferredValue paramType
    -- We use "flip unify typeRef" so that the new Pi will be
    -- the official Pi guid due to the "left-bias" in
    -- unify/unifyPair. Thus we can later assume that we got the
    -- same guid in the pi and the lambda.
    unify (eeInferredType paramType) =<< mkSet
    flip unify typeRef <=< makePi g .
      Data.Lambda paramRef $ eeInferredType body
    go [(g, paramRef)] body
  Data.ExpressionPi (Data.Lambda paramType resultType) -> do
    let paramRef = eeInferredValue paramType
    -- TODO: Is Set:Set a good idea? Agda uses "eeInferredType
    -- resultType" as the type
    setType =<< mkSet
    unify (eeInferredType paramType) =<< mkSet
    unify (eeInferredType resultType) =<< mkSet
    go [(g, paramRef)] resultType
  Data.ExpressionApply (Data.Apply func arg) -> do
    go [] func
    go [] arg
    let
      funcValRef = eeInferredValue func
      funcTypeRef = eeInferredType func
      argValRef = eeInferredValue arg
      argTypeRef = eeInferredType arg
    -- We give the new Pi the same Guid as the Apply. This is
    -- fine because Apply's Guid is meaningless and the
    -- canonization will fix it later anyway.
    unify funcTypeRef =<<
      makePi g . Data.Lambda argTypeRef =<< makeNoConstraints
    unify funcTypeRef =<<
      makeRuleRef UnifyRule
        { urArrowType = Pi
        , urDest = typeRef
        , urArg = argValRef
        }
    unify funcValRef =<<
      makeRuleRef UnifyRule
        { urArrowType = Lam
        , urDest = valueRef
        , urArg = argValRef
        }
  Data.ExpressionGetVariable (Data.ParameterRef guid) ->
    case lookup guid scope of
      -- TODO: Not in scope: Bad code,
      -- add an OutOfScopeReference type error
      Nothing -> return ()
      Just paramRef -> setType paramRef
  Data.ExpressionGetVariable (Data.DefinitionRef defI) ->
    setType =<< defRef defI
  Data.ExpressionLiteralInteger _ ->
    setType =<< mkBuiltin ["Prelude"] "Integer"
  Data.ExpressionBuiltin (Data.Builtin _ bType) ->
    setType =<< refFromPure (toPureExpression bType)
  _ -> return ()
  where
    mkSet = mkBuiltin ["Core"] "Set"
    mkBuiltin path name =
      makeSingletonRef zeroGuid . Data.ExpressionBuiltin .
      Data.Builtin (Data.FFIName path name) =<<
      makeNoConstraints
    makePi guid = makeSingletonRef guid . Data.ExpressionPi
    setType = unify typeRef
    go newVars = inferExpression (newVars ++ scope) defRef

-- New Ref tree has tcRules=[] everywhere
dupRefTreeExprs :: Monad m => Ref -> Infer m Ref
dupRefTreeExprs ref = do
  origExprs <- liftM tcExprs $ getRef ref
  exprs <- mapM recurse origExprs
  makeRef emptyConstraints { tcExprs = exprs }
  where
    recurse (Data.GuidExpression guid expr) =
      liftM (Data.GuidExpression guid) .
      Data.sequenceExpression $ fmap dupRefTreeExprs expr

unify
  :: Monad m
  => Ref
  -> Ref
  -> Infer m ()
unify a b = do
  e <- Infer $ UnionFindT.equivalent a b
  unless e $ do
    applyRulesRef a b
    applyRulesRef b a
    unifyConstraints
  where
    applyRulesRef x y = do
      xConstraints <- getRef x
      yConstraints <- getRef y
      sequence_ $
        liftA2 applyRule (tcRules xConstraints) (tcExprs yConstraints)
    applyRule
      (UnifyRule arrowType destRef argRef)
      (Data.GuidExpression guid expr) =
        case (arrowType, expr) of
        (Lam, Data.ExpressionLambda (Data.Lambda _ body)) ->
          subst guid (return argRef) body
        (Pi, Data.ExpressionPi (Data.Lambda _ resultType)) -> do
          newResultType <- dupRefTreeExprs resultType
          subst guid (return argRef) newResultType
          unify destRef newResultType
        _ -> return ()
    unifyConstraints = do
      aConstraints <- getRef a
      bConstraints <- getRef b
      bConflicts <-
        filterM (liftM not . matches (tcExprs aConstraints)) $
        tcExprs bConstraints
      -- Need to re-get a after the side-effecting matches:
      allExprs <- liftM ((++ bConflicts) . tcExprs) $ getRef a
      let allRules = tcRules aConstraints ++ tcRules bConstraints
      Infer $ do
        a `UnionFindT.union` b
        UnionFindT.setDescr a $ Constraints allExprs allRules
    matches as y = liftM or $ mapM (`unifyPair` y) as

-- biased towards left child (if unifying Pis,
-- substs right child's guids to left)
unifyPair
  :: Monad m
  => Data.GuidExpression Ref
  -> Data.GuidExpression Ref
  -> Infer m Bool
unifyPair
  (Data.GuidExpression aGuid aVal)
  (Data.GuidExpression bGuid bVal)
  = case (aVal, bVal) of
    (Data.ExpressionPi l1,
     Data.ExpressionPi l2) ->
      unifyLambdas l1 l2
    (Data.ExpressionLambda l1,
     Data.ExpressionLambda l2) ->
      unifyLambdas l1 l2
    (Data.ExpressionApply (Data.Apply aFuncRef aArgRef),
     Data.ExpressionApply (Data.Apply bFuncRef bArgRef)) -> do
      unify aFuncRef bFuncRef
      unify aArgRef bArgRef
      return True
    (Data.ExpressionBuiltin (Data.Builtin name1 _),
     Data.ExpressionBuiltin (Data.Builtin name2 _)) -> return $ name1 == name2
    (Data.ExpressionGetVariable v1,
     Data.ExpressionGetVariable v2) -> return $ v1 == v2
    (Data.ExpressionLiteralInteger i1,
     Data.ExpressionLiteralInteger i2) -> return $ i1 == i2
    (Data.ExpressionMagic,
     Data.ExpressionMagic) -> return True
    _ -> return False
  where
    unifyLambdas
      (Data.Lambda aParamRef aResultRef)
      (Data.Lambda bParamRef bResultRef) = do
      unify aParamRef bParamRef
      -- Remap b's guid to a's guid and return a as the unification:
      let
        mkGetAGuidRef =
          makeSingletonRef zeroGuid . Data.ExpressionGetVariable $
          Data.ParameterRef aGuid
      subst bGuid mkGetAGuidRef bResultRef
      unify aResultRef bResultRef
      return True

subst :: Monad m => Guid -> Infer m Ref -> Ref -> Infer m ()
subst from mkTo rootRef = do
  refs <- allUnder rootRef
  mapM_ replace refs
  where
    removeFrom
      a@(Data.GuidExpression _
       (Data.ExpressionGetVariable (Data.ParameterRef guidRef)))
      | guidRef == from = (Any True, [])
      | otherwise = (Any False, [a])
    removeFrom x = (Any False, [x])
    replace ref = do
      oldConstraints <- getRef ref
      let
        (Any removed, exprsWOGetVar) =
          mconcat . map removeFrom $ tcExprs oldConstraints
      when removed $ do
        Infer . UnionFindT.setDescr ref $
          oldConstraints { tcExprs = exprsWOGetVar }
        unify ref =<< mkTo

allUnder :: Monad m => Ref -> Infer m [Ref]
allUnder =
  (`execStateT` []) . recurse
  where
    recurse ref = do
      visited <- State.get
      alreadySeen <-
        lift . Infer . liftM or $
        mapM (UnionFindT.equivalent ref) visited
      unless alreadySeen $ do
        State.modify (ref :)
        types <- liftM tcExprs . lift $ getRef ref
        mapM_ onType types
    onType =
      Data.sequenceExpression . fmap recurse . Data.geValue

canonizeIdentifiers
  :: RandomGen g => g -> LoopGuidExpression -> LoopGuidExpression
canonizeIdentifiers gen =
  runIdentity . runRandomT gen . (`runReaderT` Map.empty) . f
  where
    onLambda oldGuid newGuid (Data.Lambda paramType body) =
      liftM2 Data.Lambda (f paramType) .
      Reader.local (Map.insert oldGuid newGuid) $ f body
    f (Loop _oldGuid) =
      lift . liftM Loop $ nextRandom
    f (NoLoop (Data.GuidExpression oldGuid v)) = do
      newGuid <- lift nextRandom
      liftM (NoLoop . Data.GuidExpression newGuid) $
        case v of
        Data.ExpressionLambda lambda ->
          liftM Data.ExpressionLambda $ onLambda oldGuid newGuid lambda
        Data.ExpressionPi lambda ->
          liftM Data.ExpressionPi $ onLambda oldGuid newGuid lambda
        Data.ExpressionApply (Data.Apply func arg) ->
          liftM Data.ExpressionApply $
          liftM2 Data.Apply (f func) (f arg)
        gv@(Data.ExpressionGetVariable (Data.ParameterRef guid)) ->
          Reader.asks $
          maybe gv (Data.ExpressionGetVariable . Data.ParameterRef) .
          Map.lookup guid
        x -> return x

-- TODO: This should not go through LoopGuidExpression and should
-- probably be in Data.Ops or such along with canonize*
alphaEq :: Data.PureGuidExpression -> Data.PureGuidExpression -> Bool
alphaEq =
  on (==) (canonizeIdentifiers (Random.mkStdGen 0) . toLoop)
  where
    toLoop = runIdentity . Data.mapMExpression f
    f (Data.PureGuidExpression (Data.GuidExpression guid expr)) =
      ( Identity expr
      , Identity . NoLoop . Data.GuidExpression guid
      )

builtinsToGlobals :: Anchors.BuiltinsMap -> LoopGuidExpression -> LoopGuidExpression
builtinsToGlobals _ x@(Loop _) = x
builtinsToGlobals builtinsMap (NoLoop (Data.GuidExpression guid expr)) =
  NoLoop . Data.GuidExpression guid $
  fmap (builtinsToGlobals builtinsMap) $
  case expr of
  builtin@(Data.ExpressionBuiltin (Data.Builtin name _)) ->
    (maybe builtin Data.ExpressionGetVariable . Map.lookup name)
    builtinsMap
  other -> other

loadDefTypePure :: Monad m => Data.DefinitionIRef -> T m Data.PureGuidExpression
loadDefTypePure = DataLoad.loadPureExpression . Data.defType <=< Transaction.readIRef

loadDefTypeToRef :: Monad m => Data.DefinitionIRef -> Infer (T m) Ref
loadDefTypeToRef = refFromPure <=< lift . loadDefTypePure

loadDefTypeWithinContext
  :: Monad m
  => Maybe (StoredDefinition (T m)) -> Data.DefinitionIRef -> Infer (T m) Ref
loadDefTypeWithinContext Nothing = loadDefTypeToRef
loadDefTypeWithinContext (Just def) = getDefRef (deInferredType def) (deIRef def)

pureInferExpressionWithinContext
  :: Monad m
  => [(Guid, Ref)]
  -> Maybe (StoredDefinition (T m))
  -> Data.PureGuidExpression -> Infer (T m) (Expression ())
pureInferExpressionWithinContext scope mDef pureExpr = do
  expr <- fromPure pureExpr
  inferExpression scope (loadDefTypeWithinContext mDef) expr
  return expr

getDefRef
  :: Monad m => Ref -> Data.DefinitionIRef -> Data.DefinitionIRef -> Infer (T m) Ref
getDefRef defRef defI getDefI
  | getDefI == defI = return defRef
  | otherwise = loadDefTypeToRef getDefI

inferDefinition
  :: (Monad m, Monad f)
  => DataLoad.DefinitionEntity (T f)
  -> T m (StoredDefinition (T f))
inferDefinition (DataLoad.DefinitionEntity defI (Data.Definition bodyI typeI)) = do
  (typeContext, (bodyExpr, typeExpr)) <- runInfer $ do
    bodyExpr <- fromLoaded bodyI
    typeExpr <- fromLoaded typeI
    inferExpression [] (getDefRef (eeInferredType bodyExpr) defI) bodyExpr
    return (bodyExpr, typeExpr)
  return StoredDefinition
    { deIRef = defI
    , deInferredType = eeInferredType bodyExpr
    , deValue = Data.Definition bodyExpr typeExpr
    , deTypeContext = typeContext
    }

loadInferDefinition
  :: Monad m => Data.DefinitionIRef
  -> T m (StoredDefinition (T m))
loadInferDefinition =
  inferDefinition <=< DataLoad.loadDefinition

loadInferExpression
  :: Monad m
  => Data.ExpressionIRefProperty (T m)
  -> T m (Expression (Data.ExpressionIRefProperty (T m)))
loadInferExpression exprProp = do
  expr <- DataLoad.loadExpression exprProp
  liftM snd . runInfer $ do
    tExpr <- fromLoaded expr
    inferExpression [] loadDefTypeToRef tExpr
    return tExpr
