{-# LANGUAGE TemplateHaskell #-}
module Lamdu.Data.Infer
  ( Infer, Error(..)
  , infer, unify
  , emptyContext
  -- Re-export:
  , Context
  , TypedValue(..), tvVal, tvType
  , ScopedTypedValue(..), stvTV, stvScope
  ) where

import Control.Applicative (Applicative(..), (<*>), (<$>), (<$))
import Control.Lens.Operators
import Control.Monad (when, void)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.State (StateT, state)
import Data.Foldable (traverse_, sequenceA_)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Monoid (Monoid(..))
import Data.Set (Set)
import Data.Store.Guid (Guid)
import Data.Traversable (sequenceA)
import Data.Tuple (swap)
import Data.UnionFind (Ref)
import Lamdu.Data.Infer.Internal
import System.Random (Random, random)
import qualified Control.Lens as Lens
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.UnionFind as UF
import qualified Lamdu.Data.Expression as Expr
import qualified Lamdu.Data.Expression.Lens as ExprLens
import qualified Lamdu.Data.Expression.Utils as ExprUtil
import qualified Lamdu.Data.Infer.ExprRefs as ExprRefs
import qualified System.Random as Random

emptyContext :: Random.StdGen -> Context def
emptyContext gen =
  Context
  { _ctxExprRefs =
    ExprRefs
    { _exprRefsUF = UF.empty
    , _exprRefsData = mempty
    }
  , _ctxDefRefs = Map.empty
  , _ctxRandomGen = gen
  }

rename :: Map Guid Guid -> Guid -> Guid
rename renames guid = fromMaybe guid $ renames ^. Lens.at guid

data Error def
  = VarEscapesScope
  | VarNotInScope
  | InfiniteType Ref
  | Mismatch (Expr.Body def Ref) (Expr.Body def Ref)
  deriving (Show)

type Infer def = StateT (Context def) (Either (Error def))

-- If we don't assert that the scopes have same refs we could be pure
intersectScopes :: Scope -> Scope -> Infer def Scope
intersectScopes (Scope aScope) (Scope bScope) =
  Scope <$> sequenceA (Map.intersectionWith verifyEquiv aScope bScope)
  where
    -- Expensive assertion
    verifyEquiv aref bref = do
      equiv <- ExprRefs.equiv aref bref
      if equiv
        then return aref
        else error "Scope unification of differing refs"

newtype HoleConstraints = HoleConstraints
  { hcUnusableInHoleScope :: Set Guid
  }

-- You must apply this recursively
applyHoleConstraints ::
  HoleConstraints -> RefData def -> Either (Error def) (RefData def)
applyHoleConstraints (HoleConstraints unusableSet) (RefData scope substs mRenameHistory body)
  | Lens.anyOf ExprLens.bodyParameterRef (`Set.member` unusableSet) body =
    Left VarEscapesScope
  -- Expensive assertion
  | Lens.anyOf (Expr._BodyLam . Expr.lamParamId) (`Set.member` unusableSet) body =
    error "applyHoleConstraints: Shadowing detected"
  | otherwise =
    return $ RefData newScope substs mRenameHistory body
  where
    unusableMap = Map.fromSet (const ()) unusableSet
    newScope = scope & scopeMap %~ (`Map.difference` unusableMap)

data UnifyPhase
  = UnifyHoleConstraints HoleConstraints
  | UnifyRef Ref

mergeBodies ::
  Eq def =>
  (Map Guid Guid -> Ref -> UnifyPhase -> Infer def Ref) ->
  Map Guid Guid ->
  Scope -> Expr.Body def Ref ->
  Scope -> Expr.Body def Ref ->
  Infer def (Expr.Body def Ref)
mergeBodies recurse renames xScope xBody yScope yBody =
  case (xBody, yBody) of
  (_, Expr.BodyLeaf Expr.Hole) -> unifyWithHole renames   yScope xScope xBody
  (Expr.BodyLeaf Expr.Hole, _) -> unifyWithHole Map.empty xScope yScope yBody
  _ ->
    case sequenceA <$> ExprUtil.matchBody matchLamResult matchOther (==) xBody yBody of
    Nothing -> lift . Left $ Mismatch xBody yBody
    Just mkBody -> mkBody
  where
    unifyWithHole activeRenames holeScope otherScope nonHoleBody =
      nonHoleBody <$ traverse_
      ( flip (recurse activeRenames)
        (unifyHoleConstraints holeScope otherScope)
      ) nonHoleBody
    unifyHoleConstraints holeScope otherScope =
      UnifyHoleConstraints HoleConstraints
      { hcUnusableInHoleScope =
        Map.keysSet $ Map.difference
        (otherScope ^. scopeMap)
        (holeScope ^. scopeMap)
      }
    matchLamResult xGuid yGuid xRef yRef =
      (yGuid, recurse (renames & Lens.at xGuid .~ Just yGuid) xRef (UnifyRef yRef))
    matchOther xRef yRef = recurse renames xRef (UnifyRef yRef)

renameAppliedPiResult :: Map Guid Guid -> AppliedPiResult -> AppliedPiResult
renameAppliedPiResult renames (AppliedPiResult piGuid argVal destRef copiedNames) =
  AppliedPiResult
  (rename renames piGuid) argVal destRef
  (Map.mapKeys (rename renames) copiedNames)

renameRefData :: Map Guid Guid -> RefData def -> RefData def
renameRefData renames (RefData scope substs renameHistory body)
  -- Expensive assertion
  | Lens.anyOf (Expr._BodyLam . Expr.lamParamId) (`Map.member` renames) body =
    error "Shadowing encountered, what to do?"
  | otherwise =
    RefData
    (scope & scopeMap %~ Map.mapKeys (rename renames))
    (substs <&> renameAppliedPiResult renames)
    (renameHistory & _RenameHistory %~ Map.union renames)
    (body & ExprLens.bodyParameterRef %~ rename renames)

mergeRefData ::
  Eq def =>
  (Map Guid Guid -> Ref -> UnifyPhase -> Infer def Ref) ->
  Map Guid Guid -> RefData def -> RefData def -> Infer def ([AppliedPiResult], RefData def)
mergeRefData recurse renames
  (RefData aScope aAppliedPiResults aMRenameHistory aBody)
  (RefData bScope bAppliedPiResults bMRenameHistory bBody) =
  mkRefData
  <$> intersectScopes aScope bScope
  <*> mergeBodies recurse renames aScope aBody bScope bBody
  where
    newInfoOnSelf =
      Lens.has ExprLens.bodyHole aBody /=
      Lens.has ExprLens.bodyHole bBody
    substsToExecute
      | newInfoOnSelf = mergedAppliedPiResults
      | otherwise = []
    mergedAppliedPiResults = aAppliedPiResults ++ bAppliedPiResults
    mkRefData intersectedScope mergedBody =
      (,) substsToExecute $
      RefData
      { _rdScope = intersectedScope
      , _rdAppliedPiResults = mergedAppliedPiResults
      , _rdRenameHistory = mappend aMRenameHistory bMRenameHistory
      , _rdBody = mergedBody
      }

renameMergeRefData ::
  Eq def =>
  (Map Guid Guid -> Ref -> UnifyPhase -> Infer def Ref) ->
  Map Guid Guid -> RefData def -> RefData def -> Infer def ([AppliedPiResult], RefData def)
renameMergeRefData recurse renames a b =
  mergeRefData recurse renames (renameRefData renames a) b

unifyRecurse :: Eq def => Set Ref -> Map Guid Guid -> Ref -> UnifyPhase -> Infer def Ref
unifyRecurse visited renames rawNode phase = do
  nodeRep <- ExprRefs.find "unifyRecurse:rawNode" rawNode
  if visited ^. Lens.contains nodeRep
    then lift . Left $ InfiniteType nodeRep
    else
    case phase of
    UnifyHoleConstraints holeConstraints -> do
      rawNodeData <- ExprRefs.readRep nodeRep
      nodeData <-
        lift . applyHoleConstraints holeConstraints $
        renameRefData renames rawNodeData
      ExprRefs.writeRep nodeRep nodeData
      traverse_
        (flip (recurse nodeRep renames)
         (UnifyHoleConstraints holeConstraints)) $
        nodeData ^. rdBody
      return nodeRep
    UnifyRef other ->
      ExprRefs.unifyRefs merge nodeRep other
      -- This bind will now execute the substOrUnify's action returned
      -- from merge, which is now safe because unifyRefs completed.
      >>= fromMaybe (return nodeRep)
  where
    recurse visitedRef = unifyRecurse (visited & Lens.contains visitedRef .~ True)
    merge rep a b =
      renameMergeRefData (recurse rep) renames a b
      <&> swap
      -- Schedule the substOrUnify's to happen after the merge has been
      -- written to the rep (return the substOrUnify's as an *action* that's
      -- not yet executed):
      <&> Lens._2 %~ (rep <$) . traverse_ (substOrUnify rep)

unify :: Eq def => Ref -> Ref -> Infer def Ref
unify x y = unifyRecurse mempty mempty x (UnifyRef y)

infer ::
  Eq def => Scope -> Expr.Expression (LoadedDef def) a ->
  Infer def (Expr.Expression (LoadedDef def) (ScopedTypedValue, a))
infer = exprIntoSTV

scopeLookup :: Scope -> Guid -> Either (Error def) Ref
scopeLookup scope guid =
  case scope ^. scopeMap . Lens.at guid of
  Nothing -> Left VarNotInScope
  Just ref -> pure ref

makePiTypeOfLam ::
  Guid -> TypedValue -> TypedValue ->
  Expr.Body def Ref
makePiTypeOfLam paramGuid paramType body =
  Expr.BodyLam $
  Expr.Lam Expr.KType paramGuid
  (paramType ^. tvVal)
  -- We rely on the scope of the Lam KVal body being equal to the
  -- scope of the Lam KType body, because we use the same
  -- paramGuid. This means param guids cannot be unique.
  (body ^. tvType)

fresh :: Scope -> Expr.Body def Ref -> Infer def Ref
fresh scope body = ExprRefs.fresh RefData
  { _rdScope = scope
  , _rdAppliedPiResults = []
  , _rdRenameHistory = Untracked
  , _rdBody = body
  }

newRandom :: Random r => Infer def r
newRandom = Lens.zoom ctxRandomGen $ state random

forcePiType :: Eq def => Scope -> Ref -> Ref -> Infer def (Guid, Ref)
forcePiType piScope paramTypeRef destRef = do
  newGuid <- newRandom
  let
    piResultScope = piScope & scopeMap %~ Map.insert newGuid paramTypeRef
  newResultType <- fresh piResultScope $ ExprLens.bodyHole # ()
  newPiType <-
    fresh piScope . Expr.BodyLam $
    Expr.Lam Expr.KType newGuid paramTypeRef newResultType
  -- left is renamed into right (keep existing names of destRef):
  rep <- unify newPiType destRef
  body <- (^. rdBody) <$> ExprRefs.readRep rep
  return $
    case body ^? ExprLens.bodyKindedLam Expr.KType of
    Just (paramGuid, _piParamTypeRef, piResultRef) -> (paramGuid, piResultRef)
    Nothing -> error "We just unified Lam KType into rep"

-- Remap a Guid from piResult context to the Apply context
remapSubstGuid :: AppliedPiResult -> RefData def -> Guid -> Maybe Guid
remapSubstGuid subst applyTypeData src =
  case subst ^? aprCopiedNames . Lens.ix src of
  -- If it's not a copied guid, it should be the same guid in both
  -- contexts
  Nothing -> Nothing
  Just copiedAs ->
    Just . fromMaybe copiedAs $
    applyTypeData ^? rdRenameHistory . _RenameHistory . Lens.ix copiedAs

copySubstGetPar ::
  Eq def => Guid -> Ref -> Ref -> RefData defa -> AppliedPiResult -> Infer def ()
copySubstGetPar paramGuid piResultRef applyTypeRef applyTypeData subst =
  void $ unify applyTypeRef =<< ref
  where
    ref
      | paramGuid == subst ^. aprPiGuid = return $ subst ^. aprArgVal
      | otherwise =
        case remapSubstGuid subst applyTypeData paramGuid of
        Nothing -> return piResultRef
        Just destGuid ->
          fresh (applyTypeData ^. rdScope) $ ExprLens.bodyParameterRef # destGuid

freshSubstDestHole :: Scope -> Infer def Ref
freshSubstDestHole scope =
  ExprRefs.fresh RefData
  { _rdScope = scope
  , _rdAppliedPiResults = []
  , _rdRenameHistory = RenameHistory Map.empty
  , _rdBody = ExprLens.bodyHole # ()
  }

substParent :: Eq def => Scope -> Ref -> AppliedPiResult -> Expr.Body def Ref -> Infer def ()
substParent destScope destRef subst srcBody = do
  destBodyRef <-
    srcBody & Lens.traverse %%~ const (freshSubstDestHole destScope)
    >>= fresh destScope
  void $ unify destBodyRef destRef -- destBodyRef is swallowed by destRef if it had anything...
  destBody <- (^. rdBody) <$> ExprRefs.read destRef
  matchRes <-
    sequenceA $ sequenceA_ <$>
    ExprUtil.matchBody matchLamResult matchOther
    (error "Parent cannot be getPar") srcBody destBody
  when (Lens.has Lens._Nothing matchRes) $
    error "We just successfully unified src and dest bodies!"
  where
    matchLamResult srcGuid destGuid srcChildRef destChildRef =
      subst
      & aprCopiedNames %~ Map.insert srcGuid destGuid
      & recurse srcChildRef destChildRef
      & (,) srcGuid
    matchOther srcChildRef destChildRef = recurse srcChildRef destChildRef subst
    recurse srcChildRef destChildRef newAppliedPiResult =
      newAppliedPiResult
      & aprDestRef .~ destChildRef
      & substOrUnify srcChildRef

substOrUnify :: Eq def => Ref -> AppliedPiResult -> Infer def ()
substOrUnify piResultRef subst = do
  piResultData <- ExprRefs.read piResultRef
  applyTypeData <- ExprRefs.read applyTypeRef
  let
    piResultScope = piResultData ^. rdScope
    applyTypeScope = applyTypeData ^. rdScope
    -- Only if the piResultScope has any variable available that's not
    -- already available in the applyTypeScope could it be a GetVar.
    notSubst =
      Map.null $
      Map.difference (piResultScope ^. scopeMap) (applyTypeScope ^. scopeMap)
    onParent = substParent applyTypeScope applyTypeRef subst
  if notSubst
    then -- It can't be a GetVar, we can merge and pull information
         -- from the apply:
    void $ unify applyTypeRef piResultRef
    else -- Do the subst:
    case piResultData ^. rdBody of
    Expr.BodyLeaf (Expr.GetVariable (Expr.ParameterRef paramGuid)) ->
      copySubstGetPar paramGuid piResultRef applyTypeRef applyTypeData subst
    Expr.BodyLeaf Expr.Hole ->
      piResultData & rdAppliedPiResults %~ (subst :) & ExprRefs.write piResultRef
    Expr.BodyLeaf {} -> void $ unify applyTypeRef piResultRef
    Expr.BodyLam lam ->
      lam
      & Expr.lamParamId %%~ const newRandom
      <&> Expr.BodyLam
      >>= onParent
    body -> onParent body
  where
    applyTypeRef = subst ^. aprDestRef

makeApplyType ::
  Eq def => Scope -> ScopedTypedValue -> ScopedTypedValue ->
  Infer def Ref
makeApplyType applyScope func arg = do
  (piGuid, piResultRef) <-
    forcePiType
    (func ^. stvScope)
    (arg ^. stvTV . tvType)
    (func ^. stvTV . tvType)
  applyTypeRef <- fresh applyScope $ ExprLens.bodyHole # ()
  substOrUnify piResultRef AppliedPiResult
    { _aprPiGuid = piGuid
    , _aprArgVal = arg ^. stvTV . tvVal
    , _aprDestRef = applyTypeRef
    , _aprCopiedNames = mempty
    }
  return applyTypeRef

makeGetFieldType :: Eq def => Scope -> Expr.GetField TypedValue -> Infer def Ref
makeGetFieldType scope (Expr.GetField _record tag) = do
  tagTypeRef <- fresh scope $ ExprLens.bodyTagType # ()
  void . unify tagTypeRef $ tag ^. tvType
  fresh scope $ ExprLens.bodyHole # () -- TODO

makeLambdaType :: Eq def => Scope -> Guid -> TypedValue -> TypedValue -> Infer def Ref
makeLambdaType scope paramGuid paramType result = do
  typeRef <- fresh scope $ ExprLens.bodyType # ()
  void . unify typeRef $ paramType ^. tvType
  fresh scope $ makePiTypeOfLam paramGuid paramType result

makeRecordType :: Eq def => Scope -> [(TypedValue, TypedValue)] -> Infer def Ref
makeRecordType scope fields = do
  tagTypeRef <- fresh scope $ ExprLens.bodyTagType # ()
  fields & traverse_ . Lens._1 . tvType %%~ unify tagTypeRef
  fresh scope $
    Expr.BodyRecord . Expr.Record Expr.KType $ onField <$> fields
  where
    onField (tag, val) = (tag ^. tvVal, val ^. tvType)

makePiType :: Eq def => Scope -> TypedValue -> TypedValue -> Infer def Ref
makePiType scope paramType resultType = do
  typeRef <- fresh scope $ ExprLens.bodyType # ()
  void . unify typeRef $ paramType ^. tvType
  void . unify typeRef $ resultType ^. tvType
  return typeRef

makeTypeRef ::
  Eq def => Scope ->
  Expr.Body (LoadedDef def) ScopedTypedValue ->
  Infer def Ref
makeTypeRef scope body =
  case body of
  -- Simple types
  Expr.BodyLeaf Expr.Type -> typeIsType
  Expr.BodyLeaf Expr.IntegerType -> typeIsType
  Expr.BodyLeaf Expr.TagType -> typeIsType
  Expr.BodyLam (Expr.Lam Expr.KType _ paramType resultType) ->
    makePiType scope (paramType ^. stvTV) (resultType ^. stvTV)
  Expr.BodyRecord (Expr.Record Expr.KType _) -> typeIsType
  Expr.BodyLeaf Expr.LiteralInteger {} -> fresh scope $ ExprLens.bodyIntegerType # ()
  Expr.BodyLeaf Expr.Tag {} -> fresh scope $ ExprLens.bodyTagType # ()
  Expr.BodyLeaf Expr.Hole -> fresh scope $ ExprLens.bodyHole # ()
  -- GetPars
  Expr.BodyLeaf (Expr.GetVariable (Expr.DefinitionRef (LoadedDef _ ref))) -> pure ref
  Expr.BodyLeaf (Expr.GetVariable (Expr.ParameterRef guid)) -> lift $ scopeLookup scope guid
  -- Complex:
  Expr.BodyGetField getField -> makeGetFieldType scope ((^. stvTV) <$> getField)
  Expr.BodyApply (Expr.Apply func arg) -> makeApplyType scope func arg
  Expr.BodyLam (Expr.Lam Expr.KVal paramGuid paramType result) ->
    makeLambdaType scope paramGuid (paramType ^. stvTV) (result ^. stvTV)
  Expr.BodyRecord (Expr.Record Expr.KVal fields) ->
    makeRecordType scope $ fields <&> Lens.both %~ (^. stvTV)
  where
    typeIsType = fresh scope $ ExprLens.bodyType # ()

-- With hole apply vals and hole types
exprIntoSTV ::
  Eq def => Scope -> Expr.Expression (LoadedDef def) a ->
  Infer def (Expr.Expression (LoadedDef def) (ScopedTypedValue, a))
exprIntoSTV scope (Expr.Expression body pl) = do
  newBody <-
    case body of
    Expr.BodyLam (Expr.Lam k paramGuid paramType result) -> do
      paramTypeS <- exprIntoSTV scope paramType
      let
        newScope =
          scope
          & scopeMap . Lens.at paramGuid .~
            Just (paramTypeS ^. Expr.ePayload . Lens._1 . stvTV . tvVal)
      resultS <- exprIntoSTV newScope result
      pure . Expr.BodyLam $ Expr.Lam k paramGuid paramTypeS resultS
    _ ->
      body & Lens.traverse %%~ exprIntoSTV scope
  valRef <-
    newBody
    & circumcizeApply
    <&> (^. Expr.ePayload . Lens._1 . stvTV . tvVal)
    & ExprLens.bodyDef %~ (^. ldDef)
    & mkRefData
    & ExprRefs.fresh
  typeRef <-
    newBody <&> (^. Expr.ePayload . Lens._1) & makeTypeRef scope
  pure $
    Expr.Expression newBody
    ((ScopedTypedValue (TypedValue valRef typeRef) scope), pl)
  where
    mkRefData newBody = RefData
      { _rdScope = scope
      , _rdAppliedPiResults = []
      , _rdRenameHistory = Untracked
      , _rdBody = newBody
      }
    -- Except of apply of type constructors:
    circumcizeApply x
      | Lens.nullOf Expr._BodyApply x = x
      | Lens.has (Expr._BodyApply . Expr.applyFunc . ExprLens.exprDefinitionRef) x = x
      | otherwise = ExprLens.bodyHole # ()
