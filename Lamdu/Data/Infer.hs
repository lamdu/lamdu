{-# LANGUAGE TemplateHaskell #-}
module Lamdu.Data.Infer
  ( Infer, Error(..)
  , infer, unify
  , emptyContext
  -- Re-export:
  , Context
  , Scope, emptyScope
  , TypedValue(..), tvVal, tvType
  , ScopedTypedValue(..), stvTV, stvScope
  ) where

import Control.Applicative (Applicative(..), (<*>), (<$>))
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
  { _hcUnusableInHoleScope :: Set Guid
  }

-- You must apply this recursively
applyHoleConstraints ::
  HoleConstraints -> RefData def -> Either (Error def) (RefData def)
applyHoleConstraints (HoleConstraints unusableSet) refData
  | Lens.anyOf ExprLens.bodyParameterRef (`Set.member` unusableSet) body =
    Left VarEscapesScope
  -- Expensive assertion
  | Lens.anyOf (Expr._BodyLam . Expr.lamParamId) (`Set.member` unusableSet) body =
    error "applyHoleConstraints: Shadowing detected"
  | otherwise =
    return $ refData & rdScope . scopeMap %~ (`Map.difference` unusableMap)
  where
    body = refData ^. rdBody
    unusableMap = Map.fromSet (const ()) unusableSet

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
      maybeRecurseHoleConstraints activeRenames nonHoleBody $
      makeUnusableScopeSet holeScope otherScope
    maybeRecurseHoleConstraints activeRenames nonHoleBody unusableScopeSet
      | Set.null unusableScopeSet && Map.null renames =
        return nonHoleBody
      | otherwise =
        nonHoleBody
        & Lens.traverse %%~
          flip (recurse activeRenames)
          (UnifyHoleConstraints (HoleConstraints unusableScopeSet))
    makeUnusableScopeSet holeScope otherScope =
      Map.keysSet $ Map.difference
      (otherScope ^. scopeMap)
      (holeScope ^. scopeMap)
    matchLamResult xGuid yGuid xRef yRef =
      (yGuid, recurse (renames & Lens.at xGuid .~ Just yGuid) xRef (UnifyRef yRef))
    matchOther xRef yRef = recurse renames xRef (UnifyRef yRef)

renameAppliedPiResult :: Map Guid Guid -> AppliedPiResult -> AppliedPiResult
renameAppliedPiResult renames (AppliedPiResult piGuid argVal destRef copiedNames copiedRefs) =
  AppliedPiResult
  (rename renames piGuid) argVal destRef
  (Map.mapKeys (rename renames) copiedNames)
  copiedRefs

-- No names in Relation (yet?)
renameRelations :: Map Guid Guid -> Set Relation -> Set Relation
renameRelations _ = id

renameScope :: (Guid -> Guid) -> Scope -> Scope
renameScope f = scopeMap %~ Map.mapKeys f

renameRefData :: Map Guid Guid -> RefData def -> RefData def
renameRefData renames (RefData scope substs renameHistory relations body)
  -- Expensive assertion
  | Lens.anyOf (Expr._BodyLam . Expr.lamParamId) (`Map.member` renames) body =
    error "Shadowing encountered, what to do?"
  | otherwise =
    RefData
    (scope & renameScope (rename renames))
    (substs <&> renameAppliedPiResult renames)
    (renameHistory & _RenameHistory %~ Map.union renames)
    (relations & renameRelations renames)
    (body & ExprLens.bodyParameterRef %~ rename renames)

mergeRefData ::
  Eq def =>
  (Map Guid Guid -> Ref -> UnifyPhase -> Infer def Ref) ->
  Map Guid Guid -> RefData def -> RefData def -> Infer def ([AppliedPiResult], RefData def)
mergeRefData recurse renames
  (RefData aScope aAppliedPiResults aMRenameHistory aRelations aBody)
  (RefData bScope bAppliedPiResults bMRenameHistory bRelations bBody) =
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
      , _rdRelations = mappend aRelations bRelations
      , _rdBody = mergedBody
      }

renameMergeRefData ::
  Eq def =>
  (Map Guid Guid -> Ref -> UnifyPhase -> Infer def Ref) ->
  Ref -> Map Guid Guid -> RefData def -> RefData def ->
  Infer def ()
renameMergeRefData recurse rep renames a b = do
  (appliedPiResults, mergedRefData) <-
    mergeRefData recurse renames (renameRefData renames a) b
  -- First let's write the mergedRefData so we're not in danger zone
  -- of reading missing data:
  ExprRefs.write rep mergedRefData
  -- Now we can safely run the relations
  traverse_ (substOrUnify rep) appliedPiResults


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
    UnifyRef other -> do
      (rep, unifyResult) <- ExprRefs.unifyRefs nodeRep other
      case unifyResult of
        ExprRefs.UnifyRefsAlreadyUnified -> return ()
        ExprRefs.UnifyRefsUnified xData yData -> merge rep xData yData
      return rep
  where
    recurse visitedRef = unifyRecurse (visited & Lens.contains visitedRef .~ True)
    merge rep = renameMergeRefData (recurse rep) rep renames

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
fresh scope body = ExprRefs.fresh $ defaultRefData scope body

newRandom :: Random r => Infer def r
newRandom = Lens.zoom ctxRandomGen $ state random

forceLam :: Eq def => Expr.Kind -> Ref -> Scope -> Infer def (Guid, Ref, Ref)
forceLam k destRef lamScope = do
  newGuid <- newRandom
  newParamTypeRef <- fresh lamScope $ ExprLens.bodyHole # ()
  let lamResultScope = lamScope & scopeMap %~ Map.insert newGuid newParamTypeRef
  newResultTypeRef <- fresh lamResultScope $ ExprLens.bodyHole # ()
  newLamRef <-
    fresh lamScope . Expr.BodyLam $
    Expr.Lam k newGuid newParamTypeRef newResultTypeRef
  -- left is renamed into right (keep existing names of destRef):
  rep <- unify newLamRef destRef
  body <- (^. rdBody) <$> ExprRefs.readRep rep
  return $
    case body ^? ExprLens.bodyKindedLam k of
    Just kindedLam -> kindedLam
    Nothing -> error "We just unified Lam into rep"

-- Remap a Guid from piResult context to the Apply context
remapSubstGuid :: AppliedPiResult -> RenameHistory -> Guid -> Guid
remapSubstGuid subst destRenameHistory src =
  case subst ^? aprCopiedNames . Lens.ix src of
  -- If it's not a copied guid, it should be the same guid in both
  -- contexts
  Nothing -> src
  Just copiedAs ->
    fromMaybe copiedAs $
    destRenameHistory ^? _RenameHistory . Lens.ix copiedAs

injectRenameHistory :: RenameHistory -> Map Guid Guid -> Map Guid Guid
injectRenameHistory Untracked = id
injectRenameHistory (RenameHistory renames) =
  -- Only the copied names (in our argument map) need to be fixed,
  -- others are in shared scope so need no fixing:
  fmap (rename renames)

-- TODO: This should also substLeafs, and it should also subst getvars that aren't subst
substNode :: Eq def => Ref -> Expr.Body def Ref -> AppliedPiResult -> Infer def ()
substNode srcRef srcBody apr = do
  destData <- ExprRefs.read destRef
  let
    newApr =
      apr
      & aprCopiedNames %~ injectRenameHistory (destData ^. rdRenameHistory)
      & aprCopiedRefs . Lens.at srcRef .~ Just destRef
    matchLamResult srcGuid destGuid srcChildRef destChildRef =
      newApr
      & aprCopiedNames %~ Map.insert srcGuid destGuid
      & recurse srcChildRef destChildRef
      & (,) srcGuid
    matchOther srcChildRef destChildRef =
      recurse srcChildRef destChildRef newApr
    -- Expensive assertion
    verifyGetParGuids srcGuid destGuid =
      rename (newApr ^. aprCopiedNames) srcGuid == destGuid
  matchRes <-
    sequenceA $ sequenceA_ <$>
    ExprUtil.matchBody matchLamResult matchOther
    verifyGetParGuids srcBody (destData ^. rdBody)
  when (Lens.has Lens._Nothing matchRes) $
    error "We just successfully unified src and dest bodies!"
  where
    destRef = apr ^. aprDestRef
    recurse srcChildRef destChildRef childApr =
      substOrUnify srcChildRef $
      childApr & aprDestRef .~ destChildRef

substOrUnify :: Eq def => Ref -> AppliedPiResult -> Infer def ()
substOrUnify srcRef apr = do
  srcData <- ExprRefs.read srcRef
  destData <- ExprRefs.read destRef
  let
    srcScope = srcData ^. rdScope
    destScope = destData ^. rdScope
    -- TODO: this duplicates substNode logic about injecting the
    -- rename history into the subst, which should probably happen
    -- here instead of both places but we're not sure this is correct
    -- because there's a unify into the apply side between here and
    -- there.
    remapGuid = remapSubstGuid apr (destData ^. rdRenameHistory)
    renameRef ref = fromMaybe ref $ (apr ^. aprCopiedRefs) ^? Lens.ix ref
    renamedSrcScope =
      renameScope remapGuid srcScope
      & scopeMap . Lens.mapped %~ renameRef
    -- Only if the srcScope has any variable available that's not
    -- already available in the destScope could it be a GetVar.
    isUnify =
      Map.null $
      Map.difference (srcScope ^. scopeMap) (destScope ^. scopeMap)
  if isUnify
    then -- It can't be a GetVar, we can merge and pull information
         -- from the apply:
    void $ unify destRef srcRef
    else -- Do the subst:
    case srcData ^. rdBody of
    Expr.BodyLeaf (Expr.GetVariable (Expr.ParameterRef paramGuid))
      | paramGuid == apr ^. aprPiGuid -> void . unify destRef $ apr ^. aprArgVal
    Expr.BodyLeaf Expr.Hole -> do
      srcData & rdAppliedPiResults %~ (apr :) & ExprRefs.write srcRef
      -- We injectRenameHistory of our ancestors into the apr
      destData & rdRenameHistory <>~ RenameHistory mempty & ExprRefs.write destRef
    srcBody@(Expr.BodyLam (Expr.Lam k _ _ _)) -> do
      void $ forceLam k destRef renamedSrcScope
      substNode srcRef srcBody apr
    srcBody -> do
      destBodyRef <-
        srcBody
        & Lens.traverse %%~ const (fresh renamedSrcScope $ ExprLens.bodyHole # ())
        <&> ExprLens.bodyParameterRef %~ remapGuid
        >>= fresh renamedSrcScope
      void $ unify destBodyRef destRef -- destBodyRef is swallowed by destRef if it had anything...
      substNode srcRef srcBody apr
  where
    destRef = apr ^. aprDestRef

makeApplyType ::
  Eq def => Scope -> ScopedTypedValue -> ScopedTypedValue ->
  Infer def Ref
makeApplyType applyScope func arg = do
  (piGuid, piParamType, piResultRef) <-
    forceLam Expr.KType
    (func ^. stvTV . tvType)
    (func ^. stvScope)
  void $ unify (arg ^. stvTV . tvType) piParamType
  applyTypeRef <- fresh applyScope $ ExprLens.bodyHole # ()
  substOrUnify piResultRef AppliedPiResult
    { _aprPiGuid = piGuid
    , _aprArgVal = arg ^. stvTV . tvVal
    , _aprDestRef = applyTypeRef
    , _aprCopiedNames = mempty
    , _aprCopiedRefs = mempty
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
  fields & Lens.traverseOf_ (Lens.traverse . Lens._1 . tvType) (unify tagTypeRef)
  fields & Lens.traverseOf_ (Lens.traverse . Lens._1 . tvVal) setTagPos
  fresh scope $
    Expr.BodyRecord . Expr.Record Expr.KType $ onField <$> fields
  where
    setTagPos ref =
      ExprRefs.modify ref $ rdRelations <>~ Set.singleton RelationIsTag
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
    & defaultRefData scope
    & ExprRefs.fresh
  typeRef <-
    newBody <&> (^. Expr.ePayload . Lens._1) & makeTypeRef scope
  pure $
    Expr.Expression newBody
    ((ScopedTypedValue (TypedValue valRef typeRef) scope), pl)
  where
    -- Except of apply of type constructors:
    circumcizeApply x
      | Lens.nullOf Expr._BodyApply x = x
      | Lens.has (Expr._BodyApply . Expr.applyFunc . ExprLens.exprDefinitionRef) x = x
      | otherwise = ExprLens.bodyHole # ()
