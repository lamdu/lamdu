{-# LANGUAGE PatternGuards #-}
module Lamdu.Data.Infer.Unify
  ( unify, fresh
  , forceLam
  ) where

import Control.Applicative ((<$>))
import Control.Lens.Operators
import Control.Monad.Trans.State (state)
import Data.Foldable (traverse_)
import Data.Map (Map)
import Data.Map.Utils (lookupOrSelf)
import Data.Maybe (fromMaybe)
import Data.Maybe.Utils (unsafeUnjust)
import Data.Monoid (Monoid(..))
import Data.Set (Set)
import Data.Store.Guid (Guid)
import Data.Traversable (sequenceA)
import Data.UnionFind (Ref)
import Lamdu.Data.Infer.Internal
import Lamdu.Data.Infer.Monad (Infer, Error(..))
import System.Random (Random, random)
import qualified Control.Lens as Lens
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Lamdu.Data.Expression as Expr
import qualified Lamdu.Data.Expression.Lens as ExprLens
import qualified Lamdu.Data.Expression.Utils as ExprUtil
import qualified Lamdu.Data.Infer.ExprRefs as ExprRefs
import qualified Lamdu.Data.Infer.Monad as InferM

fresh :: Scope -> Expr.Body def Ref -> Infer def Ref
fresh scope body = ExprRefs.fresh $ defaultRefData scope body

newRandom :: Random r => Infer def r
newRandom = Lens.zoom ctxRandomGen $ state random

forceLam :: Eq def => Expr.Kind -> Scope -> Ref -> Infer def (Guid, Ref, Ref)
forceLam k lamScope destRef = do
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
  return . unsafeUnjust "We just unified Lam into rep" $
    body ^? ExprLens.bodyKindedLam k

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
checkHoleConstraints ::
  HoleConstraints -> Expr.Body def Ref -> Scope ->
  Either (Error def) Scope
checkHoleConstraints (HoleConstraints unusableSet) body scope
  | Just paramGuid <- body ^? ExprLens.bodyParameterRef
  , paramGuid `Set.member` unusableSet
  = Left $ VarEscapesScope paramGuid
  -- Expensive assertion
  | Lens.anyOf (Expr._BodyLam . Expr.lamParamId) (`Set.member` unusableSet) body =
    error "checkHoleConstraints: Shadowing detected"
  | otherwise =
    return $ scope & scopeMap %~ (`Map.difference` unusableMap)
  where
    unusableMap = Map.fromSet (const ()) unusableSet

data UnifyPhase
  = UnifyHoleConstraints HoleConstraints
  | UnifyRef Ref

mergeScopeBodies ::
  Eq def =>
  (Map Guid Guid -> Ref -> UnifyPhase -> Infer def Ref) ->
  Map Guid Guid ->
  Scope -> Expr.Body def Ref ->
  Scope -> Expr.Body def Ref ->
  Infer def (Scope, Expr.Body def Ref)
mergeScopeBodies recurse renames xScope xBody yScope yBody = do
  intersectedScope <- intersectScopes xScope yScope
  let
    unifyWithHole activeRenames holeScope otherScope nonHoleBody =
      applyHoleConstraints (recurse activeRenames) (HoleConstraints unusableScopeSet)
      nonHoleBody intersectedScope
      <&> flip (,) nonHoleBody
      where
        unusableScopeSet = makeUnusableScopeSet holeScope otherScope
  case (xBody, yBody) of
    (_, Expr.BodyLeaf Expr.Hole) -> unifyWithHole renames   yScope xScope xBody
    (Expr.BodyLeaf Expr.Hole, _) -> unifyWithHole Map.empty xScope yScope yBody
    _ ->
      fmap ((,) intersectedScope) .
      fromMaybe (InferM.error (Mismatch xBody yBody)) $
      sequenceA <$> ExprUtil.matchBody matchLamResult matchOther (==) xBody yBody
  where
    makeUnusableScopeSet holeScope otherScope =
      Map.keysSet $ Map.difference
      (otherScope ^. scopeMap)
      (holeScope ^. scopeMap)
    matchLamResult xGuid yGuid xRef yRef =
      (yGuid, recurse (renames & Lens.at xGuid .~ Just yGuid) xRef (UnifyRef yRef))
    matchOther xRef yRef = recurse renames xRef (UnifyRef yRef)

renameAppliedPiResult :: Map Guid Guid -> AppliedPiResult -> AppliedPiResult
renameAppliedPiResult renames (AppliedPiResult piGuid argVal destRef copiedNames) =
  AppliedPiResult
  (lookupOrSelf renames piGuid) argVal destRef
  (Map.mapKeys (lookupOrSelf renames) copiedNames)

-- No names in Relation (yet?)
renameRelation :: Map Guid Guid -> Relation -> Relation
renameRelation renames (RelationAppliedPiResult apr) =
  RelationAppliedPiResult $ renameAppliedPiResult renames apr
renameRelation _ x = x

renameRefData :: Map Guid Guid -> RefData def -> RefData def
renameRefData renames (RefData scope renameHistory relations body)
  -- Expensive assertion
  | Lens.anyOf (Expr._BodyLam . Expr.lamParamId) (`Map.member` renames) body =
    error "Shadowing encountered, what to do?"
  | otherwise =
    RefData
    (scope & scopeMap %~ Map.mapKeys (lookupOrSelf renames))
    (renameHistory & _RenameHistory %~ Map.union renames)
    (relations & map (renameRelation renames))
    (body & ExprLens.bodyParameterRef %~ lookupOrSelf renames)

mergeRefData ::
  Eq def =>
  (Map Guid Guid -> Ref -> UnifyPhase -> Infer def Ref) ->
  Map Guid Guid -> RefData def -> RefData def -> Infer def ([Relation], RefData def)
mergeRefData recurse renames
  (RefData aScope aMRenameHistory aRelations aBody)
  (RefData bScope bMRenameHistory bRelations bBody) =
  mkRefData
  <$> mergeScopeBodies recurse renames aScope aBody bScope bBody
  where
    newInfoOnSelf =
      Lens.has ExprLens.bodyHole aBody /=
      Lens.has ExprLens.bodyHole bBody
    relationsTriggered
      | newInfoOnSelf = mergedRelations
      | otherwise = []
    mergedRelations = aRelations ++ bRelations
    mkRefData (intersectedScope, mergedBody) =
      (,) relationsTriggered
      RefData
      { _rdScope = intersectedScope
      , _rdRenameHistory = mappend aMRenameHistory bMRenameHistory
      , _rdRelations = mergedRelations
      , _rdBody = mergedBody
      }

renameMergeRefData ::
  Eq def =>
  (Map Guid Guid -> Ref -> UnifyPhase -> Infer def Ref) ->
  Ref -> Map Guid Guid -> RefData def -> RefData def ->
  Infer def ()
renameMergeRefData recurse rep renames a b = do
  (relations, mergedRefData) <-
    mergeRefData recurse renames (renameRefData renames a) b
  -- First let's write the mergedRefData so we're not in danger zone
  -- of reading missing data:
  ExprRefs.write rep mergedRefData
  -- Now we can safely run the relations
  traverse_ (InferM.executeRelation rep) relations

applyHoleConstraints ::
  (Ref -> UnifyPhase -> Infer def dummy) -> HoleConstraints ->
  Expr.Body def Ref -> Scope -> Infer def Scope
applyHoleConstraints recurse holeConstraints body oldScope = do
  newScope <-
    InferM.liftError $
    checkHoleConstraints holeConstraints body oldScope
  traverse_ (flip recurse (UnifyHoleConstraints holeConstraints)) body
  return newScope

unifyRecurse :: Eq def => Set Ref -> Map Guid Guid -> Ref -> UnifyPhase -> Infer def Ref
unifyRecurse visited renames rawNode phase = do
  nodeRep <- ExprRefs.find "unifyRecurse:rawNode" rawNode
  if visited ^. Lens.contains nodeRep
    then InferM.error $ InfiniteExpression nodeRep
    else
    case phase of
    UnifyHoleConstraints holeConstraints -> do
      oldNodeData <- ExprRefs.readRep nodeRep
      ExprRefs.writeRep nodeRep $ error "Reading node during write..."
      let midRefData = renameRefData renames oldNodeData
      midRefData
        & rdScope %%~
          applyHoleConstraints (recurse nodeRep renames) holeConstraints
          (midRefData ^. rdBody)
        >>= ExprRefs.writeRep nodeRep
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
