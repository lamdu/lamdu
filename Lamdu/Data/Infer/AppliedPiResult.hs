module Lamdu.Data.Infer.AppliedPiResult
  ( handleAppliedPiResult
  ) where

import Control.Applicative ((<$>))
import Control.Lens.Operators
import Control.Monad (when, void)
import Data.Foldable (sequenceA_)
import Data.Map.Utils (lookupOrSelf)
import Data.Maybe (fromMaybe)
import Data.Monoid (Monoid(..))
import Data.Store.Guid (Guid)
import Data.Traversable (sequenceA)
import Data.UnionFind (Ref)
import Lamdu.Data.Infer.Internal
import Lamdu.Data.Infer.Monad (Infer)
import Lamdu.Data.Infer.Unify (fresh, freshHole, unify, forceLam)
import qualified Control.Lens as Lens
import qualified Data.Map as Map
import qualified Lamdu.Data.Expression as Expr
import qualified Lamdu.Data.Expression.Lens as ExprLens
import qualified Lamdu.Data.Expression.Utils as ExprUtil
import qualified Lamdu.Data.Infer.ExprRefs as ExprRefs
import qualified Lamdu.Data.Infer.Monad as InferM

-- Remap a Guid from piResult context to the Apply context
remapSubstGuid :: AppliedPiResult -> RenameHistory -> Guid -> Guid
remapSubstGuid apr destRenameHistory src =
  case apr ^? aprCopiedNames . Lens.ix src of
  -- If it's not a copied guid, it should be the same guid/ref in both
  -- contexts
  Nothing -> src
  Just dest ->
    fromMaybe dest $
    destRenameHistory ^? _RenameHistory . Lens.ix dest

injectRenameHistory :: RenameHistory -> AppliedPiResult -> AppliedPiResult
injectRenameHistory Untracked = id
injectRenameHistory (RenameHistory renames) =
  -- Only the copied names (in our argument map) need to be fixed,
  -- others are in shared scope so need no fixing:
  aprCopiedNames . Lens.mapped %~ lookupOrSelf renames

-- TODO: This should also substLeafs, and it should also subst getvars that aren't subst
substNode :: Eq def => Expr.Body def Ref -> AppliedPiResult -> Infer def ()
substNode srcBody rawApr = do
  destData <- InferM.liftContext $ ExprRefs.read destRef
  let
    apr = rawApr & injectRenameHistory (destData ^. rdRenameHistory)
    renameCopied = lookupOrSelf (apr ^. aprCopiedNames)
    matchLamResult srcGuid destGuid srcChildRef destChildRef
      | renameCopied srcGuid == destGuid =
        apr
        & recurse srcChildRef destChildRef
        & (,) srcGuid
      | otherwise = error "Src Guid doesn't match dest guid?!"
    matchOther srcChildRef destChildRef =
      recurse srcChildRef destChildRef apr
    -- Expensive assertion
    verifyGetParGuids srcGuid destGuid = renameCopied srcGuid == destGuid
  matchRes <-
    sequenceA $ sequenceA_ <$>
    ExprUtil.matchBody matchLamResult matchOther
    verifyGetParGuids srcBody (destData ^. rdBody)
  when (Lens.has Lens._Nothing matchRes) $
    error "We just successfully unified src and dest bodies!"
  where
    destRef = rawApr ^. aprDestRef
    recurse srcChildRef destChildRef childApr =
      handleAppliedPiResult srcChildRef $
      childApr & aprDestRef .~ destChildRef

handleAppliedPiResult :: Eq def => Ref -> AppliedPiResult -> Infer def ()
handleAppliedPiResult srcRef apr = do
  srcData <- InferM.liftContext $ ExprRefs.read srcRef
  destData <- InferM.liftContext $ ExprRefs.read destRef
  let
    srcScope = srcData ^. rdScope
    destScope = destData ^. rdScope
    -- TODO: this duplicates substNode logic about injecting the
    -- rename history into the subst, which should probably happen
    -- here instead of both places but we're not sure this is correct
    -- because there's a unify into the apply side between here and
    -- there.
    remapGuid = remapSubstGuid apr (destData ^. rdRenameHistory)
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
      srcData & rdRelations %~ (RelationAppliedPiResult apr :)
        & InferM.liftContext . ExprRefs.write srcRef
      -- We injectRenameHistory of our ancestors into the apr
      destData
        & rdRenameHistory <>~ RenameHistory mempty
        & InferM.liftContext . ExprRefs.write destRef
    srcBody@(Expr.BodyLam (Expr.Lam k srcGuid _ _)) -> do
      (destGuid, _, _) <- forceLam k destScope destRef
      substNode srcBody
        (apr & aprCopiedNames %~ Map.insert srcGuid destGuid)
    srcBody -> do
      destBodyRef <-
        InferM.liftContext $
        srcBody
        & Lens.traverse %%~ const (freshHole destScope)
        <&> ExprLens.bodyParameterRef %~ remapGuid
        >>= fresh destScope
      void $ unify destBodyRef destRef -- destBodyRef is swallowed by destRef if it had anything...
      substNode srcBody apr
  where
    destRef = apr ^. aprDestRef
