module Lamdu.Data.Infer.Unify
  ( unify, forceLam
  , U, uInfer, decycleDefend
  ) where

import Control.Applicative ((<$>), (<$), Applicative(..))
import Control.Lens.Operators
import Control.Monad (when, unless, guard)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Decycle (DecycleT, runDecycleT, visit)
import Control.Monad.Trans.State (state)
import Control.Monad.Trans.Writer (WriterT(..))
import Data.Foldable (traverse_)
import Data.Maybe.Utils (unsafeUnjust)
import Data.Monoid (Monoid(..))
import Data.Monoid.Applicative (ApplicativeMonoid(..))
import Data.Set (Set)
import Data.Store.Guid (Guid)
import Data.Traversable (sequenceA)
import Lamdu.Data.Infer.Monad (Infer, Error(..))
import Lamdu.Data.Infer.RefData (RefData(..), Scope(..), scopeNormalizeParamRefs)
import Lamdu.Data.Infer.RefTags (ExprRef, TagParam, TagRule)
import Lamdu.Data.Infer.Trigger (Trigger)
import System.Random (Random, random)
import qualified Control.Lens as Lens
import qualified Control.Monad.Trans.State as State
import qualified Control.Monad.Trans.Writer as Writer
import qualified Data.List as List
import qualified Data.OpaqueRef as OR
import qualified Data.UnionFind.WithData as UFData
import qualified Lamdu.Data.Expression as Expr
import qualified Lamdu.Data.Expression.Lens as ExprLens
import qualified Lamdu.Data.Expression.Utils as ExprUtil
import qualified Lamdu.Data.Infer.Context as Context
import qualified Lamdu.Data.Infer.GuidAliases as GuidAliases
import qualified Lamdu.Data.Infer.Monad as InferM
import qualified Lamdu.Data.Infer.RefData as RefData
import qualified Lamdu.Data.Infer.Trigger as Trigger

newRandom :: Random r => Infer def r
newRandom = InferM.liftContext . Lens.zoom Context.randomGen $ state random

forceLam ::
  Ord def =>
  Expr.Kind -> RefData.Scope def ->
  ExprRef def ->
  Infer def (Guid, ExprRef def, ExprRef def)
forceLam k lamScope destRef = do
  newGuid <- newRandom
  newParamRep <- InferM.liftGuidAliases $ GuidAliases.getRep newGuid
  newParamTypeRef <- InferM.liftContext . Context.fresh lamScope $ ExprLens.bodyHole # ()
  -- TODO: Directly manipulate RefData to avoid scope buildup?
  let lamResultScope = lamScope & RefData.scopeMap . Lens.at newParamRep .~ Just newParamTypeRef
  newResultTypeRef <- InferM.liftContext . Context.fresh lamResultScope $ ExprLens.bodyHole # ()
  newLamRef <-
    InferM.liftContext . Context.fresh lamScope . Expr.BodyLam $
    Expr.Lam k newGuid newParamTypeRef newResultTypeRef
  rep <- unify newLamRef destRef
  body <- InferM.liftUFExprs $ (^. RefData.rdBody) <$> State.gets (UFData.readRep rep)
  return . unsafeUnjust "We just unified Lam into rep" $
    body ^? ExprLens.bodyKindedLam k

intersectMDefs :: Eq def => Maybe def -> Maybe def -> Maybe def
intersectMDefs (Just x) (Just y) | x == y = Just x
intersectMDefs _ _ = Nothing

-- If we don't assert that the scopes have same refs we could be pure
intersectScopes :: Eq def => Scope def -> Scope def -> Infer def (Scope def)
intersectScopes aScope bScope = do
  Scope aScopeNorm mADef <- InferM.liftGuidAliases $ scopeNormalizeParamRefs aScope
  Scope bScopeNorm mBDef <- InferM.liftGuidAliases $ scopeNormalizeParamRefs bScope
  (`Scope` intersectMDefs mADef mBDef) <$>
    sequenceA (OR.refMapIntersectionWith verifyEquiv aScopeNorm bScopeNorm)
  where
    -- Expensive assertion
    verifyEquiv aref bref = do
      equiv <- InferM.liftUFExprs $ UFData.equiv aref bref
      if equiv
        then return aref
        else error "Scope unification of differing refs"

data HoleConstraints def = HoleConstraints
  { hcUnusableScopeReps :: OR.RefSet (TagParam def)
  , hcRemoveDef :: Bool
  }

-- You must apply this recursively
checkHoleConstraints :: HoleConstraints def -> Expr.Body def (ExprRef def) -> Infer def ()
checkHoleConstraints (HoleConstraints unusableSet _removeDef) body =
  case body of
  Expr.BodyLeaf (Expr.GetVariable (Expr.ParameterRef paramGuid)) -> do
    paramIdRep <- getRep paramGuid
    when (unusableSet ^. Lens.contains paramIdRep) $
      InferM.error $ VarEscapesScope paramGuid
  -- Expensive assertion
  Expr.BodyLam lam -> do
    paramIdRep <- getRep (lam ^. Expr.lamParamId)
    when (unusableSet ^. Lens.contains paramIdRep) $
      error "checkHoleConstraints: Shadowing detected"
  _ -> return ()
  where
    getRep = InferM.liftGuidAliases . GuidAliases.getRep

type U def = DecycleT (ExprRef def) (Infer def)

uInfer :: Infer def a -> U def a
uInfer = lift

type WU def = WriterT (ApplicativeMonoid (U def) ()) (U def)
wuInfer :: Infer def a -> WU def a
wuInfer = lift . uInfer
wuRun :: WU def a -> U def (a, U def ())
wuRun = fmap (Lens._2 %~ runApplicativeMonoid) . runWriterT
wuLater :: U def () -> WU def ()
wuLater = Writer.tell . ApplicativeMonoid

noConstraints :: HoleConstraints def -> Bool
noConstraints (HoleConstraints unusableScopeReps removeDef) =
  OR.refSetNull unusableScopeReps && not removeDef

applyHoleConstraints ::
  Eq def => HoleConstraints def ->
  Expr.Body def (ExprRef def) -> Scope def ->
  WU def (Scope def)
applyHoleConstraints holeConstraints body oldScope = do
  wuInfer $ checkHoleConstraints holeConstraints body
  let isUnusable x = oldUnusable ^. Lens.contains x
  Scope oldScopeNorm oldScopeMDef <- wuInfer . InferM.liftGuidAliases $ scopeNormalizeParamRefs oldScope
  let
    (unusables, usables) = List.partition (isUnusable . fst) $ oldScopeNorm ^@.. Lens.itraversed
    newHoleConstraints = HoleConstraints
      { hcUnusableScopeReps = OR.refSetFromList $ fst <$> unusables
      , hcRemoveDef = oldRemoveDef && Lens.has Lens._Just oldScopeMDef
      }
  unless (noConstraints newHoleConstraints) . wuLater $
    traverse_ (holeConstraintsRecurse newHoleConstraints) body
  return $ oldScope
    & RefData.scopeMap .~ OR.refMapFromList usables
    & RefData.scopeMDef %~
      if oldRemoveDef
      then const Nothing
      else id
  where
    HoleConstraints oldUnusable oldRemoveDef = holeConstraints

unifyWithHole ::
  Eq def => Scope def -> Scope def -> Expr.Body def (ExprRef def) ->
  WU def (Scope def, Expr.Body def (ExprRef def))
unifyWithHole holeScope otherScope nonHoleBody = do
  ( Scope holeScopeMapNorm holeScopeMDef
    , otherScopeNorm@(Scope otherScopeMapNorm otherScopeMDef)
    ) <-
    wuInfer . InferM.liftGuidAliases $
    (,) <$> scopeNormalizeParamRefs holeScope <*> scopeNormalizeParamRefs otherScope
  let
    removeDef = Lens.has Lens._Nothing $ intersectMDefs holeScopeMDef otherScopeMDef
    unusableScopeReps = OR.refMapKeysSet $ OR.refMapDifference otherScopeMapNorm holeScopeMapNorm
    holeConstraints = HoleConstraints unusableScopeReps removeDef
  if noConstraints holeConstraints
    then return (otherScopeNorm, nonHoleBody)
    else
      applyHoleConstraints holeConstraints nonHoleBody otherScopeNorm
      <&> flip (,) nonHoleBody

mergeScopeBodies ::
  Ord def =>
  Scope def -> Expr.Body def (ExprRef def) ->
  Scope def -> Expr.Body def (ExprRef def) ->
  WU def (Scope def, Expr.Body def (ExprRef def))
mergeScopeBodies xScope xBody yScope yBody =
  case (xBody, yBody) of
    (_, Expr.BodyLeaf Expr.Hole) -> unifyWithHole yScope xScope xBody
    (Expr.BodyLeaf Expr.Hole, _) -> unifyWithHole xScope yScope yBody
    _ -> do
      intersectedScope <- wuInfer $ intersectScopes xScope yScope
      wuLater $
        handleMatchResult =<<
        ExprUtil.matchBody matchLamResult unifyRecurse matchGetPars xBody yBody
      return (intersectedScope, yBody)
  where
    zoomGuidAliases = uInfer . InferM.liftGuidAliases
    handleMatchResult Nothing = uInfer . InferM.error $ Mismatch xBody yBody
    handleMatchResult (Just _) = return ()
    matchLamResult xGuid yGuid xRef yRef = do
      (_guidRep, resGuid) <- zoomGuidAliases $ GuidAliases.unify xGuid yGuid
      (,) resGuid <$> unifyRecurse xRef yRef
    matchGetPars xGuid yGuid = zoomGuidAliases $ do
      xRep <- GuidAliases.getRep xGuid
      yRep <- GuidAliases.getRep yGuid
      return $ yGuid <$ guard (xRep == yRep)

mergeRefData ::
  Ord def => RefData def -> RefData def ->
  WU def (RefData def)
mergeRefData
  (RefData aScope aWasNotDirectlyTag aTriggers aRestrictions aBody)
  (RefData bScope bWasNotDirectlyTag bTriggers bRestrictions bBody) =
  mkRefData <$> mergeScopeBodies aScope aBody bScope bBody
  where
    mkRefData (scope, mergedBody) =
      RefData
      { _rdScope = scope
      , _rdWasNotDirectlyTag = mappend aWasNotDirectlyTag bWasNotDirectlyTag
      , _rdTriggers = OR.refMapUnionWith mappend aTriggers bTriggers
      , _rdRestrictions = aRestrictions ++ bRestrictions
      , _rdBody = mergedBody
      }

mergeRefDataAndTrigger ::
  Ord def =>
  ExprRef def ->
  (ExprRef def, RefData def) ->
  (ExprRef def, RefData def) ->
  WU def (RefData def)
mergeRefDataAndTrigger rep a@(_, aData) b@(_, bData) = do
  wuInfer . InferM.liftContext $ do
    Context.removeFromVisibility a
    Context.removeFromVisibility b
  mergedData <- mergeRefData aData bData
  wuInfer . InferM.liftContext $ Context.addToVisibility (rep, mergedData)
  wuInfer $ Trigger.updateRefData rep mergedData

decycleDefend :: ExprRef def -> (ExprRef def -> U def a) -> U def a
decycleDefend ref action = do
  nodeRep <- uInfer . InferM.liftUFExprs $ UFData.find ref
  mResult <- visit nodeRep (action nodeRep)
  case mResult of
    Nothing -> uInfer . InferM.error $ InfiniteExpression nodeRep
    Just result -> return result

holeConstraintsRecurse ::
  Eq def => HoleConstraints def -> ExprRef def -> U def (ExprRef def)
holeConstraintsRecurse holeConstraints rawNode =
  decycleDefend rawNode $ \nodeRep -> do
    oldNodeData <- uInfer . InferM.liftUFExprs . State.gets $ UFData.readRep nodeRep
    uInfer . InferM.liftUFExprs . UFData.writeRep nodeRep $
      error "Reading node during write..."
    (newRefData, later) <-
      wuRun $
      oldNodeData
      & RefData.rdScope %%~
        applyHoleConstraints holeConstraints
        (oldNodeData ^. RefData.rdBody)
    uInfer . InferM.liftUFExprs $ UFData.writeRep nodeRep newRefData
    later
    return nodeRep

fireUnificationTriggers ::
  ExprRef def -> OR.RefMap (TagRule def) (Set (Trigger def)) -> ExprRef def ->
  Infer def ()
fireUnificationTriggers rep triggers unifiedWithRep =
  traverse_ act $ triggers ^@.. Lens.itraversed <. Lens.folded
  where
    act (ruleRef, Trigger.OnUnify) =
      InferM.ruleTrigger ruleRef rep $ Trigger.FiredUnify unifiedWithRep
    act _ = return ()

unifyRecurse ::
  Ord def => ExprRef def -> ExprRef def -> U def (ExprRef def)
unifyRecurse xRef yRef =
  decycleDefend xRef $ \xRep -> do
    yRep <- uInfer . InferM.liftUFExprs $ UFData.find yRef
    (rep, unifyResult) <- uInfer . InferM.liftUFExprs $ UFData.unifyRefs xRep yRep
    case unifyResult of
      UFData.UnifyRefsAlreadyUnified -> return ()
      UFData.UnifyRefsUnified xData yData -> do
        uInfer $ fireUnificationTriggers xRep (xData ^. RefData.rdTriggers) yRep
        uInfer $ fireUnificationTriggers yRep (yData ^. RefData.rdTriggers) xRep
        (mergedRefData, later) <-
          wuRun $ mergeRefDataAndTrigger rep (xRep, xData) (yRep, yData)
        -- First let's write the mergedRefData so we're not in danger zone
        -- of reading missing data:
        uInfer . InferM.liftUFExprs $ UFData.write rep mergedRefData
        -- Now lets do the deferred recursive unifications:
        later
    return rep

unify :: Ord def => ExprRef def -> ExprRef def -> Infer def (ExprRef def)
unify x y = runDecycleT $ unifyRecurse x y
