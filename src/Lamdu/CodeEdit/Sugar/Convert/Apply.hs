module Lamdu.CodeEdit.Sugar.Convert.Apply
  ( convert
  ) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Lens.Operators
import Control.Monad (MonadPlus(..), guard, (<=<))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Either (EitherT(..))
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.MonadA (MonadA)
import Data.Function (on)
import Data.Store.Guid (Guid)
import Data.Store.IRef (Tag)
import Data.Traversable (traverse)
import Data.Typeable (Typeable1)
import Lamdu.CodeEdit.Sugar.Infer (ExprMM)
import Lamdu.CodeEdit.Sugar.Monad (SugarM)
import Lamdu.CodeEdit.Sugar.Types
import qualified Control.Lens as Lens
import qualified Control.Monad.Trans.Either as Either
import qualified Data.Set as Set
import qualified Data.Store.Guid as Guid
import qualified Lamdu.CodeEdit.Sugar.Expression as SugarExpr
import qualified Lamdu.CodeEdit.Sugar.Infer as SugarInfer
import qualified Lamdu.CodeEdit.Sugar.Monad as SugarM
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Expression as Expr
import qualified Lamdu.Data.Expression.IRef as DataIRef
import qualified Lamdu.Data.Expression.Utils as ExprUtil
import qualified Lamdu.Data.Expression.Lens as ExprLens
import qualified Lamdu.Data.Ops as DataOps

uneither :: Either a a -> a
uneither = either id id

justToLeft :: Monad m => MaybeT m a -> EitherT a m ()
justToLeft = maybe (pure ()) Either.left <=< lift . runMaybeT

convert ::
  (Typeable1 m, MonadA m) =>
  Expr.Apply (ExprMM m) ->
  ExprMM m -> SugarM m (ExpressionU m)
convert app@(Expr.Apply funcI argI) exprI =
  fmap uneither . runEitherT $ do
    justToLeft $ convertEmptyList app exprI
    argS <- lift $ SugarM.convertSubexpression argI
    justToLeft $ convertList app argS exprI
    funcS <- lift $ SugarM.convertSubexpression funcI
    justToLeft $ expandSection funcS funcI argS exprI
    justToLeft $ convertLabeled funcS argS exprI
    lift $ convertPrefix funcS funcI argS exprI

maybeToMPlus :: MonadPlus m => Maybe a -> m a
maybeToMPlus Nothing = mzero
maybeToMPlus (Just x) = return x

isAtomicBody :: Body name m (ExpressionP name m pl) -> Bool
isAtomicBody BodyHole {} = True
isAtomicBody (BodyInferred inferred) =
  isAtomicBody $ inferred ^. iValue . rBody
isAtomicBody BodyCollapsed {} = True
isAtomicBody BodyAtom {} = True
isAtomicBody BodyLiteralInteger {} = True
isAtomicBody BodyTag {} = True
isAtomicBody BodyGetVar {} = True
isAtomicBody BodyGetParams {} = True
isAtomicBody _ = False
-- TODO: getField isn't atomic but we might want to allow it as left
-- side labeled func

convertLabeled ::
  (MonadA m, Typeable1 m) =>
  ExpressionU m -> ExpressionU m -> ExprMM m ->
  MaybeT (SugarM m) (ExpressionU m)
convertLabeled funcS argS exprI = do
  Record Val fields <- maybeToMPlus $ argS ^? rBody . _BodyRecord
  let
    getArg field = do
      tagG <- maybeToMPlus $ field ^? rfTag . rBody . _BodyTag
      pure (tagG, field ^. rfExpr)
  args <- traverse getArg $ fields ^. flItems
  let
    tagGuids = args ^.. Lens.traversed . Lens._1 . tagGuid
    numTags = length tagGuids
  guard $ numTags > 1
  guard $ numTags == Set.size (Set.fromList tagGuids)
  guard . isAtomicBody $ funcS ^. rBody
  lift . SugarExpr.make exprI $ BodyLabeledApply LabeledApply
    { _laFunc = SugarExpr.removeSuccessfulType funcS
    , _laArgs = args
    }

expandSection ::
  (MonadA m, Typeable1 m) =>
  ExpressionU m -> ExprMM m -> ExpressionU m -> ExprMM m ->
  MaybeT (SugarM m) (ExpressionU m)
expandSection funcS funcI argRef exprI = do
  Section mLeft op Nothing <- maybeToMPlus $ funcS ^? rBody . _BodySection . Lens._2
  let
    right =
      case argRef ^. rBody of
      BodySection _ (Section (Just _) rightOp (Just _))
        | on isSameOp (^. rBody) op rightOp -> argRef
      _ -> SugarExpr.addApplyChildParens argRef
  lift $
    SugarExpr.make exprI . BodySection DontHaveParens =<<
    case mLeft of
    Nothing
      | SugarInfer.isPolymorphicFunc funcI -> do
        newOpRef <- convertPrefix op funcI argRef exprI
        pure $ Section Nothing (SugarExpr.removeSuccessfulType newOpRef) Nothing
      | otherwise ->
        pure $ Section (Just (SugarExpr.addApplyChildParens argRef)) op Nothing
    Just left ->
      pure $ on (Section . Just) (SugarExpr.setNextHole right) left op (Just right)
  where
    -- TODO: Handle left/right-associativity
    isSameOp (BodyCollapsed p0) (BodyCollapsed p1) =
      on isSameVar (^. pCompact) p0 p1
    isSameOp (BodyGetVar v0) (BodyGetVar v1) =
      isSameVar v0 v1
    isSameOp _ _ = False
    isSameVar = on (==) (^. gvIdentifier)

makeCollapsed ::
  (MonadA m, Typeable1 m) =>
  ExprMM m ->
  Guid -> GetVar MStoredName m -> ExpressionU m -> SugarM m (ExpressionU m)
makeCollapsed exprI g compact fullExpression =
  SugarExpr.make exprI $ BodyCollapsed Collapsed
    { _pFuncGuid = g
    , _pCompact = compact
    , _pFullExpression =
      Lens.set rGuid expandedGuid $ SugarExpr.removeInferredTypes fullExpression
    }
  where
    expandedGuid = Guid.combine (SugarInfer.resultGuid exprI) $ Guid.fromString "polyExpanded"

convertPrefix ::
  (MonadA m, Typeable1 m) =>
  ExpressionU m -> ExprMM m -> ExpressionU m ->
  ExprMM m -> SugarM m (ExpressionU m)
convertPrefix funcRef funcI argRef applyI = do
  sugarContext <- SugarM.readContext
  let
    newArgRef = addCallWithNextArg $ SugarExpr.addParens argRef
    fromMaybeStored = traverse (SugarInfer.ntraversePayload pure id)
    onStored expr f = maybe id f $ fromMaybeStored expr
    addCallWithNextArg =
      onStored applyI $ \applyS ->
        rPayload . plActions . Lens.mapped . callWithNextArg .~
        SugarExpr.mkCallWithArg sugarContext applyS
    newFuncRef =
      SugarExpr.setNextHole newArgRef .
      SugarExpr.addApplyChildParens .
      SugarExpr.removeSuccessfulType $
      funcRef
    makeFullApply = makeApply newFuncRef
    makeApply f =
      SugarExpr.make applyI . BodyApply DontHaveParens $
      Expr.Apply f newArgRef
  if SugarInfer.isPolymorphicFunc funcI
    then
      case funcRef ^. rBody of
      BodyCollapsed (Collapsed g compact full) ->
        makeCollapsed applyI g compact =<< makeApply full
      BodyGetVar var ->
        makeCollapsed applyI (SugarInfer.resultGuid funcI) var =<< makeFullApply
      _ -> makeFullApply
    else
      makeFullApply

setListGuid :: Guid -> ExpressionU m -> ExpressionU m
setListGuid consistentGuid e = e
  & rGuid .~ consistentGuid
  & rHiddenGuids %~ (e ^. rGuid :)

subExpressionGuids ::
  Lens.Fold
  (Expr.Expression def (SugarInfer.Payload t i (Maybe (SugarInfer.Stored m)))) Guid
subExpressionGuids = Lens.folding ExprUtil.subExpressions . SugarInfer.exprStoredGuid

mkListAddFirstItem ::
  MonadA m => Anchors.SpecialFunctions (Tag m) -> SugarInfer.Stored m -> T m Guid
mkListAddFirstItem specialFunctions =
  fmap (DataIRef.exprGuid . snd) . DataOps.addListItem specialFunctions

convertEmptyList ::
  (Typeable1 m, MonadA m) =>
  Expr.Apply (ExprMM m) ->
  ExprMM m ->
  MaybeT (SugarM m) (ExpressionU m)
convertEmptyList app@(Expr.Apply funcI _) exprI = do
  specialFunctions <-
    lift $ (^. SugarM.scSpecialFunctions) <$> SugarM.readContext
  let
    mkListActions exprS =
      ListActions
      { addFirstItem = mkListAddFirstItem specialFunctions exprS
      , replaceNil = DataIRef.exprGuid <$> DataOps.setToHole exprS
      }
  guard $
    Lens.anyOf (Expr.eBody . ExprLens.bodyDefinitionRef)
    (== Anchors.sfNil specialFunctions) funcI
  let guids = app ^.. Lens.traversed . subExpressionGuids
  (rHiddenGuids <>~ guids) .
    setListGuid consistentGuid <$>
    (lift . SugarExpr.make exprI . BodyList)
    (List [] (mkListActions <$> SugarInfer.resultStored exprI))
  where
    consistentGuid = Guid.augment "list" (SugarInfer.resultGuid exprI)

isCons ::
  Anchors.SpecialFunctions t ->
  DataIRef.Expression t a -> Bool
isCons specialFunctions =
  Lens.anyOf
  (Expr.eBody . Expr._BodyApply . Expr.applyFunc . Expr.eBody . ExprLens.bodyDefinitionRef)
  (== Anchors.sfCons specialFunctions)

convertList ::
  (Typeable1 m, MonadA m) =>
  Expr.Apply (ExprMM m) ->
  ExpressionU m ->
  ExprMM m ->
  MaybeT (SugarM m) (ExpressionU m)
convertList (Expr.Apply funcI argI) argS exprI = do
  specialFunctions <- lift $ (^. SugarM.scSpecialFunctions) <$> SugarM.readContext
  Expr.Apply funcFuncI funcArgI <-
    maybeToMPlus $ funcI ^? Expr.eBody . Expr._BodyApply
  List innerValues innerListMActions <-
    maybeToMPlus $ argS ^? rBody . _BodyList
  guard $ isCons specialFunctions funcFuncI
  listItemExpr <- lift $ SugarM.convertSubexpression funcArgI
  let
    hiddenGuids = (funcFuncI ^.. subExpressionGuids) ++ (funcI ^.. SugarInfer.exprStoredGuid)
    listItem =
      mkListItem listItemExpr argS hiddenGuids exprI argI $
      addFirstItem <$> innerListMActions
    mListActions = do
      exprS <- SugarInfer.resultStored exprI
      innerListActions <- innerListMActions
      pure ListActions
        { addFirstItem = mkListAddFirstItem specialFunctions exprS
        , replaceNil = replaceNil innerListActions
        }
  setListGuid (argS ^. rGuid) <$>
    (lift . SugarExpr.make exprI . BodyList)
    (List (listItem : innerValues) mListActions)

mkListItem ::
  MonadA m =>
  ExpressionU m -> ExpressionU m -> [Guid] ->
  ExprMM m -> ExprMM m -> Maybe (T m Guid) ->
  ListItem m (ExpressionU m)
mkListItem listItemExpr argS hiddenGuids exprI argI mAddNextItem =
  ListItem
  { liExpr =
    listItemExpr
    & SugarExpr.setNextHole argS
    & rHiddenGuids <>~ hiddenGuids ++ (argS ^. rHiddenGuids)
  , liMActions = do
      addNext <- mAddNextItem
      exprProp <- SugarInfer.resultStored exprI
      argProp <- SugarInfer.resultStored argI
      return ListItemActions
        { _itemAddNext = addNext
        , _itemDelete = SugarInfer.replaceWith exprProp argProp
        }
  }
