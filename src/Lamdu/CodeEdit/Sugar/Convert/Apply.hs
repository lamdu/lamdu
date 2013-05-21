module Lamdu.CodeEdit.Sugar.Convert.Apply
  ( convert, makeCollapsed
  ) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Lens.Operators
import Control.MonadA (MonadA)
import Data.Function (on)
import Data.Maybe (fromMaybe)
import Data.Store.Guid (Guid)
import Data.Store.IRef (Tag)
import Data.Traversable (traverse)
import Data.Typeable (Typeable1)
import Lamdu.CodeEdit.Sugar.Infer (ExprMM)
import Lamdu.CodeEdit.Sugar.Monad (SugarM)
import Lamdu.CodeEdit.Sugar.Types
import qualified Control.Lens as Lens
import qualified Data.Store.Guid as Guid
import qualified Lamdu.CodeEdit.Sugar.Expression as SugarExpr
import qualified Lamdu.CodeEdit.Sugar.Infer as SugarInfer
import qualified Lamdu.CodeEdit.Sugar.Monad as SugarM
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Expression as Expression
import qualified Lamdu.Data.Expression.IRef as DataIRef
import qualified Lamdu.Data.Expression.Utils as ExprUtil
import qualified Lamdu.Data.Ops as DataOps

type Convertor m = ExprMM m -> SugarM m (ExpressionU m)

infixr 9 `orElse`
orElse :: Monad m => m (Maybe a) -> m a -> m a
orElse a b = maybe b return =<< a

convert ::
  (Typeable1 m, MonadA m) =>
  Expression.Apply (ExprMM m) ->
  Convertor m
convert app@(Expression.Apply funcI argI) exprI = do
  -- if we're an apply of the form (nil T): Return an empty list
  specialFunctions <- (^. SugarM.scSpecialFunctions) <$> SugarM.readContext
  convertEmptyList app specialFunctions exprI
    `orElse` do
      argS <- SugarM.convertSubexpression argI
      convertList app argS specialFunctions exprI
        `orElse`
        convertNormal funcI argS exprI

applyOnSection ::
  (MonadA m, Typeable1 m) => Section (ExpressionU m) ->
  ExpressionU m -> ExprMM m -> ExpressionU m ->
  Convertor m
applyOnSection (Section Nothing op Nothing) _ funcI argRef exprI
  | SugarInfer.isPolymorphicFunc funcI = do
    newOpRef <-
      convertPrefix op funcI argRef exprI
    SugarExpr.make exprI . BodySection DontHaveParens $
      Section Nothing (SugarExpr.removeSuccessfulType newOpRef) Nothing
  | otherwise =
    SugarExpr.make exprI . BodySection DontHaveParens $
    Section (Just (SugarExpr.addApplyChildParens argRef)) op Nothing
applyOnSection (Section (Just left) op Nothing) _ _ argRef exprI =
  SugarExpr.make exprI . BodySection DontHaveParens $
  on (Section . Just) (SugarExpr.setNextHole right) left op (Just right)
  where
    -- TODO: Handle left/right-associativity
    isSameOp (BodyCollapsed p0) (BodyCollapsed p1) =
      on isSameVar (^. pCompact) p0 p1
    isSameOp (BodyGetVar v0) (BodyGetVar v1) =
      isSameVar v0 v1
    isSameOp _ _ = False
    isSameVar = on (==) (^. gvIdentifier)
    right =
      case argRef ^. rBody of
      BodySection _ (Section (Just _) rightOp (Just _))
        | on isSameOp (^. rBody) op rightOp -> argRef
      _ -> SugarExpr.addApplyChildParens argRef
applyOnSection _ funcRef funcI argRef exprI = convertPrefix funcRef funcI argRef exprI

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
  Convertor m
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
      Expression.Apply f newArgRef
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

convertNormal ::
  (Typeable1 m, MonadA m) =>
  ExprMM m -> ExpressionU m -> Convertor m
convertNormal funcI argS exprI = do
  funcS <- SugarM.convertSubexpression funcI
  case funcS ^. rBody of
    BodySection _ section ->
      applyOnSection section funcS funcI argS exprI
    _ -> convertPrefix funcS funcI sugaredArg exprI
  where
    sugaredArg =
      fromMaybe argS $ do
        [field] <- argS ^? rBody . _BodyRecord . rFields . flItems
        pure $ field ^. rfExpr

setListGuid :: Guid -> ExpressionU m -> ExpressionU m
setListGuid consistentGuid e = e
  & rGuid .~ consistentGuid
  & rHiddenGuids %~ (e ^. rGuid :)

subExpressionGuids ::
  Lens.Fold
  (Expression.Expression def (SugarInfer.Payload t i (Maybe (SugarInfer.Stored m)))) Guid
subExpressionGuids = Lens.folding ExprUtil.subExpressions . SugarInfer.exprStoredGuid

mkListAddFirstItem ::
  MonadA m => Anchors.SpecialFunctions (Tag m) -> SugarInfer.Stored m -> T m Guid
mkListAddFirstItem specialFunctions =
  fmap (DataIRef.exprGuid . snd) . DataOps.addListItem specialFunctions

convertEmptyList ::
  (Typeable1 m, MonadA m) =>
  Expression.Apply (ExprMM m) ->
  Anchors.SpecialFunctions (Tag m) ->
  ExprMM m ->
  SugarM m (Maybe (ExpressionU m))
convertEmptyList app@(Expression.Apply funcI _) specialFunctions exprI
  | Lens.anyOf
    (Expression.eBody . ExprUtil.bodyDefinitionRef)
    (== Anchors.sfNil specialFunctions)
    funcI
  = Just . (rHiddenGuids <>~ (app ^.. Lens.traversed . subExpressionGuids)) .
    setListGuid consistentGuid <$>
    (SugarExpr.make exprI . BodyList)
    (List [] (mkListActions <$> SugarInfer.resultStored exprI))
  | otherwise = pure Nothing
  where
    consistentGuid = Guid.augment "list" (SugarInfer.resultGuid exprI)
    mkListActions exprS =
      ListActions
      { addFirstItem = mkListAddFirstItem specialFunctions exprS
      , replaceNil = DataIRef.exprGuid <$> DataOps.setToHole exprS
      }

convertList ::
  (Typeable1 m, MonadA m) =>
  Expression.Apply (ExprMM m) ->
  ExpressionU m ->
  Anchors.SpecialFunctions (Tag m) ->
  ExprMM m ->
  SugarM m (Maybe (ExpressionU m))
convertList (Expression.Apply funcI argI) argS specialFunctions exprI =
  case (funcI ^? eApply, argS ^. rBody) of
  ( Just (Expression.Apply funcFuncI funcArgI)
    , BodyList (List innerValues innerListMActions)
    )
    -- exprI@(funcI@(funcFuncI funcArgI) argI)
    | Lens.anyOf
      (eApply . Expression.applyFunc . Expression.eBody . ExprUtil.bodyDefinitionRef)
      (== Anchors.sfCons specialFunctions)
      funcFuncI
      -- exprI@(funcI@(funcFuncI@(cons _) funcArgI) argI)
      -> Just <$> do
        listItemExpr <- SugarM.convertSubexpression funcArgI
        let
          listItem =
            ListItem
            { liExpr =
              listItemExpr
              & SugarExpr.setNextHole argS
              & rHiddenGuids <>~
                concat
                [ funcFuncI ^.. subExpressionGuids
                , funcI ^.. SugarInfer.exprStoredGuid
                , argS ^. rHiddenGuids
                ]
            , liMActions = do
                addNext <- addFirstItem <$> innerListMActions
                exprProp <- SugarInfer.resultStored exprI
                argProp <- SugarInfer.resultStored argI
                return ListItemActions
                  { _itemAddNext = addNext
                  , _itemDelete = SugarInfer.replaceWith exprProp argProp
                  }
            }
          mListActions = do
            exprS <- SugarInfer.resultStored exprI
            innerListActions <- innerListMActions
            pure ListActions
              { addFirstItem = mkListAddFirstItem specialFunctions exprS
              , replaceNil = replaceNil innerListActions
              }
        setListGuid (argS ^. rGuid) <$>
          (SugarExpr.make exprI . BodyList)
          (List (listItem : innerValues) mListActions)
  _ -> pure Nothing

eApply :: Lens.Traversal' (Expression.Expression def a) (Expression.Apply (Expression.Expression def a))
eApply = Expression.eBody . Expression._BodyApply
