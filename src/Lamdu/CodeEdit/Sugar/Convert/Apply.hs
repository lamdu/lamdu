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
import Data.Store.Guid (Guid)
import Data.Store.IRef (Tag)
import Data.Traversable (traverse)
import Data.Typeable (Typeable1)
import Lamdu.CodeEdit.Sugar.Infer (ExprMM)
import Lamdu.CodeEdit.Sugar.Monad (SugarM)
import Lamdu.CodeEdit.Sugar.Types
import Lamdu.Data.Anchors (PresentationMode(..))
import Lamdu.Data.Expression.IRef (DefI)
import Lamdu.Data.Expression.Infer.Conflicts (iwcInferred)
import qualified Control.Lens as Lens
import qualified Control.Monad.Trans.Either as Either
import qualified Data.Set as Set
import qualified Data.Store.Guid as Guid
import qualified Lamdu.CodeEdit.Sugar.Convert.Hole as ConvertHole
import qualified Lamdu.CodeEdit.Sugar.Expression as SugarExpr
import qualified Lamdu.CodeEdit.Sugar.Infer as SugarInfer
import qualified Lamdu.CodeEdit.Sugar.Monad as SugarM
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Expression as Expr
import qualified Lamdu.Data.Expression.IRef as ExprIRef
import qualified Lamdu.Data.Expression.Infer as Infer
import qualified Lamdu.Data.Expression.Lens as ExprLens
import qualified Lamdu.Data.Expression.Utils as ExprUtil
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
    justToLeft $ convertAppliedHole funcI argS exprI
    justToLeft $ convertList app argS exprI
    funcS <- lift $ SugarM.convertSubexpression funcI
    justToLeft $ convertLabeled funcS argS exprI
    lift $ convertPrefix funcS funcI argS argI exprI

maybeToMPlus :: MonadPlus m => Maybe a -> m a
maybeToMPlus Nothing = mzero
maybeToMPlus (Just x) = return x

indirectDefinitionGuid :: ExpressionP name m pl -> Maybe Guid
indirectDefinitionGuid funcS =
  case funcS ^. rBody of
  BodyGetVar gv -> Just $ gv ^. gvIdentifier
  BodyCollapsed c -> Just $ c ^. cCompact . gvIdentifier
  BodyInferred i -> indirectDefinitionGuid $ i ^. iValue
  BodyGetField _ -> Nothing -- TODO: <-- do we want to make something up here?
  _ -> Nothing

indirectDefinitionPresentationMode :: MonadA m => ExpressionP name m pl -> SugarM m (Maybe PresentationMode)
indirectDefinitionPresentationMode =
  traverse (SugarM.getP . Anchors.assocPresentationMode) .
  indirectDefinitionGuid

noRepetitions :: Ord a => [a] -> Bool
noRepetitions x = length x == Set.size (Set.fromList x)

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
  args@((_, arg0) : args1toN@((_, arg1) : args2toN)) <-
    traverse getArg $ fields ^. flItems
  let tagGuids = args ^.. Lens.traversed . Lens._1 . tagGuid
  guard $ noRepetitions tagGuids
  presentationMode <- MaybeT $ indirectDefinitionPresentationMode funcS
  let
    (specialArgs, annotatedArgs) =
      case presentationMode of
      Verbose -> (NoSpecialArgs, args)
      OO -> (ObjectArg arg0, args1toN)
      Infix -> (InfixArgs arg0 arg1, args2toN)
  (lift . SugarExpr.make exprI . BodyApply) Apply
    { _aFunc = funcS
    , _aSpecialArgs = specialArgs
    , _aAnnotatedArgs = annotatedArgs
    }
    <&> rHiddenGuids <>~
        (argS ^. rGuid :
         fields ^.. flItems . Lens.traversed . rfTag . rGuid)

makeCollapsed ::
  (MonadA m, Typeable1 m) =>
  ExprMM m ->
  Guid -> GetVar MStoredName m -> Bool ->
  ExpressionU m -> SugarM m (ExpressionU m)
makeCollapsed exprI g compact hasInfo fullExpression =
  SugarExpr.make exprI $ BodyCollapsed Collapsed
    { _cFuncGuid = g
    , _cCompact = compact
    , _cFullExprHasInfo = hasInfo
    , _cFullExpression =
      Lens.set rGuid expandedGuid $ SugarExpr.removeInferredTypes fullExpression
    }
  where
    expandedGuid = Guid.combine (SugarInfer.resultGuid exprI) $ Guid.fromString "polyExpanded"

convertPrefix ::
  (MonadA m, Typeable1 m) =>
  ExpressionU m -> ExprMM m -> ExpressionU m ->
  ExprMM m -> ExprMM m -> SugarM m (ExpressionU m)
convertPrefix funcRef funcI rawArgS argI applyI = do
  sugarContext <- SugarM.readContext
  let
    argS =
      rawArgS
      & case traverse (Lens.sequenceOf SugarInfer.plStored) applyI of
        Nothing -> id
        Just storedApply ->
          rPayload . plActions . Lens.traversed %~
          (callWithNextArg .~ SugarExpr.mkCallWithArg sugarContext storedApply) .
          if Lens.has ExprLens.exprHole funcI
          then replaceWithNewHole .~ SugarExpr.mkReplaceWithNewHole storedApply
          else id
    makeFullApply = makeApply $ SugarExpr.setNextHoleToFirstSubHole argS funcRef
    makeApply f =
      SugarExpr.make applyI $ BodyApply Apply
      { _aFunc = f
      , _aSpecialArgs = ObjectArg argS
      , _aAnnotatedArgs = []
      }
  if SugarInfer.isPolymorphicFunc funcI
    then
      case funcRef ^. rBody of
      BodyCollapsed (Collapsed g compact full hadInfo) ->
        makeCollapsed applyI g compact (hadInfo || haveInfo) =<< makeApply full
      BodyGetVar var ->
        makeCollapsed applyI (SugarInfer.resultGuid funcI) var haveInfo =<< makeFullApply
      _ -> makeFullApply
    else
      makeFullApply
  where
    haveInfo = Lens.nullOf ExprLens.exprHole argI

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
  fmap (ExprIRef.exprGuid . snd) . DataOps.addListItem specialFunctions

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
      , replaceNil = ExprIRef.exprGuid <$> DataOps.setToHole exprS
      }
  guard $
    Lens.anyOf ExprLens.exprDefinitionRef
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
  ExprIRef.Expression t a -> Bool
isCons specialFunctions =
  Lens.anyOf
  (ExprLens.exprApply . Expr.applyFunc . ExprLens.exprDefinitionRef)
  (== Anchors.sfCons specialFunctions)

typeCheckIdentityAt ::
  (MonadA m, Typeable1 m) =>
  Infer.InferNode (DefI (Tag m)) -> SugarM m Bool
typeCheckIdentityAt point = do
  sugarContext <- SugarM.readContext
  let
    inferState = sugarContext ^. SugarM.scHoleInferState
    inferStateKey = sugarContext ^. SugarM.scHoleInferStateKey
  SugarM.liftCTransaction .
    fmap (Lens.has Lens._Just) $
    SugarInfer.memoLoadInfer Nothing identityFunc
    inferStateKey (inferState, point)
  where
    identityFunc =
      ExprLens.pureExpr #
      ExprUtil.makeLambda paramGuid ExprUtil.pureHole getParam
    getParam = ExprLens.pureExpr . ExprLens.bodyParameterRef # paramGuid
    paramGuid = Guid.fromString "typeCheckId"

convertAppliedHole ::
  (MonadA m, Typeable1 m) => ExprMM m -> ExpressionU m -> ExprMM m ->
  MaybeT (SugarM m) (ExpressionU m)
convertAppliedHole funcI rawArgS exprI
  | Lens.has ExprLens.exprHole funcI = lift $ do
    isTypeMatch <-
      maybe (return False)
      (typeCheckIdentityAt . Infer.iPoint . iwcInferred) $
      SugarInfer.resultInferred funcI
    let
      holeArg = HoleArg
        { _haExpr = argS
        , _haTypeIsAMatch = isTypeMatch
        }
    (rBody . _BodyHole . holeMArg .~ Just holeArg) .
      (rHiddenGuids <>~ funcGuids) <$>
      ConvertHole.convertPlain exprI
  | otherwise = mzero
  where
    argS = SugarExpr.setNextHole (SugarInfer.resultGuid exprI) rawArgS
    funcGuids = [SugarInfer.resultGuid funcI]

convertList ::
  (Typeable1 m, MonadA m) =>
  Expr.Apply (ExprMM m) ->
  ExpressionU m ->
  ExprMM m ->
  MaybeT (SugarM m) (ExpressionU m)
convertList (Expr.Apply funcI argI) argS exprI = do
  specialFunctions <- lift $ (^. SugarM.scSpecialFunctions) <$> SugarM.readContext
  Record Val (FieldList [headField, tailField] _) <-
    maybeToMPlus $ argS ^? rBody . _BodyRecord
  let
    verifyTag tag field =
      guard . (== tag specialFunctions) =<<
      maybeToMPlus (field ^? rfTag . rBody . _BodyTag . tagGuid)
  verifyTag Anchors.sfHeadTag headField
  verifyTag Anchors.sfTailTag tailField
  List innerValues innerListMActions <-
    maybeToMPlus $ tailField ^? rfExpr . rBody . _BodyList
  guard $ isCons specialFunctions funcI
  let
    hiddenGuids = (funcI ^.. subExpressionGuids) ++ (funcI ^.. SugarInfer.exprStoredGuid)
    listItem =
      mkListItem (headField ^. rfExpr) argS hiddenGuids exprI argI $
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
    & SugarExpr.setNextHoleToFirstSubHole argS
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
