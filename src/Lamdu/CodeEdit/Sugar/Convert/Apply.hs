module Lamdu.CodeEdit.Sugar.Convert.Apply
  ( convert
  ) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Lens.Operators
import Control.Monad (MonadPlus(..), guard)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Either (EitherT(..))
import Control.Monad.Trans.Either.Utils (justToLeft)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.MonadA (MonadA)
import Data.Store.Guid (Guid)
import Data.Store.IRef (Tag)
import Data.Traversable (traverse)
import Data.Typeable (Typeable1)
import Lamdu.CodeEdit.Sugar.Infer (ExprMM, PayloadMM)
import Lamdu.CodeEdit.Sugar.Internal
import Lamdu.CodeEdit.Sugar.Monad (SugarM)
import Lamdu.CodeEdit.Sugar.Types
import Lamdu.CodeEdit.Sugar.Types.Internal
import Lamdu.Data.Anchors (PresentationMode(..))
import Lamdu.Data.Expression.IRef (DefI)
import Lamdu.Data.Expression.Infer.Conflicts (iwcInferred)
import qualified Control.Lens as Lens
import qualified Data.Set as Set
import qualified Data.Store.Guid as Guid
import qualified Data.Store.Property as Property
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

convert ::
  (Typeable1 m, MonadA m) =>
  Expr.Apply (ExprMM m) ->
  PayloadMM m -> SugarM m (ExpressionU m)
convert app@(Expr.Apply funcI argI) exprPl =
  fmap uneither . runEitherT $ do
    justToLeft $ convertEmptyList app exprPl
    argS <- lift $ SugarM.convertSubexpression argI
    justToLeft $ convertAppliedHole funcI argS argI exprPl
    justToLeft $ convertList app argS exprPl
    funcS <- lift $ SugarM.convertSubexpression funcI
    justToLeft $ convertLabeled funcS argS exprPl
    lift $ convertPrefix funcS funcI argS argI exprPl

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
  ExpressionU m -> ExpressionU m -> PayloadMM m ->
  MaybeT (SugarM m) (ExpressionU m)
convertLabeled funcS argS exprPl = do
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
  (lift . SugarExpr.make exprPl . BodyApply) Apply
    { _aFunc = funcS
    , _aSpecialArgs = specialArgs
    , _aAnnotatedArgs = annotatedArgs
    }
    <&> rPayload . plHiddenGuids <>~
        (argS ^. rPayload . plGuid :
         fields ^.. flItems . Lens.traversed . rfTag . rPayload . plGuid)

makeCollapsed ::
  (MonadA m, Typeable1 m) =>
  PayloadMM m ->
  Guid -> GetVar MStoredName m -> Bool ->
  ExpressionU m -> SugarM m (ExpressionU m)
makeCollapsed exprPl g compact hasInfo fullExpression =
  SugarExpr.make exprPl $ BodyCollapsed Collapsed
    { _cFuncGuid = g
    , _cCompact = compact
    , _cFullExprHasInfo = hasInfo
    , _cFullExpression =
      fullExpression
      & SugarExpr.removeInferredTypes
      & rPayload . plGuid .~ expandedGuid
    }
  where
    expandedGuid = Guid.combine (exprPl ^. SugarInfer.plGuid) $ Guid.fromString "polyExpanded"

convertPrefix ::
  (MonadA m, Typeable1 m) =>
  ExpressionU m -> ExprMM m -> ExpressionU m ->
  ExprMM m -> PayloadMM m -> SugarM m (ExpressionU m)
convertPrefix funcRef funcI rawArgS argI applyPl = do
  sugarContext <- SugarM.readContext
  let
    argS =
      rawArgS
      & case applyPl ^. SugarInfer.plStored of
        Nothing -> id
        Just stored ->
          rPayload . plActions . Lens.traversed %~
          (callWithNextArg .~ SugarExpr.mkCallWithArg sugarContext stored) .
          if Lens.has ExprLens.exprHole funcI
          then replaceWithNewHole .~ SugarExpr.mkReplaceWithNewHole stored
          else id
    makeFullApply = makeApply $ SugarExpr.setNextHoleToFirstSubHole argS funcRef
    makeApply f =
      SugarExpr.make applyPl $ BodyApply Apply
      { _aFunc = f
      , _aSpecialArgs = ObjectArg argS
      , _aAnnotatedArgs = []
      }
  if SugarInfer.isPolymorphicFunc $ funcI ^. Expr.ePayload
    then
      case funcRef ^. rBody of
      BodyCollapsed (Collapsed g compact full hadInfo) ->
        makeCollapsed applyPl g compact (hadInfo || haveInfo) =<< makeApply full
      BodyGetVar var ->
        makeCollapsed applyPl (funcI ^. SugarInfer.exprGuid) var haveInfo =<< makeFullApply
      _ -> makeFullApply
    else
      makeFullApply
  where
    haveInfo = Lens.nullOf ExprLens.exprHole argI

setListGuid :: Guid -> ExpressionU m -> ExpressionU m
setListGuid consistentGuid e = e
  & rPayload . plGuid .~ consistentGuid
  & rPayload . plHiddenGuids %~ (e ^. rPayload . plGuid :)

storedSubExpressionGuids ::
  Lens.Fold
  (Expr.Expression def (SugarInfer.Payload i (Maybe (SugarInfer.Stored m)))) Guid
storedSubExpressionGuids = Lens.folding ExprUtil.subExpressions . SugarInfer.exprStoredGuid

mkListAddFirstItem ::
  MonadA m => Anchors.SpecialFunctions (Tag m) -> SugarInfer.Stored m -> T m Guid
mkListAddFirstItem specialFunctions =
  fmap (ExprIRef.exprGuid . snd) . DataOps.addListItem specialFunctions

convertEmptyList ::
  (Typeable1 m, MonadA m) =>
  Expr.Apply (ExprMM m) ->
  PayloadMM m ->
  MaybeT (SugarM m) (ExpressionU m)
convertEmptyList app@(Expr.Apply funcI _) exprPl = do
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
  let guids = app ^.. Lens.traversed . storedSubExpressionGuids
  (rPayload . plHiddenGuids <>~ guids) .
    setListGuid consistentGuid <$>
    (lift . SugarExpr.make exprPl . BodyList)
    (List [] (mkListActions <$> exprPl ^. SugarInfer.plStored))
  where
    consistentGuid = Guid.augment "list" $ exprPl ^. SugarInfer.plGuid

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
  (MonadA m, Typeable1 m) => ExprMM m -> ExpressionU m -> ExprMM m -> PayloadMM m ->
  MaybeT (SugarM m) (ExpressionU m)
convertAppliedHole funcI rawArgS argI exprPl
  | Lens.has ExprLens.exprHole funcI = lift $ do
    isTypeMatch <-
      maybe (return False)
      (typeCheckIdentityAt . Infer.iPoint . iwcInferred) $
      funcI ^. SugarInfer.exprInferred
    sugarContext <- SugarM.readContext
    let
      holeArg = HoleArg
        { _haExpr = argS
        , _haExprPresugared =
          fmap (StorePoint . Property.value) .
          (^. SugarInfer.plStored) <$> argI
        , _haTypeIsAMatch = isTypeMatch
        }
      mDelete =
        case (exprPl ^. SugarInfer.plStored, argI ^. SugarInfer.exprStored) of
        (Just prop, Just argP) ->
          SugarExpr.guardReinferSuccess sugarContext $
          ExprIRef.exprGuid <$> DataOps.replace prop (Property.value argP)
        _ -> return Nothing
    ( rBody . _BodyHole %~
      (holeMArg .~ Just holeArg) .
      (holeMActions . Lens._Just . holeMDelete .~ mDelete)
      ) .
      (rPayload . plHiddenGuids <>~ funcGuids) <$>
      ConvertHole.convertPlain exprPl
  | otherwise = mzero
  where
    guid = exprPl ^. SugarInfer.plGuid
    argS =
      SugarExpr.setNextHole guid .
      (rPayload . plActions . Lens._Just . wrap .~
       AlreadyWrapped guid) $
      rawArgS
    funcGuids = [funcI ^. SugarInfer.exprGuid]

convertList ::
  (Typeable1 m, MonadA m) =>
  Expr.Apply (ExprMM m) -> ExpressionU m -> PayloadMM m ->
  MaybeT (SugarM m) (ExpressionU m)
convertList (Expr.Apply funcI argI) argS exprPl = do
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
    hiddenGuids = funcI ^.. storedSubExpressionGuids
    listItem =
      mkListItem (headField ^. rfExpr) argS hiddenGuids exprPl argI $
      addFirstItem <$> innerListMActions
    mListActions = do
      exprS <- exprPl ^. SugarInfer.plStored
      innerListActions <- innerListMActions
      pure ListActions
        { addFirstItem = mkListAddFirstItem specialFunctions exprS
        , replaceNil = replaceNil innerListActions
        }
  setListGuid (argS ^. rPayload . plGuid) <$>
    (lift . SugarExpr.make exprPl . BodyList)
    (List (listItem : innerValues) mListActions)

mkListItem ::
  MonadA m =>
  ExpressionU m -> ExpressionU m -> [Guid] ->
  PayloadMM m -> ExprMM m -> Maybe (T m Guid) ->
  ListItem m (ExpressionU m)
mkListItem listItemExpr argS hiddenGuids exprPl argI mAddNextItem =
  ListItem
  { liExpr =
    listItemExpr
    & SugarExpr.setNextHoleToFirstSubHole argS
    & rPayload . plHiddenGuids <>~ hiddenGuids ++ (argS ^. rPayload . plHiddenGuids)
  , liMActions = do
      addNext <- mAddNextItem
      exprProp <- exprPl ^. SugarInfer.plStored
      argProp <- argI ^. SugarInfer.exprStored
      return ListItemActions
        { _itemAddNext = addNext
        , _itemDelete = SugarInfer.replaceWith exprProp argProp
        }
  }
