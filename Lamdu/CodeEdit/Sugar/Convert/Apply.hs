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
import Data.Maybe.Utils (maybeToMPlus)
import Data.Monoid (Monoid(..))
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
import Lamdu.Data.Expression.IRef (DefM)
import Lamdu.Data.Expression.Infer.Conflicts (iwcInferred)
import qualified Control.Lens as Lens
import qualified Data.Set as Set
import qualified Data.Store.Guid as Guid
import qualified Data.Store.Property as Property
import qualified Lamdu.CodeEdit.Sugar.Convert.Hole as ConvertHole
import qualified Lamdu.CodeEdit.Sugar.Expression as SugarExpr
import qualified Lamdu.CodeEdit.Sugar.Infer as SugarInfer
import qualified Lamdu.CodeEdit.Sugar.Monad as SugarM
import qualified Lamdu.CodeEdit.Sugar.RemoveTypes as SugarRemoveTypes
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
  (Typeable1 m, MonadA m, Monoid a) =>
  Expr.Apply (ExprMM m a) ->
  PayloadMM m a -> SugarM m (ExpressionU m a)
convert app@(Expr.Apply funcI argI) exprPl =
  fmap uneither . runEitherT $ do
    justToLeft $ convertEmptyList app exprPl
    argS <- lift $ SugarM.convertSubexpression argI
    justToLeft $ convertAppliedHole funcI argS argI exprPl
    justToLeft $ convertList app argS exprPl
    funcS <- lift $ SugarM.convertSubexpression funcI
    justToLeft $ convertLabeled funcS argS argI exprPl
    lift $ convertPrefix funcS funcI argS argI exprPl

indirectDefinitionGuid :: ExpressionP name m pl -> Maybe Guid
indirectDefinitionGuid funcS =
  case funcS ^. rBody of
  BodyGetVar gv -> Just $ gv ^. gvIdentifier
  BodyCollapsed c -> Just $ c ^. cCompact . gvIdentifier
  BodyInferred i -> indirectDefinitionGuid $ i ^. iValue
  BodyGetField _ -> Nothing -- TODO: <-- do we want to make something up here?
  _ -> Nothing

indirectDefinitionPresentationMode ::
  MonadA m => ExpressionP name m pl -> SugarM m (Maybe PresentationMode)
indirectDefinitionPresentationMode =
  traverse (SugarM.getP . Anchors.assocPresentationMode) .
  indirectDefinitionGuid

noRepetitions :: Ord a => [a] -> Bool
noRepetitions x = length x == Set.size (Set.fromList x)

convertLabeled ::
  (MonadA m, Typeable1 m, Monoid a) =>
  ExpressionU m a -> ExpressionU m a -> ExprMM m a -> PayloadMM m a ->
  MaybeT (SugarM m) (ExpressionU m a)
convertLabeled funcS argS argI exprPl = do
  Record Val fields <- maybeToMPlus $ argS ^? rBody . _BodyRecord
  let
    getArg field = do
      tagG <- maybeToMPlus $ field ^? rfTag . rBody . _BodyTag
      pure AnnotatedArg
        { _aaTag = tagG
        , _aaTagExprGuid = field ^. rfTag . rPayload . plGuid
        , _aaExpr = field ^. rfExpr
        }
  args@(arg0 : args1toN@(arg1 : args2toN)) <-
    traverse getArg $ fields ^. flItems
  let
    tagGuids = args ^.. Lens.traversed . aaTag . tagGuid
  guard $ noRepetitions tagGuids
  presentationMode <- MaybeT $ indirectDefinitionPresentationMode funcS
  let
    (specialArgs, annotatedArgs) =
      case presentationMode of
      Verbose -> (NoSpecialArgs, args)
      OO -> (ObjectArg (arg0 ^. aaExpr), args1toN)
      Infix -> (InfixArgs (arg0 ^. aaExpr) (arg1 ^. aaExpr), args2toN)
  BodyApply Apply
    { _aFunc = funcS
    , _aSpecialArgs = specialArgs
    , _aAnnotatedArgs = annotatedArgs
    }
    & lift . SugarExpr.make exprPl
    <&> rPayload %~
      ( plData <>~
        mappend
        (argS ^. rPayload . plData)
        (fields ^. flItems . Lens.traversed . rfTag . rPayload . plData)
      ) .
      ( plActions . Lens._Just . mSetToInnerExpr .~ do
        stored <- exprPl ^. SugarInfer.plStored
        fieldsI <- argI ^? Expr.eBody . Expr._BodyRecord . Expr.recordFields
        val <-
          case filter (Lens.nullOf ExprLens.exprHole) (map snd fieldsI) of
          [x] -> Just x
          _ -> Nothing
        valStored <- traverse (^. SugarInfer.plStored) val
        return $
          ExprIRef.exprGuid <$>
          DataOps.setToWrapper (Property.value (valStored ^. Expr.ePayload)) stored
      )

makeCollapsed ::
  (MonadA m, Typeable1 m, Monoid a) =>
  PayloadMM m a ->
  Guid -> GetVar MStoredName m -> Bool ->
  ExpressionU m a -> SugarM m (ExpressionU m a)
makeCollapsed exprPl g compact hasInfo fullExpression =
  BodyCollapsed Collapsed
  { _cFuncGuid = g
  , _cCompact = compact
  , _cFullExprHasInfo = hasInfo
  , _cFullExpression =
    fullExpression
    & SugarRemoveTypes.inferredTypes
    & rPayload . plGuid .~ expandedGuid
    & rPayload . plData .~ mempty
  }
  & SugarExpr.make exprPl
  <&> rPayload . plData <>~ fullExpression ^. rPayload . plData
  where
    expandedGuid = Guid.combine (exprPl ^. SugarInfer.plGuid) $ Guid.fromString "polyExpanded"

convertPrefix ::
  (MonadA m, Typeable1 m, Monoid a) =>
  ExpressionU m a -> ExprMM m a -> ExpressionU m a ->
  ExprMM m a -> PayloadMM m a -> SugarM m (ExpressionU m a)
convertPrefix funcRef funcI argS argI applyPl
  | SugarInfer.isPolymorphicFunc $ funcI ^. Expr.ePayload =
    case funcRef ^. rBody of
    BodyCollapsed (Collapsed g compact full hadInfo) ->
      makeCollapsed applyPl g compact (hadInfo || haveInfo) =<< makeApply full
    BodyGetVar var -> do
      makeCollapsed applyPl (funcI ^. SugarInfer.exprGuid) var haveInfo =<< makeFullApply
    _ -> makeFullApply
  | otherwise = makeFullApply
  where
    haveInfo = Lens.nullOf ExprLens.exprHole argI
    makeFullApply = makeApply $ SugarExpr.setNextHoleToFirstSubHole argS funcRef
    makeApply f =
      SugarExpr.make applyPl $ BodyApply Apply
      { _aFunc = f
      , _aSpecialArgs = ObjectArg argS
      , _aAnnotatedArgs = []
      }

mkListAddFirstItem ::
  MonadA m => Anchors.SpecialFunctions (Tag m) -> SugarInfer.Stored m -> T m Guid
mkListAddFirstItem specialFunctions =
  fmap (ExprIRef.exprGuid . snd) . DataOps.addListItem specialFunctions

convertEmptyList ::
  (Typeable1 m, MonadA m, Monoid a) =>
  Expr.Apply (ExprMM m a) ->
  PayloadMM m a ->
  MaybeT (SugarM m) (ExpressionU m a)
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
  let
    hiddenData = app ^. Lens.traversed . Lens.traversed . SugarInfer.plData
  (lift . SugarExpr.make exprPl . BodyList)
    (List [] (mkListActions <$> exprPl ^. SugarInfer.plStored))
    <&> rPayload . plData <>~ hiddenData

isCons ::
  Anchors.SpecialFunctions t ->
  ExprIRef.Expression t a -> Bool
isCons specialFunctions =
  Lens.anyOf
  (ExprLens.exprApply . Expr.applyFunc . ExprLens.exprDefinitionRef)
  (== Anchors.sfCons specialFunctions)

typeCheckIdentityAt ::
  (MonadA m, Typeable1 m) =>
  Infer.InferNode (DefM m) -> SugarM m Bool
typeCheckIdentityAt point = do
  sugarContext <- SugarM.readContext
  SugarM.liftCTransaction .
    fmap (Lens.has Lens._Just) $
    SugarM.memoLoadInferInHoleContext sugarContext identityFunc point
  where
    identityFunc =
      ExprLens.pureExpr #
      ExprUtil.makeLambda paramGuid ExprUtil.pureHole getParam
    getParam = ExprLens.pureExpr . ExprLens.bodyParameterRef # paramGuid
    paramGuid = Guid.fromString "typeCheckId"

unwrap ::
  MonadA m =>
  ExprIRef.ExpressionProperty m ->
  ExprIRef.ExpressionProperty m ->
  SugarInfer.ExprMM def stored ->
  T m Guid
unwrap outerP argP argExpr = do
  res <- DataOps.replace outerP (Property.value argP)
  return $
    case mOrderedHoles of
    Just (x:_) -> x ^. Expr.ePayload . Lens._1
    _ -> ExprIRef.exprGuid res
  where
    mArgInferred = Lens.sequenceOf (Lens.traversed . SugarInfer.plInferred) argExpr
    f x =
      ( x ^. SugarInfer.plGuid
      , iwcInferred $ x ^. SugarInfer.plInferred
      )
    mOrderedHoles = ConvertHole.orderedInnerHoles . fmap f <$> mArgInferred

convertAppliedHole ::
  (MonadA m, Typeable1 m, Monoid a) =>
  ExprMM m a -> ExpressionU m a -> ExprMM m a -> PayloadMM m a ->
  MaybeT (SugarM m) (ExpressionU m a)
convertAppliedHole funcI rawArgS argI exprPl
  | Lens.has ExprLens.exprHole funcI = lift $ do
    isTypeMatch <-
      maybe (return False)
      (typeCheckIdentityAt . Infer.iNode . iwcInferred) $
      funcI ^. SugarInfer.exprInferred
    let
      holeArg = HoleArg
        { _haExpr = argS
        , _haExprPresugared =
          flip (,) () . fmap (StorePoint . Property.value) .
          (^. SugarInfer.plStored) <$> argI
        , _haTypeIsAMatch = isTypeMatch
        }
      mUnwrap = do
        guard isTypeMatch
        stored <- exprPl ^. SugarInfer.plStored
        argP <- argI ^. SugarInfer.exprStored
        return $ unwrap stored argP argI
    ConvertHole.convertPlain exprPl
      <&> rBody . _BodyHole %~
          (holeMArg .~ Just holeArg) .
          (holeMActions . Lens._Just . holeMUnwrap .~ mUnwrap)
      <&> rPayload . plData <>~ funcI ^. Expr.ePayload . SugarInfer.plData
  | otherwise = mzero
  where
    guid = exprPl ^. SugarInfer.plGuid
    argS =
      SugarExpr.setNextHole guid .
      (rPayload . plActions . Lens._Just . wrap .~
       AlreadyWrapped guid) $
      rawArgS

convertList ::
  (Typeable1 m, MonadA m, Monoid a) =>
  Expr.Apply (ExprMM m a) -> ExpressionU m a -> PayloadMM m a ->
  MaybeT (SugarM m) (ExpressionU m a)
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
    listItem =
      mkListItem (headField ^. rfExpr) argS exprPl argI
      (addFirstItem <$> innerListMActions)
      & liExpr . rPayload . plData <>~ funcI ^. Lens.traversed . SugarInfer.plData
    mListActions = do
      exprS <- exprPl ^. SugarInfer.plStored
      innerListActions <- innerListMActions
      pure ListActions
        { addFirstItem = mkListAddFirstItem specialFunctions exprS
        , replaceNil = replaceNil innerListActions
        }
  lift . SugarExpr.make exprPl . BodyList $
    List (listItem : innerValues) mListActions

mkListItem ::
  (MonadA m, Monoid a) =>
  ExpressionU m a -> ExpressionU m a ->
  PayloadMM m a -> ExprMM m a -> Maybe (T m Guid) ->
  ListItem m (ExpressionU m a)
mkListItem listItemExpr recordArgS exprPl argI mAddNextItem =
  ListItem
  { _liExpr =
    listItemExpr
    & SugarExpr.setNextHoleToFirstSubHole recordArgS
    & rPayload . plData <>~ recordArgS ^. rPayload . plData
  , _liMActions = do
      addNext <- mAddNextItem
      exprProp <- exprPl ^. SugarInfer.plStored
      argProp <- argI ^. SugarInfer.exprStored
      return ListItemActions
        { _itemAddNext = addNext
        , _itemDelete = SugarInfer.replaceWith exprProp argProp
        }
  }
