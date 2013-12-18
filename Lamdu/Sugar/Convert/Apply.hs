module Lamdu.Sugar.Convert.Apply
  ( convert
  ) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Lens.Operators
import Control.Monad (guard)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Either.Utils (runMatcherT, justToLeft)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Trans.State (StateT(..))
import Control.MonadA (MonadA)
import Data.Maybe.Utils (maybeToMPlus)
import Data.Monoid (Monoid(..))
import Data.Store.Guid (Guid)
import Data.Traversable (traverse)
import Data.Typeable (Typeable1)
import Lamdu.Data.Anchors (PresentationMode(..))
import Lamdu.Data.Expr.IRef (DefIM)
import Lamdu.Sugar.Convert.Monad (ConvertM)
import Lamdu.Sugar.Internal
import Lamdu.Sugar.Types
import Lamdu.Sugar.Types.Internal
import qualified Control.Lens as Lens
import qualified Data.Set as Set
import qualified Data.Store.Guid as Guid
import qualified Data.Store.Property as Property
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Expr as Expr
import qualified Lamdu.Data.Expr.IRef as ExprIRef
import qualified Lamdu.Data.Expr.Lens as ExprLens
import qualified Lamdu.Data.Expr.Utils as ExprUtil
import qualified Lamdu.Data.Infer as Infer
import qualified Lamdu.Data.Infer.Deref as InferDeref
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.Sugar.Convert.Expression as ConvertExpr
import qualified Lamdu.Sugar.Convert.Hole as ConvertHole
import qualified Lamdu.Sugar.Convert.Infer as SugarInfer
import qualified Lamdu.Sugar.Convert.List as ConvertList
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import qualified Lamdu.Sugar.RemoveTypes as SugarRemoveTypes

convert ::
  (Typeable1 m, MonadA m, Monoid a) =>
  Expr.Apply (InputExpr m a) ->
  InputPayload m a -> ConvertM m (ExpressionU m a)
convert app@(Expr.Apply funcI argI) exprPl =
  runMatcherT $ do
    argS <- lift $ ConvertM.convertSubexpression argI
    justToLeft $ convertAppliedHole funcI argS argI exprPl
    justToLeft $ ConvertList.convert app argS exprPl
    funcS <- lift $ ConvertM.convertSubexpression funcI
    justToLeft $ convertLabeled funcS argS argI exprPl
    lift $ convertPrefix funcS funcI argS argI exprPl

indirectDefinitionGuid :: ExpressionP name m pl -> Maybe Guid
indirectDefinitionGuid funcS =
  case funcS ^. rBody of
  BodyGetVar gv -> Just $ gv ^. gvIdentifier
  BodyCollapsed c -> Just $ c ^. cCompact . gvIdentifier
  BodyGetField _ -> Nothing -- TODO: <-- do we want to make something up here?
  _ -> Nothing

indirectDefinitionPresentationMode ::
  MonadA m => ExpressionP name m pl -> ConvertM m (Maybe PresentationMode)
indirectDefinitionPresentationMode =
  traverse (ConvertM.getP . Anchors.assocPresentationMode) .
  indirectDefinitionGuid

noRepetitions :: Ord a => [a] -> Bool
noRepetitions x = length x == Set.size (Set.fromList x)

convertLabeled ::
  (MonadA m, Typeable1 m, Monoid a) =>
  ExpressionU m a -> ExpressionU m a -> InputExpr m a -> InputPayload m a ->
  MaybeT (ConvertM m) (ExpressionU m a)
convertLabeled funcS argS argI exprPl = do
  Record KVal fields <- maybeToMPlus $ argS ^? rBody . _BodyRecord
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
    & lift . ConvertExpr.make exprPl
    <&> rPayload %~
      ( plData <>~
        mappend
        (argS ^. rPayload . plData)
        (fields ^. flItems . Lens.traversed . rfTag . rPayload . plData)
      ) .
      ( plActions . Lens._Just . mSetToInnerExpr .~ do
        stored <- exprPl ^. ipStored
        fieldsI <- argI ^? Expr.eBody . Expr._BodyRecord . Expr.recordFields
        val <-
          case filter (Lens.nullOf ExprLens.exprHole) (map snd fieldsI) of
          [x] -> Just x
          _ -> Nothing
        valStored <- traverse (^. ipStored) val
        return $
          ExprIRef.exprGuid <$>
          DataOps.setToWrapper (Property.value (valStored ^. Expr.ePayload)) stored
      )

makeCollapsed ::
  (MonadA m, Typeable1 m, Monoid a) =>
  InputPayload m a ->
  Guid -> GetVar MStoredName m -> Bool ->
  ExpressionU m a -> ConvertM m (ExpressionU m a)
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
  & ConvertExpr.make exprPl
  <&> rPayload . plData <>~ fullExpression ^. rPayload . plData
  where
    expandedGuid = Guid.combine (exprPl ^. ipGuid) $ Guid.fromString "polyExpanded"

convertPrefix ::
  (MonadA m, Typeable1 m, Monoid a) =>
  ExpressionU m a -> InputExpr m a -> ExpressionU m a ->
  InputExpr m a -> InputPayload m a -> ConvertM m (ExpressionU m a)
convertPrefix funcRef funcI argS argI applyPl
  | SugarInfer.isPolymorphicFunc $ funcI ^. Expr.ePayload =
    case funcRef ^. rBody of
    BodyCollapsed (Collapsed g compact full hadInfo) ->
      makeCollapsed applyPl g compact (hadInfo || haveInfo) =<< makeApply full
    BodyGetVar var ->
      makeCollapsed applyPl (funcI ^. SugarInfer.exprGuid) var haveInfo =<< makeFullApply
    _ -> makeFullApply
  | otherwise = makeFullApply
  where
    haveInfo = Lens.nullOf ExprLens.exprHole argI
    makeFullApply = makeApply funcRef
    makeApply f =
      ConvertExpr.make applyPl $ BodyApply Apply
      { _aFunc = f
      , _aSpecialArgs = ObjectArg argS
      , _aAnnotatedArgs = []
      }

typeCheckIdentityAt ::
  (MonadA m, Typeable1 m) =>
  Infer.TypedValue (DefIM m) -> ConvertM m Bool
typeCheckIdentityAt point = do
  sugarContext <- ConvertM.readContext
  SugarInfer.load identityFunc
    >>= SugarInfer.memoInferAt point
    & runMaybeT . (`runStateT` (sugarContext ^. ConvertM.scHoleInferContext))
    <&> Lens.has Lens._Just
    & ConvertM.liftCTransaction
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
  InputExpr def stored ->
  T m Guid
unwrap outerP argP argExpr = do
  res <- DataOps.replace outerP (Property.value argP)
  return $
    case mOrderedHoles of
    Just (x:_) -> x ^. Expr.ePayload . Lens._1
    _ -> ExprIRef.exprGuid res
  where
    mArgInferred = Lens.sequenceOf (Lens.traversed . ipInferred) argExpr
    f x =
      ( x ^. ipGuid
      , x ^. ipInferred
      )
    mOrderedHoles = ConvertHole.orderedInnerHoles . fmap f <$> mArgInferred

convertAppliedHole ::
  (MonadA m, Typeable1 m, Monoid a) =>
  InputExpr m a -> ExpressionU m a -> InputExpr m a -> InputPayload m a ->
  MaybeT (ConvertM m) (ExpressionU m a)
convertAppliedHole funcI argS argI exprPl = do
  guard $ Lens.has ExprLens.exprHole funcI
  lift $ do
    isTypeMatch <-
      maybe (return False) typeCheckIdentityAt $
      funcI ^? SugarInfer.exprInferred . Lens._Just . InferDeref.dTV
    let
      argWrap =
        maybe WrapNotAllowed
        (WrappedAlready . ExprIRef.exprGuid . Property.value) $
        exprPl ^. ipStored
      holeArg = HoleArg
        { _haExpr =
          argS
          & rPayload . plActions . Lens._Just %~
            (wrap .~ argWrap) .
            (mSetToHole .~ Nothing)
        , _haExprPresugared =
          flip (,) () . fmap (StorePoint . Property.value) .
          (^. ipStored) <$> argI
        , _haUnwrap =
          if isTypeMatch
          then UnwrapMAction mUnwrap
          else UnwrapTypeMismatch
        }
      mUnwrap = do
        stored <- exprPl ^. ipStored
        argP <- argI ^. SugarInfer.exprStored
        return $ unwrap stored argP argI
    ConvertHole.convertPlain exprPl
      <&> rBody . _BodyHole . holeMArg .~ Just holeArg
      <&> rPayload . plData <>~ funcI ^. SugarInfer.exprData
      <&> rPayload . plActions . Lens._Just . wrap .~ WrapperAlready
