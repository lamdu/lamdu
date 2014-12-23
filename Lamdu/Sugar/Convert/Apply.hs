module Lamdu.Sugar.Convert.Apply
  ( convert
  ) where

import Control.Applicative ((<$>))
import Control.Lens.Operators
import Control.Monad (guard, unless)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Either.Utils (runMatcherT, justToLeft)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Trans.State (evalStateT)
import Control.MonadA (MonadA)
import Data.Maybe.Utils (maybeToMPlus)
import Data.Monoid (Monoid(..))
import Data.Store.Guid (Guid)
import Data.Store.Transaction (Transaction)
import Data.Traversable (traverse)
import Lamdu.Expr.Type (Type)
import Lamdu.Expr.Val (Val(..))
import Lamdu.Infer.Unify (unify)
import Lamdu.Sugar.Convert.Expression.Actions (addActions)
import Lamdu.Sugar.Convert.Monad (ConvertM)
import Lamdu.Sugar.Internal
import Lamdu.Sugar.Types
import qualified Control.Lens as Lens
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Store.Property as Property
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.Lens as ExprLens
import qualified Lamdu.Expr.RecordVal as RecordVal
import qualified Lamdu.Expr.UniqueId as UniqueId
import qualified Lamdu.Expr.Val as V
import qualified Lamdu.Infer as Infer
import qualified Lamdu.Sugar.Convert.Hole as ConvertHole
import qualified Lamdu.Sugar.Convert.Input as Input
import qualified Lamdu.Sugar.Convert.List as ConvertList
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import qualified Lamdu.Sugar.Internal.EntityId as EntityId

type T = Transaction

convert ::
  (MonadA m, Monoid a) => V.Apply (Val (Input.Payload m a)) ->
  Input.Payload m a -> ConvertM m (ExpressionU m a)
convert app@(V.Apply funcI argI) exprPl =
  runMatcherT $ do
    argS <- lift $ ConvertM.convertSubexpression argI
    justToLeft $ convertAppliedHole funcI argS argI exprPl
    justToLeft $ ConvertList.cons app argS exprPl
    funcS <-
      ConvertM.convertSubexpression funcI
      & lift
    justToLeft $ convertLabeled funcS argS argI exprPl
    lift $ convertPrefix funcS argS exprPl

indirectDefinitionGuid :: ExpressionU m pl -> Maybe Guid
indirectDefinitionGuid funcS =
  case funcS ^. rBody of
  -- TODO: This is incorrect because BodyGetVar is used even when it's
  -- a GetField behind the scenes, and we probably don't want to
  -- associate the Guid of the tag here? Need to throw this Guid or
  -- associated data into the GetVar/GetField itself anyway!
  BodyGetVar gv -> Just (gv ^. gvName)
  BodyGetField _ -> Nothing -- TODO: <-- do we want to make something up here?
  _ -> Nothing

indirectDefinitionPresentationMode ::
  MonadA m => ExpressionU m pl -> ConvertM m (Maybe PresentationMode)
indirectDefinitionPresentationMode =
  traverse (ConvertM.getP . Anchors.assocPresentationMode) .
  indirectDefinitionGuid

noRepetitions :: Ord a => [a] -> Bool
noRepetitions x = length x == Set.size (Set.fromList x)

convertLabeled ::
  (MonadA m, Monoid a) =>
  ExpressionU m a -> ExpressionU m a -> Val (Input.Payload m a) -> Input.Payload m a ->
  MaybeT (ConvertM m) (ExpressionU m a)
convertLabeled funcS argS argI exprPl = do
  record <- maybeToMPlus $ argS ^? rBody . _BodyRecord
  guard $ length (record ^. rItems) >= 2
  let
    getArg field =
      AnnotatedArg
        { _aaTag = field ^. rfTag
        , _aaTagExprEntityId = field ^. rfTag . tagInstance
        , _aaExpr = field ^. rfExpr
        }
  let args@(arg0 : args1toN@(arg1 : args2toN)) = map getArg $ record ^. rItems
  let tags = args ^.. Lens.traversed . aaTag . tagVal
  unless (noRepetitions tags) $ error "Repetitions should not type-check"
  presentationMode <- MaybeT $ indirectDefinitionPresentationMode funcS
  protectedSetToVal <- lift ConvertM.typeProtectedSetToVal
  let
    (specialArgs, annotatedArgs) =
      case presentationMode of
      Verbose -> (NoSpecialArgs, args)
      OO -> (ObjectArg (arg0 ^. aaExpr), args1toN)
      Infix -> (InfixArgs (arg0 ^. aaExpr) (arg1 ^. aaExpr), args2toN)
    setToInnerExprAction =
      maybe NoInnerExpr SetToInnerExpr $
      do
        stored <- exprPl ^. Input.mStored
        val <-
          case (filter (Lens.nullOf ExprLens.valHole) . map snd . Map.elems) fieldsI of
          [x] -> Just x
          _ -> Nothing
        valStored <- traverse (^. Input.mStored) val
        return $
          EntityId.ofValI <$>
          protectedSetToVal stored (Property.value (valStored ^. V.payload))
  BodyApply Apply
    { _aFunc = funcS
    , _aSpecialArgs = specialArgs
    , _aAnnotatedArgs = annotatedArgs
    }
    & lift . addActions exprPl
    <&> rPayload %~
      ( plData <>~ (argS ^. rPayload . plData) ) .
      ( plActions . Lens._Just . setToInnerExpr .~ setToInnerExprAction
      )
  where
    (fieldsI, Val _ (V.BLeaf V.LRecEmpty)) = RecordVal.unpack argI

convertPrefix ::
  (MonadA m, Monoid a) =>
  ExpressionU m a -> ExpressionU m a ->
  Input.Payload m a -> ConvertM m (ExpressionU m a)
convertPrefix funcS argS applyPl =
  addActions applyPl $ BodyApply Apply
  { _aFunc = funcS
  , _aSpecialArgs = ObjectArg argS
  , _aAnnotatedArgs = []
  }

unwrap ::
  MonadA m =>
  ExprIRef.ValIProperty m ->
  ExprIRef.ValIProperty m ->
  Val (Input.Payload n a) ->
  T m EntityId
unwrap outerP argP argExpr = do
  res <- DataOps.replace outerP (Property.value argP)
  return $
    case ConvertHole.orderedInnerHoles argExpr of
    (x:_) -> x ^. V.payload . Input.entityId
    _ -> EntityId.ofValI res

checkTypeMatch :: MonadA m => Type -> Type -> ConvertM m Bool
checkTypeMatch x y = do
  inferContext <- (^. ConvertM.scInferContext) <$> ConvertM.readContext
  return $ Lens.has Lens._Right $ evalStateT (Infer.run (unify x y)) inferContext

ipType :: Lens.Lens' (Input.Payload m a) Type
ipType = Input.inferred . Infer.plType

convertAppliedHole ::
  (MonadA m, Monoid a) =>
  Val (Input.Payload m a) -> ExpressionU m a -> Val (Input.Payload m a) -> Input.Payload m a ->
  MaybeT (ConvertM m) (ExpressionU m a)
convertAppliedHole funcI argS argI exprPl = do
  guard $ Lens.has ExprLens.valHole funcI
  isTypeMatch <- lift $ checkTypeMatch (argI ^. V.payload . ipType) (exprPl ^. ipType)
  let
    argWrap =
      maybe WrapNotAllowed
      (WrappedAlready . addEntityId) $
      exprPl ^. Input.mStored
    holeArg = HoleArg
      { _haExpr =
        argS
        & rPayload . plActions . Lens._Just %~
          (wrap .~ argWrap) .
          (setToHole .~ AlreadyAHole)
      , _haUnwrap =
        if isTypeMatch
        then UnwrapMAction mUnwrap
        else UnwrapTypeMismatch
      }
    mUnwrap = do
      stored <- exprPl ^. Input.mStored
      argP <- argI ^. V.payload . Input.mStored
      return $ unwrap stored argP argI
  lift $ ConvertHole.convertPlain (Just argI) exprPl
    <&> rBody . _BodyHole . holeMArg .~ Just holeArg
    <&> rPayload . plData <>~ funcI ^. V.payload . Input.userData
    <&> rPayload . plActions . Lens._Just . wrap .~
        maybe WrapNotAllowed (WrapperAlready . addEntityId) (exprPl ^. Input.mStored)
  where
    addEntityId = guidEntityId . Property.value
    guidEntityId valI = (UniqueId.toGuid valI, EntityId.ofValI valI)
