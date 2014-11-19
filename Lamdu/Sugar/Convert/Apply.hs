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
import Data.Traversable (traverse)
import Lamdu.Data.Anchors (PresentationMode(..))
import Lamdu.Expr.Type (Type)
import Lamdu.Expr.Val (Val(..))
import Lamdu.Infer.Unify (unify)
import Lamdu.Sugar.Convert.Monad (ConvertM)
import Lamdu.Sugar.Internal
import Lamdu.Sugar.Types
import Lamdu.Sugar.Types.Internal
import qualified Control.Lens as Lens
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Store.Property as Property
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.Lens as ExprLens
import qualified Lamdu.Expr.RecordVal as RecordVal
import qualified Lamdu.Expr.Val as V
import qualified Lamdu.Infer as Infer
import qualified Lamdu.Sugar.Convert.Expression as ConvertExpr
import qualified Lamdu.Sugar.Convert.Hole as ConvertHole
import qualified Lamdu.Sugar.Convert.List as ConvertList
import qualified Lamdu.Sugar.Convert.Monad as ConvertM

convert ::
  (MonadA m, Monoid a) => V.Apply (InputExpr m a) ->
  InputPayload m a -> ConvertM m (ExpressionU m a)
convert app@(V.Apply funcI argI) exprPl =
  runMatcherT $ do
    argS <- lift $ ConvertM.convertSubexpression argI
    justToLeft $ convertAppliedHole funcI argS argI exprPl
    justToLeft $ ConvertList.cons app argS exprPl
    funcS <- lift $ ConvertM.convertSubexpression funcI
    justToLeft $ convertLabeled funcS argS argI exprPl
    lift $ convertPrefix funcS argS exprPl

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
  (MonadA m, Monoid a) =>
  ExpressionU m a -> ExpressionU m a -> InputExpr m a -> InputPayload m a ->
  MaybeT (ConvertM m) (ExpressionU m a)
convertLabeled funcS argS argI exprPl = do
  record <- maybeToMPlus $ argS ^? rBody . _BodyRecord
  guard $ length (record ^. rItems) >= 2
  let
    getArg field =
      AnnotatedArg
        { _aaTag = field ^. rfTag
        , _aaTagExprGuid = field ^. rfTag . tagInstance
        , _aaExpr = field ^. rfExpr
        }
  let args@(arg0 : args1toN@(arg1 : args2toN)) = map getArg $ record ^. rItems
  let tags = args ^.. Lens.traversed . aaTag . tagVal
  unless (noRepetitions tags) $ error "Repetitions should not type-check"
  presentationMode <- MaybeT $ indirectDefinitionPresentationMode funcS
  let
    (specialArgs, annotatedArgs) =
      case presentationMode of
      Verbose -> (NoSpecialArgs, args)
      OO -> (ObjectArg (arg0 ^. aaExpr), args1toN)
      Infix -> (InfixArgs (arg0 ^. aaExpr) (arg1 ^. aaExpr), args2toN)
    setToInnerExprAction =
      maybe NoInnerExpr SetToInnerExpr $
      do
        stored <- exprPl ^. ipStored
        val <-
          case (filter (Lens.nullOf ExprLens.valHole) . map snd . Map.elems) fieldsI of
          [x] -> Just x
          _ -> Nothing
        valStored <- traverse (^. ipStored) val
        return $
          ExprIRef.valIGuid <$>
          DataOps.setToWrapper (Property.value (valStored ^. V.payload)) stored
  BodyApply Apply
    { _aFunc = funcS
    , _aSpecialArgs = specialArgs
    , _aAnnotatedArgs = annotatedArgs
    }
    & lift . ConvertExpr.make exprPl
    <&> rPayload %~
      ( plData <>~ (argS ^. rPayload . plData) ) .
      ( plActions . Lens._Just . setToInnerExpr .~ setToInnerExprAction
      )
  where
    (fieldsI, Val _ (V.BLeaf V.LRecEmpty)) = RecordVal.unpack argI

convertPrefix ::
  (MonadA m, Monoid a) =>
  ExpressionU m a -> ExpressionU m a ->
  InputPayload m a -> ConvertM m (ExpressionU m a)
convertPrefix funcS argS applyPl =
  ConvertExpr.make applyPl $ BodyApply Apply
  { _aFunc = funcS
  , _aSpecialArgs = ObjectArg argS
  , _aAnnotatedArgs = []
  }

unwrap ::
  MonadA m =>
  ExprIRef.ValIProperty m ->
  ExprIRef.ValIProperty m ->
  InputExpr def stored ->
  T m Guid
unwrap outerP argP argExpr = do
  res <- DataOps.replace outerP (Property.value argP)
  return $
    case orderedHoles of
    (x:_) -> x ^. V.payload . Lens._1
    _ -> ExprIRef.valIGuid res
  where
    f x =
      ( x ^. ipGuid
      , x ^. ipInferred
      )
    orderedHoles = ConvertHole.orderedInnerHoles $ f <$> argExpr

checkTypeMatch :: MonadA m => Type -> Type -> ConvertM m Bool
checkTypeMatch x y = do
  inferContext <- (^. ConvertM.scInferContext) <$> ConvertM.readContext
  return $ Lens.has Lens._Right $ evalStateT (Infer.run (unify x y)) inferContext

ipType :: Lens.Lens' (InputPayload m a) Type
ipType = ipInferred . Infer.plType

convertAppliedHole ::
  (MonadA m, Monoid a) =>
  InputExpr m a -> ExpressionU m a -> InputExpr m a -> InputPayload m a ->
  MaybeT (ConvertM m) (ExpressionU m a)
convertAppliedHole funcI argS argI exprPl = do
  guard $ Lens.has ExprLens.valHole funcI
  isTypeMatch <- lift $ checkTypeMatch (argI ^. V.payload . ipType) (exprPl ^. ipType)
  let
    argWrap =
      maybe WrapNotAllowed
      (WrappedAlready . ExprIRef.valIGuid . Property.value) $
      exprPl ^. ipStored
    holeArg = HoleArg
      { _haExpr =
        argS
        & rPayload . plActions . Lens._Just %~
          (wrap .~ argWrap) .
          (setToHole .~ AlreadyAHole)
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
      argP <- argI ^. V.payload . ipStored
      return $ unwrap stored argP argI
  lift $ ConvertHole.convertPlain exprPl
    <&> rBody . _BodyHole . holeMArg .~ Just holeArg
    <&> rPayload . plData <>~ funcI ^. V.payload . ipData
    <&> rPayload . plActions . Lens._Just . wrap .~ WrapperAlready
