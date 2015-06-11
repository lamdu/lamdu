{-# LANGUAGE NamedFieldPuns #-}
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
import qualified Data.Foldable as Foldable
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Store.Property as Property
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
        funcS <- ConvertM.convertSubexpression funcI & lift
        justToLeft $ convertLabeled funcS argS argI exprPl
        lift $ convertPrefix funcS argS exprPl

noRepetitions :: Ord a => [a] -> Bool
noRepetitions x = length x == Set.size (Set.fromList x)

convertLabeled ::
    (MonadA m, Monoid a) =>
    ExpressionU m a -> ExpressionU m a -> Val (Input.Payload m a) -> Input.Payload m a ->
    MaybeT (ConvertM m) (ExpressionU m a)
convertLabeled funcS argS argI exprPl = do
    record <- maybeToMPlus $ argS ^? rBody . _BodyRecord
    guard $ length (record ^. rItems) >= 2
    let getArg field =
            AnnotatedArg
                { _aaTag = field ^. rfTag
                , _aaExpr = field ^. rfExpr
                }
    let args = map getArg $ record ^. rItems
    let tags = args ^.. Lens.traversed . aaTag . tagVal
    unless (noRepetitions tags) $ error "Repetitions should not type-check"
    protectedSetToVal <- lift ConvertM.typeProtectedSetToVal
    let setToInnerExprAction =
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
        , _aSpecialArgs = NoSpecialArgs
        , _aAnnotatedArgs = args
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

orderedInnerHoles :: Val a -> [Val a]
orderedInnerHoles e =
    case e ^. V.body of
    V.BLeaf V.LHole -> [e]
    V.BApp (V.Apply func@(V.Val _ (V.BLeaf V.LHole)) arg) ->
        orderedInnerHoles arg ++ [func]
    body -> Foldable.concatMap orderedInnerHoles body

unwrap ::
    MonadA m =>
    ExprIRef.ValIProperty m ->
    ExprIRef.ValIProperty m ->
    Val (Input.Payload n a) ->
    T m EntityId
unwrap outerP argP argExpr = do
    res <- DataOps.replace outerP (Property.value argP)
    return $
        case orderedInnerHoles argExpr of
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
    let argWrap =
            exprPl ^. Input.mStored
            & maybe WrapNotAllowed (WrappedAlready . addEntityId)
        holeArg = HoleArg
            { _haExpr =
                  argS
                  & rPayload . plActions . Lens._Just . wrap .~ argWrap
            , _haUnwrap =
                  if isTypeMatch
                  then UnwrapMAction mUnwrap
                  else UnwrapTypeMismatch
            }
        mUnwrap =
            do
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
