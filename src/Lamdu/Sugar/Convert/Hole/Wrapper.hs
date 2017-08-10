{-# LANGUAGE NoImplicitPrelude #-}
-- | Convert wrapper holes

module Lamdu.Sugar.Convert.Hole.Wrapper
    ( convertAppliedHole
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.Trans.Maybe (MaybeT(..))
import           Control.Monad.Trans.State (evalStateT, runStateT)
import           Control.Monad.Transaction (transaction)
import qualified Data.Foldable as Foldable
import qualified Data.Store.Property as Property
import           Data.Store.Transaction (Transaction)
import           Data.UUID.Types (UUID)
import           Lamdu.Calc.Type (Type)
import qualified Lamdu.Calc.Val as V
import           Lamdu.Calc.Val.Annotated (Val(..))
import qualified Lamdu.Calc.Val.Annotated as Val
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.Lens as ExprLens
import qualified Lamdu.Expr.Load as Load
import qualified Lamdu.Expr.Pure as P
import qualified Lamdu.Expr.UniqueId as UniqueId
import qualified Lamdu.Infer as Infer
import           Lamdu.Infer.Unify (unify)
import qualified Lamdu.Sugar.Convert.Hole as ConvertHole
import qualified Lamdu.Sugar.Convert.Hole.Suggest as Suggest
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Convert.Monad (ConvertM)
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import           Lamdu.Sugar.Internal
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.Types

import           Lamdu.Prelude

type T = Transaction

mkAppliedHoleOptions ::
    Monad m =>
    ConvertM.Context m ->
    Val (Input.Payload m a) ->
    Expression name m a ->
    Input.Payload m a ->
    ExprIRef.ValIProperty m ->
    [HoleOption UUID m]
mkAppliedHoleOptions sugarContext argI argS exprPl stored =
    [ P.app P.hole P.hole | Lens.nullOf (rBody . _BodyLam) argS ]
    <&> ConvertHole.SeedExpr
    <&> ConvertHole.mkHoleOption sugarContext (Just argI) exprPl stored

mkAppliedHoleSuggesteds ::
    Monad m =>
    ConvertM.Context m ->
    Val (Input.Payload m a) ->
    Input.Payload m a ->
    ExprIRef.ValIProperty m ->
    T m [HoleOption UUID m]
mkAppliedHoleSuggesteds sugarContext argI exprPl stored =
    Suggest.valueConversion Load.nominal Nothing (argI <&> onPl)
    <&> (`runStateT` (sugarContext ^. ConvertM.scInferContext))
    <&> Lens.mapped %~ onSuggestion
    where
        onPl pl = (pl ^. Input.inferred, Just pl)
        onSuggestion (sugg, newInferCtx) =
            ConvertHole.mkHoleOptionFromInjected
            (sugarContext & ConvertM.scInferContext .~ newInferCtx)
            exprPl stored
            (sugg <&> _1 %~ (^. Infer.plType))

orderedInnerHoles :: Val a -> [Val a]
orderedInnerHoles e =
    case e ^. Val.body of
    V.BLeaf V.LHole -> [e]
    V.BApp (V.Apply func@(Val _ (V.BLeaf V.LHole)) arg) ->
        orderedInnerHoles arg ++ [func]
    body -> Foldable.concatMap orderedInnerHoles body

checkTypeMatch :: Monad m => Type -> Type -> ConvertM m Bool
checkTypeMatch x y =
    do
        inferContext <- (^. ConvertM.scInferContext) <$> ConvertM.readContext
        return $ Lens.has Lens._Right $ evalStateT (Infer.run (unify x y)) inferContext

unwrap ::
    Monad m =>
    ExprIRef.ValIProperty m ->
    ExprIRef.ValIProperty m ->
    Val (Input.Payload n a) ->
    T m EntityId
unwrap outerP argP argExpr =
    do
        res <- DataOps.replace outerP (Property.value argP)
        return $
            case orderedInnerHoles argExpr of
            (x:_) -> x ^. Val.payload . Input.entityId
            _ -> EntityId.ofValI res

convertAppliedHole ::
    (Monad m, Monoid a) =>
    V.Apply (Val (Input.Payload m a)) -> ExpressionU m a ->
    Input.Payload m a ->
    MaybeT (ConvertM m) (ExpressionU m a)
convertAppliedHole (V.Apply funcI argI) argS exprPl =
    do
        guard $ Lens.has ExprLens.valHole funcI
        isTypeMatch <-
            checkTypeMatch (argI ^. Val.payload . Input.inferredType)
            (exprPl ^. Input.inferredType) & lift
        postProcess <- lift ConvertM.postProcess
        let holeArg = HoleArg
                { _haExpr =
                      argS
                      & rPayload . plActions . wrap .~ WrappedAlready storedEntityId
                      & rPayload . plActions . setToHole .~
                        SetWrapperToHole
                        ( exprPl ^. Input.stored & DataOps.setToHole <&> uuidEntityId )
                , _haUnwrap =
                      if isTypeMatch
                      then unwrap (exprPl ^. Input.stored)
                           (argI ^. Val.payload . Input.stored) argI
                           <* postProcess
                           & UnwrapAction
                      else UnwrapTypeMismatch
                }
        do
            sugarContext <- ConvertM.readContext
            hole <- ConvertHole.convertCommon (Just argI) exprPl
            suggesteds <-
                mkAppliedHoleSuggesteds sugarContext
                argI exprPl (exprPl ^. Input.stored)
                & transaction
            hole
                & rBody . _BodyHole . holeActions . holeOptions . Lens.mapped
                    %~  ConvertHole.addSuggestedOptions suggesteds
                    .   mappend (mkAppliedHoleOptions sugarContext
                        argI (argS <&> (^. pUserData)) exprPl (exprPl ^. Input.stored))
                & return
            & lift
            <&> rBody . _BodyHole . holeMArg .~ Just holeArg
            <&> rPayload . plData . pUserData <>~ funcI ^. Val.payload . Input.userData
            <&> rPayload . plActions . wrap .~ WrapperAlready storedEntityId
    where
        storedEntityId = exprPl ^. Input.stored & Property.value & uuidEntityId
        uuidEntityId valI = (UniqueId.toUUID valI, EntityId.ofValI valI)
