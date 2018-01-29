{-# LANGUAGE NoImplicitPrelude #-}
-- | Convert applied holes to Fragments

module Lamdu.Sugar.Convert.Fragment
    ( convertAppliedHole
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.Trans.Maybe (MaybeT(..))
import           Control.Monad.Trans.State (evalStateT, runStateT)
import           Control.Monad.Transaction (transaction)
import qualified Data.Store.Property as Property
import           Data.Store.Transaction (Transaction)
import           Data.UUID.Types (UUID)
import           Lamdu.Calc.Type (Type)
import qualified Lamdu.Calc.Val as V
import           Lamdu.Calc.Val.Annotated (Val(..))
import qualified Lamdu.Calc.Val.Annotated as Val
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.Expr.Lens as ExprLens
import qualified Lamdu.Expr.Load as Load
import qualified Lamdu.Expr.Pure as P
import qualified Lamdu.Infer as Infer
import           Lamdu.Infer.Unify (unify)
import           Lamdu.Sugar.Convert.Expression.Actions (addActions)
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
    Expression name f a ->
    Input.Payload m a ->
    [HoleOption (T m) (Expression UUID (T m) ())]
mkAppliedHoleOptions sugarContext argI argS exprPl =
    [ P.app P.hole P.hole | Lens.nullOf (rBody . _BodyLam) argS ]
    <&> ConvertHole.SeedExpr
    <&> ConvertHole.mkHoleOption sugarContext (Just argI) exprPl

mkAppliedHoleSuggesteds ::
    Monad m =>
    ConvertM.Context m ->
    Val (Input.Payload m a) ->
    Input.Payload m a ->
    T m [HoleOption (T m) (Expression UUID (T m) ())]
mkAppliedHoleSuggesteds sugarContext argI exprPl =
    Suggest.valueConversion Load.nominal Nothing (argI <&> onPl)
    <&> (`runStateT` (sugarContext ^. ConvertM.scInferContext))
    <&> Lens.mapped %~ onSuggestion
    where
        onPl pl = (pl ^. Input.inferred, Just pl)
        onSuggestion (sugg, newInferCtx) =
            ConvertHole.mkHoleOptionFromFragment
            (sugarContext & ConvertM.scInferContext .~ newInferCtx)
            exprPl (sugg <&> _1 %~ (^. Infer.plType))

checkTypeMatch :: Monad m => Type -> Type -> ConvertM m Bool
checkTypeMatch x y =
    ConvertM.readContext
    <&> (^. ConvertM.scInferContext)
    <&> evalStateT (Infer.run (unify x y))
    <&> Lens.has Lens._Right

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
        do
            sugarContext <- ConvertM.readContext
            suggesteds <-
                mkAppliedHoleSuggesteds sugarContext argI exprPl
                & transaction
            options <-
                ConvertHole.mkOptions (Just argI) exprPl
                <&> Lens.mapped %~ mappend (mkAppliedHoleOptions sugarContext argI (argS <&> (^. pUserData)) exprPl)
                <&> Lens.mapped %~ ConvertHole.addSuggestedOptions suggesteds
            BodyFragment Fragment
                { _fExpr =
                      argS
                      & rPayload . plActions . detach .~ FragmentExprAlready storedEntityId
                      & rPayload . plActions . mSetToHole ?~
                        (DataOps.setToHole (exprPl ^. Input.stored) <* postProcess <&> EntityId.ofValI)
                , _fAttach =
                      if isTypeMatch
                      then DataOps.replace (exprPl ^. Input.stored)
                           (argI ^. Val.payload . Input.stored . Property.pVal)
                           <* postProcess
                           <&> EntityId.ofValI
                           & AttachAction
                      else AttachTypeMismatch
                , _fOptions = options
                } & pure
            >>= addActions exprPl
            & lift
            <&> rPayload . plActions . detach .~ FragmentAlready storedEntityId
    where
        storedEntityId = exprPl ^. Input.stored & Property.value & EntityId.ofValI
