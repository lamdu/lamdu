-- | Convert applied holes to Fragments

module Lamdu.Sugar.Convert.Fragment
    ( convertAppliedHole
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.Trans.Maybe (MaybeT(..))
import           Hyper
import           Hyper.Unify (unify, applyBindings)
import           Hyper.Unify.Binding (UVar)
import           Lamdu.Calc.Infer (runPureInfer)
import qualified Lamdu.Calc.Lens as ExprLens
import qualified Lamdu.Calc.Term as V
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Data.Ops as DataOps
import           Lamdu.Expr.IRef (iref)
import qualified Lamdu.Sugar.Config as Config
import qualified Lamdu.Sugar.Convert.Expression.Actions as Actions
import           Lamdu.Sugar.Convert.Fragment.Heal (healMismatch)
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Convert.Monad (ConvertM)
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import           Lamdu.Sugar.Internal
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.Types

import           Lamdu.Prelude

checkTypeMatch :: Monad m => UVar # T.Type -> UVar # T.Type -> ConvertM m Bool
checkTypeMatch x y =
    Lens.view ConvertM.scInferContext
    <&> check
    <&> Lens.has Lens._Right
    where
        check ctx =
            unify x y >> applyBindings x
            & runPureInfer V.emptyScope ctx

convertAppliedHole ::
    (Monad m, Monoid a) =>
    V.App V.Term # Ann (Input.Payload m a) ->
    Input.Payload m a # V.Term ->
    ExpressionU EvalPrep m a ->
    MaybeT (ConvertM m) (ExpressionU EvalPrep m a)
convertAppliedHole app@(V.App funcI argI) exprPl argS =
    do
        Lens.view (ConvertM.scConfig . Config.sugarsEnabled . Config.fragment) >>= guard
        guard (Lens.has ExprLens.valHole funcI)
        do
            isTypeMatch <-
                checkTypeMatch (argI ^. hAnn . Input.inferredTypeUVar)
                (exprPl ^. Input.inferredTypeUVar)
            postProcess <- ConvertM.postProcessAssert
            healMis <- healMismatch
            typeMismatch <-
                if isTypeMatch
                then pure Nothing
                else
                    Actions.makeTypeAnnotation
                        (EntityId.ofFragmentArg (argPl ^. Input.entityId))
                        (argPl ^. Input.inferredType) <&> Just
            BodyFragment Fragment
                { _fExpr =
                    argS
                    & annotation . pActions . detach .~ FragmentedAlready storedEntityId
                    & annotation . pActions . delete .~
                        SetToHole
                        (DataOps.setToHole stored <* postProcess <&> EntityId.ofValI)
                , _fHeal =
                    ( if isTypeMatch
                        then DataOps.replace stored argIRef <* postProcess
                        else argIRef <$ healMis (stored ^. iref)
                    )
                    <&> EntityId.ofValI
                , _fTypeMismatch = typeMismatch
                } & pure
            >>= Actions.addActions app exprPl
            & lift
        <&> annotation . pActions . detach .~ FragmentedAlready storedEntityId
    where
        argPl = argS ^. annotation . pInput
        argIRef = argI ^. hAnn . Input.stored . iref
        stored = exprPl ^. Input.stored
        storedEntityId = stored ^. iref & EntityId.ofValI
