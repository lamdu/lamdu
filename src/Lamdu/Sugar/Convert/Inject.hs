module Lamdu.Sugar.Convert.Inject
    ( convert
    ) where

import qualified Lamdu.Calc.Term as V
import qualified Lamdu.Expr.IRef as ExprIRef
import           Lamdu.Sugar.Convert.Expression.Actions (addActions)
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Convert.Monad (ConvertM(..))
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import qualified Lamdu.Sugar.Convert.Tag as ConvertTag
import           Lamdu.Sugar.Internal
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.Types

import           Lamdu.Prelude

convert ::
    (Monad m, Monoid a) =>
    V.Inject # Ann (Input.Payload m a) ->
    Input.Payload m a # V.Term ->
    ConvertM m (ExpressionU EvalPrep m a)
convert (V.Inject tag injected) exprPl =
    do
        protectedSetToVal <- ConvertM.typeProtectedSetToVal
        let typeProtect = protectedSetToVal (exprPl ^. Input.stored) valI
        injectedS <- ConvertM.convertSubexpression injected
        let setTag newTag =
                do
                    V.Inject newTag injectedI & V.BInject & ExprIRef.writeValI valI
                    void typeProtect
        ConvertTag.ref tag nameWithoutContext mempty (EntityId.ofTag entityId) setTag
            >>= ConvertM . lift
            <&> (`Inject` injectedS) <&> BodyInject
            >>= addActions (Const ()) exprPl
    where
        entityId = exprPl ^. Input.entityId
        valI = exprPl ^. Input.stored . ExprIRef.iref
        injectedI = injected ^. hAnn . Input.stored . ExprIRef.iref
