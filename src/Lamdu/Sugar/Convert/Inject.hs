module Lamdu.Sugar.Convert.Inject
    ( convert
    ) where

import qualified Lamdu.Calc.Term as V
import qualified Lamdu.Calc.Type as T
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
    T.Tag ->
    Input.Payload m a # V.Term ->
    ConvertM m (ExpressionU EvalPrep m a)
convert tag exprPl =
    do
        protectedSetToVal <- ConvertM.typeProtectedSetToVal
        let typeProtect = protectedSetToVal (exprPl ^. Input.stored) valI
        let setTag newTag =
                do
                    V.LInject newTag & V.BLeaf & ExprIRef.writeValI valI
                    void typeProtect
        ConvertTag.ref tag nameWithoutContext mempty (EntityId.ofTag entityId) setTag
            >>= ConvertM . lift
            <&> BodyInject
            >>= addActions (Const ()) exprPl
    where
        entityId = exprPl ^. Input.entityId
        valI = exprPl ^. Input.stored . ExprIRef.iref
