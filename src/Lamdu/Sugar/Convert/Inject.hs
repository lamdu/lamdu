module Lamdu.Sugar.Convert.Inject
    ( convert
    ) where

import           Control.Monad.Once (OnceT)
import qualified Lamdu.Calc.Term as V
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Convert.Monad (ConvertM(..))
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import           Lamdu.Sugar.Convert.NodeActions (addActions)
import qualified Lamdu.Sugar.Convert.Tag as ConvertTag
import           Lamdu.Sugar.Internal
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.Types
import           Revision.Deltum.Transaction (Transaction)

import           Lamdu.Prelude

type T = Transaction

convert ::
    Monad m =>
    (TagRef InternalName (OnceT (T m)) (T m) -> Term v InternalName (OnceT (T m)) (T m) # Annotated (ConvertPayload m)) ->
    T.Tag ->
    Input.Payload m # V.Term ->
    ConvertM m (ExpressionU v m)
convert c tag exprPl =
    do
        protectedSetToVal <- ConvertM.typeProtectedSetToVal
        let typeProtect = protectedSetToVal (exprPl ^. Input.stored) valI
        let setTag newTag =
                do
                    V.LInject newTag & V.BLeaf & ExprIRef.writeValI valI
                    void typeProtect
        let resultInfo () = ConvertTag.TagResultInfo <$> EntityId.ofTag entityId <*> setTag
        ConvertTag.ref tag Nothing mempty (pure ()) resultInfo >>= ConvertM . lift
    <&> c
    >>= addActions (Ann exprPl (V.BLeaf (V.LInject tag)))
    where
        entityId = exprPl ^. Input.entityId
        valI = exprPl ^. Input.stored . ExprIRef.iref
