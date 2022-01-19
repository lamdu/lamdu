module Lamdu.Sugar.Convert.GetField
    ( convert, convertGetFieldParam
    ) where

import qualified Control.Lens as Lens
import qualified Lamdu.Calc.Lens as ExprLens
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

convertGetFieldParam ::
    (Monad m, Monoid a) =>
    V.App V.Term # Ann (Input.Payload m a) ->
    Input.Payload m a # V.Term ->
    ConvertM m (Maybe (ExpressionU v m a))
convertGetFieldParam (V.App g@(Ann _ (V.BLeaf (V.LGetField tag))) recExpr) exprPl =
    do
        recParams <- Lens.view (ConvertM.scScopeInfo . ConvertM.siRecordParams)
        do
            param <- recExpr ^? ExprLens.valVar
            tags <- recParams ^. Lens.at param
            guard (tags ^. Lens.contains tag)
            GetVar VarRef
                { _vNameRef = NameRef
                  { _nrName = nameWithContext Nothing param tag
                  , _nrGotoDefinition = EntityId.ofTaggedEntity param tag & pure
                  }
                , _vForm = GetNormalVar
                , _vVar = param
                , _vInline = CannotInline
                } & LeafGetVar & BodyLeaf & Just
            & Lens._Just %%~ addActions (App g recExpr) exprPl
convertGetFieldParam _ _= pure Nothing

convert ::
    (Monad m, Monoid a) =>
    T.Tag ->
    Input.Payload m a # V.Term ->
    ConvertM m (ExpressionU v m a)
convert tag exprPl =
    do
        protectedSetToVal <- ConvertM.typeProtectedSetToVal
        let setTag newTag =
                do
                    V.LGetField newTag & V.BLeaf & ExprIRef.writeValI valI
                    protectedSetToVal (exprPl ^. Input.stored) valI & void
        let resultInfo () = ConvertTag.TagResultInfo <$> EntityId.ofTag (exprPl ^. Input.entityId) <*> setTag
        ConvertTag.ref tag Nothing mempty (pure ()) resultInfo >>= ConvertM . lift
    <&> PfGetField <&> BodyPostfixFunc
    >>= addActions (Const ()) exprPl
    where
        valI = exprPl ^. Input.stored . ExprIRef.iref
