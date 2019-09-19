module Lamdu.Sugar.Convert.GetField
    ( convert
    ) where

import           AST (Tree, Ann(..), ann)
import qualified Control.Lens as Lens
import qualified Data.Property as Property
import qualified Lamdu.Calc.Lens as ExprLens
import qualified Lamdu.Calc.Term as V
import qualified Lamdu.Expr.IRef as ExprIRef
import           Lamdu.Sugar.Convert.Expression.Actions (addActions)
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Convert.Monad (ConvertM)
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import qualified Lamdu.Sugar.Convert.Tag as ConvertTag
import           Lamdu.Sugar.Internal
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.Types

import           Lamdu.Prelude

convertGetFieldParam ::
    (Monad m, Monoid a) =>
    Tree (Ann (Input.Payload m a)) V.GetField ->
    ConvertM m (Maybe (ExpressionU m a))
convertGetFieldParam (Ann exprPl (V.GetField recExpr tag)) =
    do
        tagParamInfos <- Lens.view (ConvertM.scScopeInfo . ConvertM.siTagParamInfos)
        do
            paramInfo <- tagParamInfos ^? Lens.ix tag . ConvertM._TagFieldParam
            param <- recExpr ^? ExprLens.valVar
            guard $ param == ConvertM.tpiFromParameters paramInfo
            GetParam ParamRef
                { _pNameRef = NameRef
                  { _nrName = nameWithContext param tag
                  , _nrGotoDefinition = ConvertM.tpiJumpTo paramInfo & pure
                  }
                , _pBinderMode = NormalBinder
                } & BodyGetVar & Just
            & Lens._Just %%~ addActions [recExpr] exprPl

convertGetFieldNonParam ::
    (Monad m, Monoid a) =>
    Tree (Ann (Input.Payload m a)) V.GetField ->
    ConvertM m (ExpressionU m a)
convertGetFieldNonParam (Ann exprPl (V.GetField recExpr tag)) =
    GetField
    <$> ConvertM.convertSubexpression recExpr
    <*> do
            protectedSetToVal <- ConvertM.typeProtectedSetToVal
            let setTag newTag =
                    do
                        V.GetField recExprI newTag & V.BGetField & ExprIRef.writeValI valI
                        protectedSetToVal recExprStored recExprI & void
            ConvertTag.ref tag nameWithoutContext mempty
                (EntityId.ofTag (exprPl ^. Input.entityId)) setTag
    <&> BodyGetField
    >>= addActions [recExpr] exprPl
    where
        valI = exprPl ^. Input.stored . Property.pVal
        recExprStored = recExpr ^. ann . Input.stored
        recExprI = recExprStored ^. Property.pVal

convert ::
    (Monad m, Monoid a) =>
    Tree (Ann (Input.Payload m a)) V.GetField ->
    ConvertM m (ExpressionU m a)
convert x =
    convertGetFieldParam x
    >>= maybe (convertGetFieldNonParam x) pure
