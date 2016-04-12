{-# LANGUAGE NoImplicitPrelude #-}
module Lamdu.Sugar.Convert.GetField
    ( convert
    ) where

import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Monad (guard)
import qualified Lamdu.Expr.Lens as ExprLens
import qualified Lamdu.Expr.UniqueId as UniqueId
import qualified Lamdu.Expr.Val as V
import           Lamdu.Expr.Val.Annotated (Val(..))
import           Lamdu.Sugar.Convert.Expression.Actions (addActions, addActionsWithSetToInner)
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Convert.Monad (ConvertM)
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import           Lamdu.Sugar.Internal
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.Types

import           Prelude.Compat

convertGetFieldParam ::
    Monad m =>
    V.GetField (Val a) -> Input.Payload m b ->
    ConvertM m (Maybe (ExpressionU m b))
convertGetFieldParam (V.GetField recExpr tag) exprPl =
    do
        tagParamInfos <- ConvertM.readContext <&> (^. ConvertM.scScopeInfo . ConvertM.siTagParamInfos)
        do
            paramInfo <- tagParamInfos ^? Lens.ix tag . ConvertM._TagFieldParam
            param <- recExpr ^? ExprLens.valVar
            guard $ param == ConvertM.tpiFromParameters paramInfo
            GetParam Param
                { _pNameRef = NameRef
                  { _nrName = UniqueId.toUUID tag
                  , _nrGotoDefinition = return (ConvertM.tpiJumpTo paramInfo)
                  }
                , _pForm = GetFieldParameter
                , _pBinderMode = NormalBinder
                } & BodyGetVar & Just
            & Lens._Just %%~ addActions exprPl

convertGetFieldNonParam ::
    (Monad m, Monoid a) =>
    V.GetField (Val (Input.Payload m a)) -> Input.Payload m a ->
    ConvertM m (ExpressionU m a)
convertGetFieldNonParam (V.GetField recExpr tag) exprPl =
    GetField
    { _gfRecord = recExpr
    , _gfTag =
        TagG
        { _tagInstance = EntityId.ofGetFieldTag entityId
        , _tagVal = tag
        , _tagGName = UniqueId.toUUID tag
        }
    }
    & traverse ConvertM.convertSubexpression
    <&> BodyGetField
    >>= addActionsWithSetToInner exprPl recExpr
    where
        entityId = exprPl ^. Input.entityId

convert ::
    (Monad m, Monoid a) =>
    V.GetField (Val (Input.Payload m a)) ->
    Input.Payload m a ->
    ConvertM m (ExpressionU m a)
convert getField exprPl =
    convertGetFieldParam getField exprPl
    >>= maybe (convertGetFieldNonParam getField exprPl) return
