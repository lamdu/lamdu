module Lamdu.Sugar.Convert.GetField
    ( convert
    ) where

import           Control.Lens.Operators
import           Control.Monad (guard)
import           Control.MonadA (MonadA)
import qualified Data.Map as Map
import           Data.Monoid (Monoid(..))
import           Data.Store.Guid (Guid)
import           Data.Traversable (traverse)
import qualified Lamdu.Expr.Lens as ExprLens
import qualified Lamdu.Expr.UniqueId as UniqueId
import           Lamdu.Expr.Val (Val(..))
import qualified Lamdu.Expr.Val as V
import           Lamdu.Sugar.Convert.Expression.Actions (addActions)
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Convert.Monad (ConvertM)
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import           Lamdu.Sugar.Internal
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.Types

convertGetFieldParam ::
    (MonadA m, MonadA n) =>
    V.GetField (Val a) ->
    ConvertM m (Maybe (GetVar Guid n))
convertGetFieldParam (V.GetField recExpr tag) =
    do
        tagParamInfos <- ConvertM.readContext <&> (^. ConvertM.scTagParamInfos)
        do
            paramInfo <- Map.lookup tag tagParamInfos
            param <- recExpr ^? ExprLens.valVar
            guard $ param == ConvertM.tpiFromParameters paramInfo
            Just $ GetVarNamed NamedVar
                { _nvName = UniqueId.toGuid tag
                , _nvJumpTo = return (ConvertM.tpiJumpTo paramInfo)
                , _nvVarType = GetFieldParameter
                }
            & return

convertGetFieldNonParam ::
    (MonadA m, Monoid a) =>
    V.GetField (Val (Input.Payload m a)) -> EntityId ->
    ConvertM m (Body Guid m (ExpressionU m a))
convertGetFieldNonParam (V.GetField recExpr tag) entityId =
    GetField
    { _gfRecord = recExpr
    , _gfTag =
            TagG
            { _tagInstance = EntityId.ofGetFieldTag entityId
            , _tagVal = tag
            , _tagGName = UniqueId.toGuid tag
            }
    }
    & traverse ConvertM.convertSubexpression
    <&> BodyGetField

convert ::
    (MonadA m, Monoid a) =>
    V.GetField (Val (Input.Payload m a)) ->
    Input.Payload m a ->
    ConvertM m (ExpressionU m a)
convert getField exprPl =
    convertGetFieldParam getField
    >>= maybe (convertGetFieldNonParam getField entityId) (return . BodyGetVar)
    >>= addActions exprPl
    where
        entityId = exprPl ^. Input.entityId
