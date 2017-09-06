{-# LANGUAGE NoImplicitPrelude #-}

module Lamdu.Sugar.Convert.Composite
    ( convertCompositeItem
    ) where

import           Data.Store.Transaction (Transaction)
import           Data.UUID.Types (UUID)
import qualified Lamdu.Calc.Type as T
import           Lamdu.Calc.Val.Annotated (Val(..))
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.UniqueId as UniqueId
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Convert.Monad (ConvertM)
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import           Lamdu.Sugar.Internal
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.Types

import           Lamdu.Prelude

convertTag :: EntityId -> T.Tag -> Tag UUID m
convertTag inst tag =
    Tag
    { _tagInfo = TagInfo inst tag
    , _tagName = UniqueId.toUUID tag
    , _tagActions = error "TODO: tagActions"
    }

deleteItem ::
    Monad m =>
    ExprIRef.ValIProperty m -> ExprIRef.ValI m ->
    ConvertM m (Transaction m EntityId)
deleteItem stored restI =
    do
        protectedSetToVal <- ConvertM.typeProtectedSetToVal
        protectedSetToVal stored restI <&> EntityId.ofValI & return

convertCompositeItem ::
    (Monad m, Monoid a) =>
    ExprIRef.ValIProperty m ->
    ExprIRef.ValI m ->
    EntityId -> T.Tag -> Val (Input.Payload m a) ->
    ConvertM m (CompositeItem UUID m (ExpressionU m a))
convertCompositeItem stored restI inst tag expr =
    do
        exprS <- ConvertM.convertSubexpression expr
        delItem <- deleteItem stored restI
        return CompositeItem
            { _ciTag = convertTag inst tag
            , _ciExpr = exprS
            , _ciDelete = delItem
            }
