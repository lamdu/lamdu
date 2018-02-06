{-# LANGUAGE NoImplicitPrelude #-}

module Lamdu.Sugar.Convert.Composite
    ( convertCompositeItem, setTagOrder, makeAddItem
    ) where

import qualified Control.Lens as Lens
import qualified Data.Store.Property as Property
import           Data.Store.Transaction (Transaction)
import qualified Data.Store.Transaction as Transaction
import qualified Lamdu.Calc.Type as T
import           Lamdu.Calc.Val.Annotated (Val(..))
import qualified Lamdu.Calc.Val.Annotated as Val
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Convert.Monad (ConvertM)
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import           Lamdu.Sugar.Convert.Tag (convertTag)
import           Lamdu.Sugar.Internal
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.Types

import           Lamdu.Prelude

type T = Transaction

deleteItem ::
    Monad m =>
    ExprIRef.ValIProperty m -> ExprIRef.ValI m ->
    ConvertM m (T m EntityId)
deleteItem stored restI =
    ConvertM.typeProtectedSetToVal ?? stored ?? restI <&> Lens.mapped %~ EntityId.ofValI

convertCompositeItem ::
    (Monad m, Monoid a) =>
    (T.Tag -> ExprIRef.ValI m -> ExprIRef.ValI m -> ExprIRef.ValBody m) ->
    ExprIRef.ValIProperty m ->
    ExprIRef.ValI m ->
    EntityId -> T.Tag -> Val (Input.Payload m a) ->
    ConvertM m (CompositeItem InternalName (T m) (ExpressionU m a))
convertCompositeItem cons stored restI inst tag expr =
    do
        exprS <- ConvertM.convertSubexpression expr
        delItem <- deleteItem stored restI
        protectedSetToVal <- ConvertM.typeProtectedSetToVal
        let setTag newTag =
                do
                    cons newTag (expr ^. Val.payload . Input.stored . Property.pVal) restI
                        & ExprIRef.writeValBody valI
                    protectedSetToVal stored valI & void
                where
                    valI = stored ^. Property.pVal
        tagS <- convertTag tag mempty (EntityId.ofTag inst) setTag
        return CompositeItem
            { _ciTag = tagS
            , _ciExpr = exprS
            , _ciDelete = delItem
            }

setTagOrder :: Monad m => Int -> CompositeAddItemResult -> T m CompositeAddItemResult
setTagOrder i r =
    do
        Transaction.setP (Anchors.assocTagOrder (r ^. cairNewTag . tagVal)) i
        return r

makeAddItem :: Monad m =>
    (ExprIRef.ValI m -> T m (DataOps.CompositeExtendResult m)) ->
    ExprIRef.ValIProperty m ->
    ConvertM m (T m CompositeAddItemResult)
makeAddItem addItem stored =
    do
        protectedSetToVal <- ConvertM.typeProtectedSetToVal
        do
            DataOps.CompositeExtendResult tag newValI resultI <- addItem (stored ^. Property.pVal)
            _ <- protectedSetToVal stored resultI
            let resultEntity = EntityId.ofValI resultI
            return
                CompositeAddItemResult
                { _cairNewTag = TagInfo (EntityId.ofTag resultEntity tag) tag
                , _cairNewVal = EntityId.ofValI newValI
                , _cairItem = resultEntity
                }
            & return
