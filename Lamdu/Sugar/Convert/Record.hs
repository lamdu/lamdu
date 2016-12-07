{-# LANGUAGE NoImplicitPrelude #-}
module Lamdu.Sugar.Convert.Record
    ( convertEmpty, convertExtend
    ) where

import           Lamdu.Prelude

import qualified Control.Lens as Lens
import           Data.Maybe.Utils (unsafeUnjust)
import qualified Data.Store.Property as Property
import           Data.Store.Transaction (Transaction)
import qualified Data.Store.Transaction as Transaction
import           Data.UUID.Types (UUID)
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Calc.Val as V
import qualified Lamdu.Calc.Val.Annotated as Val
import           Lamdu.Calc.Val.Annotated (Val(..))
import           Lamdu.Data.Anchors (assocTagOrder)
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.UniqueId as UniqueId
import           Lamdu.Sugar.Convert.Expression.Actions (addActions)
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Convert.Monad (ConvertM)
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import           Lamdu.Sugar.Internal
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.Types

plValI :: Lens.Lens' (Input.Payload m a) (ExprIRef.ValI m)
plValI = Input.stored . Property.pVal

convertTag :: EntityId -> T.Tag -> TagG UUID
convertTag inst tag = TagG inst tag $ UniqueId.toUUID tag

deleteField ::
    Monad m =>
    ExprIRef.ValIProperty m ->
    ExprIRef.ValI m ->
    Record name0 m (Expression name2 m a1) -> Val (Input.Payload m a) ->
    Expression name1 m0 a0 ->
    ConvertM m (Transaction m EntityId)
deleteField stored restI restS expr exprS =
    do
        typeProtect <- ConvertM.typeProtectTransaction
        protectedSetToVal <- ConvertM.typeProtectedSetToVal
        return $
            if null (restS ^. rItems)
            then
                case restS ^. rTail of
                ClosedRecord{}
                    | Lens.has (rBody . _BodyHole) exprS ->
                        V.BLeaf V.LRecEmpty & Val () & ExprIRef.newVal
                    | otherwise ->
                        -- When deleting closed one field record
                        -- we replace the record with the field value
                        -- (unless it is a hole)
                        expr ^. Val.payload . plValI & return
                RecordExtending{} -> return restI
                >>= protectedSetToVal stored
                <&> EntityId.ofValI
            else do
                let delete = DataOps.replace stored restI
                mResult <- typeProtect delete <&> fmap EntityId.ofValI
                case mResult of
                    Just result -> return result
                    Nothing ->
                        unsafeUnjust "should have a way to fix type error" $
                        case restS ^. rTail of
                        RecordExtending ext ->
                            ext ^? rPayload . plActions . wrap . _WrapAction
                            <&> fmap snd
                        ClosedRecord open -> delete >> open & Just

convertField ::
    (Monad m, Monoid a) =>
    ExprIRef.ValIProperty m ->
    ExprIRef.ValI m -> Record name m (ExpressionU m a) ->
    EntityId -> T.Tag -> Val (Input.Payload m a) ->
    ConvertM m (RecordField UUID m (ExpressionU m a))
convertField stored restI restS inst tag expr =
    do
        exprS <- ConvertM.convertSubexpression expr
        delField <- deleteField stored restI restS expr exprS
        return RecordField
            { _rfTag = convertTag inst tag
            , _rfExpr = exprS
            , _rfDelete = delField
            }

makeAddField :: Monad m =>
    ExprIRef.ValIProperty m ->
    ConvertM m (Transaction m RecordAddFieldResult)
makeAddField stored =
    do
        typeProtect <- ConvertM.typeProtectTransaction
        do
            mResultI <- DataOps.recExtend stored & typeProtect
            case mResultI of
                Just extendRes -> return extendRes
                Nothing ->
                    do
                        extendRes <- DataOps.recExtend stored
                        DataOps.setToWrapper (DataOps.rerResult extendRes) stored & void
                        return extendRes
                <&> result
            & return
    where
        result (DataOps.RecExtendResult tag newValI resultI) =
            RecordAddFieldResult
            { _rafrNewTag = TagG (EntityId.ofRecExtendTag resultEntity) tag ()
            , _rafrNewVal = EntityId.ofValI newValI
            , _rafrRecExtend = resultEntity
            }
            where
                resultEntity = EntityId.ofValI resultI

convertEmpty :: Monad m => Input.Payload m a -> ConvertM m (ExpressionU m a)
convertEmpty exprPl = do
    addField <- exprPl ^. Input.stored & makeAddField
    BodyRecord Record
        { _rItems = []
        , _rTail =
                exprPl ^. Input.stored
                & DataOps.replaceWithHole
                <&> EntityId.ofValI
                & ClosedRecord
        , _rAddField = addField
        }
        & addActions exprPl

setTagOrder ::
    Monad m => Int -> RecordAddFieldResult -> Transaction m RecordAddFieldResult
setTagOrder i r =
    do
        Transaction.setP (assocTagOrder (r ^. rafrNewTag . tagVal)) i
        return r

convertExtend ::
    (Monad m, Monoid a) => V.RecExtend (Val (Input.Payload m a)) ->
    Input.Payload m a -> ConvertM m (ExpressionU m a)
convertExtend (V.RecExtend tag val rest) exprPl = do
    restS <- ConvertM.convertSubexpression rest
    (restRecord, hiddenEntities) <-
        case restS ^. rBody of
        BodyRecord r -> return (r, restS ^. rPayload . plData)
        _ ->
            do
                addField <- rest ^. Val.payload . Input.stored & makeAddField
                return
                    ( Record [] (RecordExtending restS) addField
                    , mempty
                    )
    fieldS <-
        convertField
        (exprPl ^. Input.stored) (rest ^. Val.payload . plValI) restRecord
        (EntityId.ofRecExtendTag (exprPl ^. Input.entityId)) tag val
    restRecord
        & rItems %~ (fieldS:)
        & rAddField %~ (>>= setTagOrder (1 + length (restRecord ^. rItems)))
        & BodyRecord
        & addActions exprPl
        <&> rPayload . plData <>~ hiddenEntities
