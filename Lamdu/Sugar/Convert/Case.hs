{-# LANGUAGE NoImplicitPrelude #-}
module Lamdu.Sugar.Convert.Case
    ( convert
    , convertAbsurd
    ) where

import           Prelude.Compat

import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Monad (void)
import           Control.MonadA (MonadA)
import           Data.Maybe.Utils (unsafeUnjust)
import           Data.Store.Guid (Guid)
import qualified Data.Store.Property as Property
import           Data.Store.Transaction (Transaction)
import qualified Data.Store.Transaction as Transaction
import           Lamdu.Data.Anchors (assocTagOrder)
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.Type as T
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

-- This is mostly a copy&paste of the Convert.Record module, yuck! DRY
-- with some abstraction?

plValI :: Lens.Traversal' (Input.Payload m a) (ExprIRef.ValI m)
plValI = Input.mStored . Lens._Just . Property.pVal

convertTag :: EntityId -> T.Tag -> TagG Guid
convertTag inst tag = TagG inst tag $ UniqueId.toGuid tag

makeAddAlt :: MonadA m =>
    ExprIRef.ValIProperty m ->
    ConvertM m (Transaction m CaseAddAltResult)
makeAddAlt stored =
    do
        typeProtect <- ConvertM.typeProtectTransaction
        do
            mResultI <- DataOps.case_ stored & typeProtect
            case mResultI of
                Just caseRes -> return caseRes
                Nothing ->
                    do
                        caseRes <- DataOps.case_ stored
                        DataOps.setToWrapper (DataOps.crResult caseRes) stored & void
                        return caseRes
                <&> result
            & return
    where
        result (DataOps.CaseResult tag newValI resultI) =
            CaseAddAltResult
            { _caarNewTag = TagG (EntityId.ofRecExtendTag resultEntity) tag ()
            , _caarNewVal = EntityId.ofValI newValI
            , _caarCase = resultEntity
            }
            where
                resultEntity = EntityId.ofValI resultI

convertAbsurd :: MonadA m => Input.Payload m a -> ConvertM m (ExpressionU m a)
convertAbsurd exprPl = do
    mAddAlt <- exprPl ^. Input.mStored & Lens._Just %%~ makeAddAlt
    BodyCase Case
        { _cKind = LambdaCase
        , _cAlts = []
        , _cTail =
                exprPl ^. Input.mStored
                <&> DataOps.replaceWithHole
                <&> Lens.mapped %~ EntityId.ofValI
                & ClosedCase
        , _cMAddAlt = mAddAlt
        , _cEntityId = exprPl ^. Input.entityId
        }
        & addActions exprPl

deleteAlt ::
    MonadA m =>
    Maybe (ExprIRef.ValIProperty m) ->
    Maybe (ExprIRef.ValI m) ->
    Case name0 m (Expression name2 m a1) -> Val (Input.Payload m a) ->
    Expression name1 m0 a0 ->
    ConvertM m (Maybe (Transaction m EntityId))
deleteAlt mStored mRestI restS expr exprS =
    do
        typeProtect <- ConvertM.typeProtectTransaction
        protectedSetToVal <- ConvertM.typeProtectedSetToVal
        return $ do
            stored <- mStored
            restI <- mRestI
            exprI <- expr ^? V.payload . plValI
            Just $
                if null (restS ^. cAlts)
                then
                    case restS ^. cTail of
                    ClosedCase{}
                        | Lens.has (rBody . _BodyHole) exprS ->
                                ExprIRef.newVal $ Val () $ V.BLeaf V.LRecEmpty
                        | otherwise ->
                                -- When deleting closed one alt case
                                -- we replace the case with the alt value
                                -- (unless it is a hole)
                                return exprI
                    CaseExtending{} -> return restI
                    >>= protectedSetToVal stored
                    <&> EntityId.ofValI
                else do
                    let delete = DataOps.replace stored restI
                    mResult <- typeProtect delete <&> fmap EntityId.ofValI
                    case mResult of
                        Just result -> return result
                        Nothing ->
                            unsafeUnjust "should have a way to fix type error" $
                            case restS ^. cTail of
                            CaseExtending ext ->
                                ext ^? rPayload . plActions . Lens._Just . wrap . _WrapAction
                                <&> fmap snd
                            ClosedCase mOpen -> mOpen <&> (delete >>)

convertAlt ::
    (MonadA m, Monoid a) =>
    Maybe (ExprIRef.ValIProperty m) ->
    Maybe (ExprIRef.ValI m) -> Case name m (ExpressionU m a) ->
    EntityId -> T.Tag -> Val (Input.Payload m a) ->
    ConvertM m (CaseAlt Guid m (ExpressionU m a))
convertAlt mStored mRestI restS inst tag expr =
    do
        exprS <- ConvertM.convertSubexpression expr
        delAlt <- deleteAlt mStored mRestI restS expr exprS
        return CaseAlt
            { _caTag = convertTag inst tag
            , _caHandler = exprS
            , _caMDelete = delAlt
            }

setTagOrder ::
    MonadA m => Int -> CaseAddAltResult -> Transaction m CaseAddAltResult
setTagOrder i r =
    do
        Transaction.setP (assocTagOrder (r ^. caarNewTag . tagVal)) i
        return r

convert ::
    (MonadA m, Monoid a) => V.Case (Val (Input.Payload m a)) ->
    Input.Payload m a -> ConvertM m (ExpressionU m a)
convert (V.Case tag val rest) exprPl = do
    restS <- ConvertM.convertSubexpression rest
    (restCase, hiddenEntities) <-
        case restS ^. rBody of
        BodyCase r -> return (r, restS ^. rPayload . plData)
        _ ->
            do
                mAddAlt <- rest ^. V.payload . Input.mStored & Lens._Just %%~ makeAddAlt
                return
                    ( Case
                        { _cKind = LambdaCase
                        , _cAlts = []
                        , _cTail = CaseExtending restS
                        , _cMAddAlt = mAddAlt
                        , _cEntityId = exprPl ^. Input.entityId
                        }
                    , mempty
                    )
    altS <-
        convertAlt
        (exprPl ^. Input.mStored) (rest ^? V.payload . plValI) restCase
        (EntityId.ofCaseTag (exprPl ^. Input.entityId)) tag val
    restCase
        & cAlts %~ (altS:)
        & cMAddAlt . Lens._Just %~ (>>= setTagOrder (1 + length (restCase ^. cAlts)))
        & BodyCase
        & addActions exprPl
        <&> rPayload . plData <>~ hiddenEntities
