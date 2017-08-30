{-# LANGUAGE NoImplicitPrelude #-}
module Lamdu.Sugar.Convert.Case
    ( convert
    , convertAbsurd
    , convertAppliedCase
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.Trans.Maybe (MaybeT(..))
import           Data.Maybe.Utils (maybeToMPlus)
import qualified Data.Store.Property as Property
import           Data.Store.Transaction (Transaction)
import qualified Data.Store.Transaction as Transaction
import           Data.UUID.Types (UUID)
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Calc.Val as V
import           Lamdu.Calc.Val.Annotated (Val(..))
import qualified Lamdu.Calc.Val.Annotated as Val
import           Lamdu.Data.Anchors (assocTagOrder)
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.UniqueId as UniqueId
import           Lamdu.Sugar.Convert.Expression.Actions (addActions)
import           Lamdu.Sugar.Convert.Guard (convertGuard)
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Convert.Monad (ConvertM)
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import           Lamdu.Sugar.Internal
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.Types

import           Lamdu.Prelude

-- This is mostly a copy&paste of the Convert.Record module, yuck! DRY
-- with some abstraction?

plValI :: Lens.Lens' (Input.Payload m a) (ExprIRef.ValI m)
plValI = Input.stored . Property.pVal

convertTag :: EntityId -> T.Tag -> TagG UUID
convertTag inst tag = TagG inst tag $ UniqueId.toUUID tag

makeAddAlt :: Monad m =>
    ExprIRef.ValIProperty m ->
    ConvertM m (Transaction m CaseAddAltResult)
makeAddAlt stored =
    do
        protectedSetToVal <- ConvertM.typeProtectedSetToVal
        do
            DataOps.CaseResult tag newValI resultI <-
                DataOps.case_ (stored ^. Property.pVal)
            _ <- protectedSetToVal stored resultI
            let resultEntity = EntityId.ofValI resultI
            return
                CaseAddAltResult
                { _caarNewTag = TagG (EntityId.ofRecExtendTag resultEntity) tag ()
                , _caarNewVal = EntityId.ofValI newValI
                , _caarCase = resultEntity
                }
            & return

convertAbsurd :: Monad m => Input.Payload m a -> ConvertM m (ExpressionU m a)
convertAbsurd exprPl =
    do
        addAlt <- exprPl ^. Input.stored & makeAddAlt
        postProcess <- ConvertM.postProcess
        BodyCase Case
            { _cKind = LambdaCase
            , _cAlts = []
            , _cTail =
                    DataOps.replaceWithHole (exprPl ^. Input.stored)
                    <* postProcess
                    <&> EntityId.ofValI
                    & ClosedCase
            , _cAddAlt = addAlt
            }
            & addActions exprPl

deleteAlt ::
    Monad m =>
    ExprIRef.ValIProperty m -> ExprIRef.ValI m ->
    ConvertM m (Transaction m EntityId)
deleteAlt stored restI =
    do
        protectedSetToVal <- ConvertM.typeProtectedSetToVal
        protectedSetToVal stored restI <&> EntityId.ofValI & return

convertAlt ::
    (Monad m, Monoid a) =>
    ExprIRef.ValIProperty m -> ExprIRef.ValI m ->
    EntityId -> T.Tag -> Val (Input.Payload m a) ->
    ConvertM m (CaseAlt UUID m (ExpressionU m a))
convertAlt stored restI inst tag expr =
    do
        exprS <- ConvertM.convertSubexpression expr
        delAlt <- deleteAlt stored restI
        return CaseAlt
            { _caTag = convertTag inst tag
            , _caHandler = exprS
            , _caDelete = delAlt
            }

setTagOrder ::
    Monad m => Int -> CaseAddAltResult -> Transaction m CaseAddAltResult
setTagOrder i r =
    do
        Transaction.setP (assocTagOrder (r ^. caarNewTag . tagVal)) i
        return r

convert ::
    (Monad m, Monoid a) => V.Case (Val (Input.Payload m a)) ->
    Input.Payload m a -> ConvertM m (ExpressionU m a)
convert (V.Case tag val rest) exprPl = do
    restS <- ConvertM.convertSubexpression rest
    restCase <-
        case restS ^. rBody of
        BodyCase r | Lens.has (cKind . _LambdaCase) r ->
            return r
        _ ->
            do
                addAlt <- rest ^. Val.payload . Input.stored & makeAddAlt
                return Case
                    { _cKind = LambdaCase
                    , _cAlts = []
                    , _cTail = CaseExtending restS
                    , _cAddAlt = addAlt
                    }
    altS <-
        convertAlt
        (exprPl ^. Input.stored) (rest ^. Val.payload . plValI)
        (EntityId.ofCaseTag (exprPl ^. Input.entityId)) tag val
    restCase
        & cAlts %~ (altS:)
        & cAddAlt %~ (>>= setTagOrder (1 + length (restCase ^. cAlts)))
        & BodyCase
        & addActions exprPl

convertAppliedCase ::
    (Monad m, Monoid a) =>
    ExpressionU m a -> ExpressionU m a -> Input.Payload m a ->
    MaybeT (ConvertM m) (ExpressionU m a)
convertAppliedCase funcS argS exprPl =
    do
        caseB <- funcS ^? rBody . _BodyCase & maybeToMPlus
        Lens.has (cKind . _LambdaCase) caseB & guard
        protectedSetToVal <- lift ConvertM.typeProtectedSetToVal
        let setTo = protectedSetToVal (exprPl ^. Input.stored)
        let appliedCaseB =
                caseB
                & cKind .~ CaseWithArg
                    CaseArg
                    { _caVal = simplifyCaseArg argS
                    , _caToLambdaCase =
                        setTo (funcS ^. rPayload . plData . pStored . Property.pVal)
                        <&> EntityId.ofValI
                    }
        convertGuard setTo appliedCaseB
            & maybe (BodyCase appliedCaseB) BodyGuard
            & addActions exprPl & lift

simplifyCaseArg :: Monoid a => ExpressionU m a -> ExpressionU m a
simplifyCaseArg argS =
    case argS ^. rBody of
    BodyFromNom nom | Lens.nullOf (nVal . rBody . _BodyHole) nom -> nom ^. nVal
    _ -> argS
