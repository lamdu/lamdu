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
import qualified Lamdu.Calc.Val as V
import           Lamdu.Calc.Val.Annotated (Val(..))
import qualified Lamdu.Calc.Val.Annotated as Val
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.Expr.IRef as ExprIRef
import           Lamdu.Sugar.Convert.Composite (convertCompositeItem, setTagOrder, makeAddItem)
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

convertAbsurd :: Monad m => Input.Payload m a -> ConvertM m (ExpressionU m a)
convertAbsurd exprPl =
    do
        addAlt <- exprPl ^. Input.stored & makeAddItem DataOps.case_
        postProcess <- ConvertM.postProcess
        BodyCase Case
            { _cKind = LambdaCase
            , _cBody = Composite
                { _cItems = []
                , _cTail =
                        DataOps.replaceWithHole (exprPl ^. Input.stored)
                        <* postProcess
                        <&> EntityId.ofValI
                        & ClosedComposite
                , _cAddItem = addAlt
                }
            }
            & addActions exprPl

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
                addAlt <- rest ^. Val.payload . Input.stored & makeAddItem DataOps.case_
                return Case
                    { _cKind = LambdaCase
                    , _cBody = Composite
                        { _cItems = []
                        , _cTail = CompositeExtending restS
                        , _cAddItem = addAlt
                        }
                    }
    altS <-
        convertCompositeItem
        (exprPl ^. Input.stored) (rest ^. Val.payload . plValI)
        (EntityId.ofCaseTag (exprPl ^. Input.entityId)) tag val
    restCase
        & cBody . cItems %~ (altS:)
        & cBody . cAddItem %~ (>>= setTagOrder (1 + length (restCase ^. cBody . cItems)))
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
