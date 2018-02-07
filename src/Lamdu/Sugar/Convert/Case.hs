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
import           Lamdu.Sugar.Convert.IfElse (convertIfElse)
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Convert.Monad (ConvertM)
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import           Lamdu.Sugar.Convert.Tag (convertTagSelection)
import           Lamdu.Sugar.Internal
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import qualified Lamdu.Sugar.Lens as SugarLens
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
        let actions =
                ClosedCompositeActions
                { _closedCompositeOpen =
                    DataOps.replaceWithHole (exprPl ^. Input.stored)
                    <* postProcess
                    <&> EntityId.ofValI
                }
        addItemWithTag <-
            convertTagSelection mempty (EntityId.ofTag (exprPl ^. Input.entityId)) addAlt
            <&> Lens.mapped %~ (^. cairNewVal)
        BodyCase Case
            { _cKind = LambdaCase
            , _cBody = Composite
                { _cItems = []
                , _cTail = ClosedComposite actions
                , _cAddItem = DataOps.genNewTag >>= addAlt
                , _cAddItemWithTag = addItemWithTag
                }
            }
            & addActions exprPl

convert ::
    (Monad m, Monoid a) => V.Case (Val (Input.Payload m a)) ->
    Input.Payload m a -> ConvertM m (ExpressionU m a)
convert (V.Case tag val rest) exprPl = do
    restS <- ConvertM.convertSubexpression rest
    (restCase, modifyEntityId) <-
        case restS ^. rBody of
        BodyCase r | Lens.has (cKind . _LambdaCase) r ->
            return (r, const (restS ^. rPayload . plEntityId))
        _ ->
            do
                addAlt <- makeAddItem DataOps.case_ restStored
                protectedSetToVal <- ConvertM.typeProtectedSetToVal
                let actions =
                        OpenCompositeActions
                        { _openCompositeClose =
                            ExprIRef.newValBody (V.BLeaf V.LAbsurd)
                            >>= protectedSetToVal restStored
                            <&> EntityId.ofValI
                        }
                addItemWithTag <-
                    convertTagSelection mempty (EntityId.ofTag (exprPl ^. Input.entityId)) addAlt
                    <&> Lens.mapped %~ (^. cairNewVal)
                return
                    ( Case
                        { _cKind = LambdaCase
                        , _cBody = Composite
                            { _cItems = []
                            , _cTail = OpenComposite actions restS
                            , _cAddItem = DataOps.genNewTag >>= addAlt
                            , _cAddItemWithTag = addItemWithTag
                            }
                        }
                    , id)
    altS <-
        convertCompositeItem
        (V.Case <&> Lens.mapped . Lens.mapped %~ V.BCase)
        (exprPl ^. Input.stored) (rest ^. Val.payload . plValI)
        (exprPl ^. Input.entityId) tag val
    restCase
        & cBody . cItems %~ (altS:)
        & cBody . cAddItem %~ (>>= setTagOrder (1 + length (restCase ^. cBody . cItems)))
        & BodyCase
        & addActions exprPl
        <&> rPayload . plEntityId %~ modifyEntityId
    where
        restStored = rest ^. Val.payload . Input.stored

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
        convertIfElse setTo appliedCaseB
            & maybe (BodyCase appliedCaseB) BodyIfElse
            & addActions exprPl & lift
            <&> rPayload . plEntityId .~ funcS ^. rPayload . plEntityId

simplifyCaseArg :: Monoid a => ExpressionU m a -> ExpressionU m a
simplifyCaseArg argS =
    case argS ^. rBody of
    BodyFromNom nom | Lens.nullOf (nVal . rBody . SugarLens.bodyUnfinished) nom -> nom ^. nVal
    _ -> argS
