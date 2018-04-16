module Lamdu.Sugar.Convert.Case
    ( convert
    , convertAbsurd
    , convertAppliedCase
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.Trans.Maybe (MaybeT(..))
import           Data.Maybe.Utils (maybeToMPlus)
import qualified Data.Property as Property
import qualified Lamdu.Calc.Val as V
import           Lamdu.Calc.Val.Annotated (Val(..))
import qualified Lamdu.Calc.Val.Annotated as Val
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.Expr.IRef as ExprIRef
import           Lamdu.Sugar.Convert.Composite (convertEmptyComposite, convertCompositeExtend, convertOneItemOpenComposite)
import           Lamdu.Sugar.Convert.Expression.Actions (addActions)
import           Lamdu.Sugar.Convert.IfElse (convertIfElse)
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Convert.Monad (ConvertM)
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import           Lamdu.Sugar.Internal
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import qualified Lamdu.Sugar.Lens as SugarLens
import           Lamdu.Sugar.Types

import           Lamdu.Prelude

-- This is mostly a copy&paste of the Convert.Record module, yuck! DRY
-- with some abstraction?

plValI :: Lens.Lens' (Input.Payload m a) (ExprIRef.ValI m)
plValI = Input.stored . Property.pVal

convertAbsurd :: (Monad m, Monoid a) => Input.Payload m a -> ConvertM m (ExpressionU m a)
convertAbsurd pl =
    convertEmptyComposite DataOps.case_ pl
    <&> Case LambdaCase
    <&> BodyCase
    >>= addActions [] pl

convert ::
    (Monad m, Monoid a) => V.Case (Val (Input.Payload m a)) ->
    Input.Payload m a -> ConvertM m (ExpressionU m a)
convert caseV exprPl =
    do
        V.Case tag valS restS <- traverse ConvertM.convertSubexpression caseV
        let caseP =
                ( tag
                , caseV ^. V.caseMatch . Val.payload . plValI
                , caseV ^. V.caseMismatch . Val.payload
                )
        (modifyEntityId, cas_) <-
            case restS ^. rBody of
            BodyCase r | Lens.has (cKind . _LambdaCase) r ->
                r & cBody %%~ convertCompositeExtend mkCase DataOps.case_ valS exprPl caseP
                <&> (,) (const (restS ^. rPayload . plEntityId))
            _ ->
                convertOneItemOpenComposite V.LAbsurd mkCase DataOps.case_ valS restS exprPl caseP
                <&> Case LambdaCase
                <&> (,) id
        BodyCase cas_
            & addActions caseV exprPl
            <&> rPayload . plEntityId %~ modifyEntityId
    where
        mkCase t v r = V.Case t v r & V.BCase

convertAppliedCase ::
    (Monad m, Foldable f, Monoid a) =>
    f (Val (Input.Payload m a)) ->
    ExpressionU m a -> ExpressionU m a -> Input.Payload m a ->
    MaybeT (ConvertM m) (ExpressionU m a)
convertAppliedCase subexprs funcS argS exprPl =
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
            & addActions subexprs exprPl & lift
            <&> rPayload . plEntityId .~ funcS ^. rPayload . plEntityId

simplifyCaseArg :: ExpressionU m a -> ExpressionU m a
simplifyCaseArg argS =
    case argS ^. rBody of
    BodyFromNom nom | Lens.nullOf (nVal . rBody . SugarLens.bodyUnfinished) nom -> nom ^. nVal
    _ -> argS
