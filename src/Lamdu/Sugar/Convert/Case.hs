module Lamdu.Sugar.Convert.Case
    ( convert
    , convertAbsurd
    , convertAppliedCase
    ) where

import           AST (Tree)
import           AST.Knot.Ann (Ann, ann, val)
import           AST.Term.Row (RowExtend(..))
import qualified Control.Lens as Lens
import           Control.Monad.Trans.Maybe (MaybeT(..))
import           Data.Maybe.Extended (maybeToMPlus)
import qualified Data.Property as Property
import qualified Lamdu.Calc.Term as V
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Sugar.Convert.Composite as Composite
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
    Composite.convertEmpty DataOps.case_ pl
    <&> Case LambdaCase
    <&> BodyCase
    >>= addActions [] pl

_CaseThatIsLambdaCase ::
    Lens.Prism' (Tree (Case name i o) k) (Tree (Composite name i o) k)
_CaseThatIsLambdaCase =
    Lens.prism' (Case LambdaCase) $ \case
    Case LambdaCase x -> Just x
    _ -> Nothing

convert ::
    (Monad m, Monoid a) =>
    Tree (RowExtend T.Tag V.Term V.Term) (Ann (Input.Payload m a)) ->
    Input.Payload m a ->
    ConvertM m (ExpressionU m a)
convert (RowExtend tag v rest) exprPl =
    do
        valS <-
            ConvertM.convertSubexpression v
            <&> val . _BodyLam . lamApplyLimit .~ AtMostOneFuncApply
        restS <- ConvertM.convertSubexpression rest
        let caseP =
                Composite.ExtendVal
                { Composite._extendTag = tag
                , Composite._extendValI = v ^. ann . plValI
                , Composite._extendRest = rest ^. ann
                }
        Composite.convert DataOps.case_ V.LAbsurd mkCase (_BodyCase . _CaseThatIsLambdaCase) valS restS
            exprPl caseP
    where
        mkCase t c r = RowExtend t c r & V.BCase

convertAppliedCase ::
    (Monad m, Monoid a) =>
    Tree (V.Apply V.Term) (Ann (Input.Payload m a)) ->
    ExpressionU m a -> ExpressionU m a -> Input.Payload m a ->
    MaybeT (ConvertM m) (ExpressionU m a)
convertAppliedCase (V.Apply _ arg) funcS argS exprPl =
    do
        caseB <- funcS ^? val . _BodyCase & maybeToMPlus
        Lens.has (cKind . _LambdaCase) caseB & guard
        protectedSetToVal <- lift ConvertM.typeProtectedSetToVal
        let setTo = protectedSetToVal (exprPl ^. Input.stored)
        let appliedCaseB =
                caseB
                & cKind .~ CaseWithArg
                    CaseArg
                    { _caVal = simplifyCaseArg argS
                    , _caToLambdaCase =
                        setTo (funcS ^. ann . pInput . Input.stored . Property.pVal)
                        <&> EntityId.ofValI
                    }
        convertIfElse setTo appliedCaseB
            & maybe (BodyCase appliedCaseB) BodyIfElse
            -- func will be our entity id, so remove it from the hidden ids
            & addActions [arg] exprPl & lift
            <&> ann . pInput . Input.entityId .~ funcS ^. ann . pInput . Input.entityId
            <&> ann . pInput . Input.userData <>~
                (exprPl ^. Input.userData <> funcS ^. ann . pInput . Input.userData)

simplifyCaseArg :: ExpressionU m a -> ExpressionU m a
simplifyCaseArg argS =
    case argS ^. val of
    BodyFromNom nom | Lens.nullOf (nVal . val . SugarLens.bodyUnfinished) nom -> nom ^. nVal
    _ -> argS
