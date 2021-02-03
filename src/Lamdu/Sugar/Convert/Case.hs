module Lamdu.Sugar.Convert.Case
    ( convert
    , convertAbsurd
    ) where

import           Hyper.Type.AST.Row (RowExtend(..))
import qualified Lamdu.Calc.Term as V
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Sugar.Convert.Composite as Composite
import           Lamdu.Sugar.Convert.Expression.Actions (addActions)
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Convert.Monad (ConvertM)
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import           Lamdu.Sugar.Internal
import           Lamdu.Sugar.Types

import           Lamdu.Prelude

-- This is mostly a copy&paste of the Convert.Record module, yuck! DRY
-- with some abstraction?

convertAbsurd ::
    (Monad m, Monoid a) =>
    Input.Payload m a # V.Term -> ConvertM m (ExpressionU v m a)
convertAbsurd pl =
    Composite.convertEmpty DataOps.case_ pl
    <&> PfCase <&> BodyPostfixFunc
    >>= addActions (Const ()) pl

convert ::
    (Monad m, Monoid a) =>
    RowExtend T.Tag V.Term V.Term # Ann (Input.Payload m a) ->
    Input.Payload m a # V.Term ->
    ConvertM m (ExpressionU EvalPrep m a)
convert (RowExtend tag v rest) exprPl =
    do
        valS <-
            ConvertM.convertSubexpression v
            <&> hVal . _BodyLam . lamApplyLimit .~ AtMostOneFuncApply
        restS <- ConvertM.convertSubexpression rest
        let caseP =
                Composite.ExtendVal
                { Composite._extendTag = tag
                , Composite._extendValI = v ^. hAnn . Input.stored . ExprIRef.iref
                , Composite._extendRest = rest ^. hAnn
                }
        Composite.convert DataOps.case_ V.LAbsurd mkCase (_BodyPostfixFunc . _PfCase) valS restS
            exprPl caseP
    where
        mkCase t c r = RowExtend t c r & V.BCase
