module Lamdu.Sugar.Convert.Case
    ( convert
    , convertAbsurd
    ) where

import           Hyper.Syntax.Row (RowExtend(..))
import qualified Lamdu.Calc.Term as V
import qualified Lamdu.Calc.Type as T
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

convertAbsurd :: Monad m => Input.Payload m # V.Term -> ConvertM m (ExpressionU v m)
convertAbsurd pl =
    Composite.convertEmpty V.BCase (pl ^. Input.stored)
    <&> PfCase <&> BodyPostfixFunc
    >>= addActions (Ann pl (V.BLeaf V.LAbsurd))

convert ::
    Monad m =>
    RowExtend T.Tag V.Term V.Term # Ann (Input.Payload m) ->
    Input.Payload m # V.Term ->
    ConvertM m (ExpressionU EvalPrep m)
convert r@(RowExtend tag v rest) exprPl =
    do
        valS <- ConvertM.convertSubexpression v
        restS <- ConvertM.convertSubexpression rest
        let caseP =
                Composite.ExtendVal
                { Composite._extendTag = tag
                , Composite._extendValI = v ^. hAnn . Input.stored . ExprIRef.iref
                , Composite._extendRest = rest ^. hAnn
                }
        Composite.convert V.BCase (_BodyPostfixFunc . _PfCase) valS restS (Ann exprPl (V.BCase r)) caseP
