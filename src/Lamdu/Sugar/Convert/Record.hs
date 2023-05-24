module Lamdu.Sugar.Convert.Record
    ( convertEmpty, convertExtend
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

convertEmpty :: Monad m => Input.Payload m # V.Term -> ConvertM m (ExpressionU v m)
convertEmpty pl =
    Composite.convertEmpty Composite.CompRecord (pl ^. Input.stored)
    <&> BodyRecord
    >>= addActions (Ann pl (V.BLeaf V.LRecEmpty))
    <&> annotation . pActions . mApply .~ Nothing

convertExtend ::
    Monad m =>
    RowExtend T.Tag V.Term V.Term # Ann (Input.Payload m) ->
    Input.Payload m # V.Term ->
    ConvertM m (ExpressionU EvalPrep m)
convertExtend r@(RowExtend tag val rest) exprPl =
    do
        valS <- ConvertM.convertSubexpression val
        restS <- ConvertM.convertSubexpression rest
        let recP =
                Composite.ExtendVal
                { Composite._extendTag = tag
                , Composite._extendValI = val ^. hAnn . Input.stored . ExprIRef.iref
                , Composite._extendRest = rest ^. hAnn
                }
        Composite.convert Composite.CompRecord valS restS (Ann exprPl (V.BRecExtend r)) recP
    <&> annotation . pActions . mApply .~ Nothing
