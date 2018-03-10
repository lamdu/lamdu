{-# LANGUAGE NoImplicitPrelude #-}
module Lamdu.Sugar.Convert.Record
    ( convertEmpty, convertExtend
    ) where

import qualified Control.Lens as Lens
import qualified Data.Property as Property
import qualified Lamdu.Calc.Val as V
import           Lamdu.Calc.Val.Annotated (Val(..))
import qualified Lamdu.Calc.Val.Annotated as Val
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.Expr.IRef as ExprIRef
import           Lamdu.Sugar.Convert.Composite (convertEmptyComposite, convertCompositeExtend, convertOneItemOpenComposite)
import           Lamdu.Sugar.Convert.Expression.Actions (addActions)
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Convert.Monad (ConvertM)
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import           Lamdu.Sugar.Internal
import           Lamdu.Sugar.Types

import           Lamdu.Prelude

plValI :: Lens.Lens' (Input.Payload m a) (ExprIRef.ValI m)
plValI = Input.stored . Property.pVal

convertEmpty :: Monad m => Input.Payload m a -> ConvertM m (ExpressionU m a)
convertEmpty pl =
    convertEmptyComposite DataOps.recExtend pl
    <&> BodyRecord
    >>= addActions pl

convertExtend ::
    (Monad m, Monoid a) => V.RecExtend (Val (Input.Payload m a)) ->
    Input.Payload m a -> ConvertM m (ExpressionU m a)
convertExtend recExtend exprPl =
    do
        V.RecExtend tag valS restS <- traverse ConvertM.convertSubexpression recExtend
        let recP =
                ( tag
                , recExtend ^. V.recFieldVal . Val.payload . plValI
                , recExtend ^. V.recRest . Val.payload
                )
        (modifyEntityId, restRecord) <-
            case restS ^. rBody of
            BodyRecord r ->
                convertCompositeExtend mkRecExtend DataOps.recExtend valS exprPl recP r
                <&> (,) (const (restS ^. rPayload . plEntityId))
            _ ->
                convertOneItemOpenComposite V.LRecEmpty mkRecExtend DataOps.recExtend valS restS exprPl recP
                <&> (,) id
        restRecord
            & BodyRecord
            & addActions exprPl
            <&> rPayload . plEntityId %~ modifyEntityId
    where
        mkRecExtend t v r = V.RecExtend t v r & V.BRecExtend
