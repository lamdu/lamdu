{-# LANGUAGE NoImplicitPrelude #-}
module Lamdu.Sugar.Convert.Record
    ( convertEmpty, convertExtend
    ) where

import qualified Control.Lens as Lens
import qualified Data.Store.Property as Property
import qualified Lamdu.Calc.Val as V
import qualified Lamdu.Calc.Val.Annotated as Val
import           Lamdu.Calc.Val.Annotated (Val(..))
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.Expr.IRef as ExprIRef
import           Lamdu.Sugar.Convert.Composite (convertCompositeItem, setTagOrder, makeAddItem)
import           Lamdu.Sugar.Convert.Expression.Actions (addActions)
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Convert.Monad (ConvertM)
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import           Lamdu.Sugar.Internal
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.Types

import           Lamdu.Prelude

plValI :: Lens.Lens' (Input.Payload m a) (ExprIRef.ValI m)
plValI = Input.stored . Property.pVal

convertEmpty :: Monad m => Input.Payload m a -> ConvertM m (ExpressionU m a)
convertEmpty exprPl = do
    addItem <- exprPl ^. Input.stored & makeAddItem DataOps.recExtend
    postProcess <- ConvertM.postProcess
    BodyRecord Composite
        { _cItems = []
        , _cTail =
                DataOps.replaceWithHole (exprPl ^. Input.stored)
                <* postProcess
                <&> EntityId.ofValI
                & ClosedComposite
        , _cAddItem = addItem
        }
        & addActions exprPl

convertExtend ::
    (Monad m, Monoid a) => V.RecExtend (Val (Input.Payload m a)) ->
    Input.Payload m a -> ConvertM m (ExpressionU m a)
convertExtend (V.RecExtend tag val rest) exprPl = do
    restS <- ConvertM.convertSubexpression rest
    restRecord <-
        case restS ^. rBody of
        BodyRecord r -> return r
        _ ->
            do
                addField <- rest ^. Val.payload . Input.stored & makeAddItem DataOps.recExtend
                Composite [] (CompositeExtending restS) addField & return
    fieldS <-
        convertCompositeItem
        (exprPl ^. Input.stored) (rest ^. Val.payload . plValI)
        (EntityId.ofRecExtendTag (exprPl ^. Input.entityId)) tag val
    restRecord
        & cItems %~ (fieldS:)
        & cAddItem %~ (>>= setTagOrder (1 + length (restRecord ^. cItems)))
        & BodyRecord
        & addActions exprPl
