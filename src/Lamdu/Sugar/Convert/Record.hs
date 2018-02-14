{-# LANGUAGE NoImplicitPrelude #-}
module Lamdu.Sugar.Convert.Record
    ( convertEmpty, convertExtend
    ) where

import qualified Control.Lens as Lens
import qualified Data.Set as Set
import qualified Lamdu.Calc.Val as V
import           Lamdu.Calc.Val.Annotated (Val(..))
import qualified Lamdu.Calc.Val.Annotated as Val
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.Expr.IRef as ExprIRef
import           Lamdu.Sugar.Convert.Composite (convertCompositeItem, convertEmptyComposite, convertAddItem, convertOpenCompositeActions)
import           Lamdu.Sugar.Convert.Expression.Actions (addActions)
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Convert.Monad (ConvertM)
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import           Lamdu.Sugar.Internal
import           Lamdu.Sugar.Types
import qualified Revision.Deltum.Property as Property

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
convertExtend (V.RecExtend tag val rest) exprPl = do
    restS <- ConvertM.convertSubexpression rest
    (restRecord, modifyEntityId) <-
        case restS ^. rBody of
        BodyRecord r ->
            convertAddItem DataOps.recExtend forbiddenTags exprPl
            <&>
            \addItem ->
            ( r & cAddItem .~ addItem
            , const (restS ^. rPayload . plEntityId)
            )
            where
                forbiddenTags =
                    tag : r ^.. cItems . traverse . ciTag . tagInfo . tagVal & Set.fromList
        _ ->
            do
                actions <- convertOpenCompositeActions V.LRecEmpty restStored
                addItem <- convertAddItem DataOps.recExtend (Set.singleton tag) exprPl
                pure
                    ( Composite
                        { _cItems = []
                        , _cTail = OpenComposite actions restS
                        , _cAddItem = addItem
                        }
                    , id
                    )
    fieldS <-
        convertCompositeItem
        (V.RecExtend <&> Lens.mapped . Lens.mapped %~ V.BRecExtend)
        (exprPl ^. Input.stored) (rest ^. Val.payload . plValI)
        tagInstSource tag val
    restRecord
        & cItems %~ (fieldS:)
        & BodyRecord
        & addActions exprPl
        <&> rPayload . plEntityId %~ modifyEntityId
    where
        restStored = rest ^. Val.payload . Input.stored
        -- Tag instance is based on extended expr,
        -- because we need to know it when offering the selection of the new tag,
        -- (before this rec-extend exists).
        tagInstSource = rest ^. Val.payload . Input.entityId
