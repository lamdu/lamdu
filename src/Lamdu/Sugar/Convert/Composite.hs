{-# LANGUAGE NoImplicitPrelude #-}

module Lamdu.Sugar.Convert.Composite
    ( convertCompositeItem, convertEmptyComposite, convertOpenCompositeActions, convertAddItem
    ) where

import qualified Control.Lens as Lens
import qualified Data.Set as Set
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Calc.Val as V
import           Lamdu.Calc.Val.Annotated (Val(..))
import qualified Lamdu.Calc.Val.Annotated as Val
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Convert.Monad (ConvertM)
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import           Lamdu.Sugar.Convert.Tag (convertTag, convertTagSelection)
import           Lamdu.Sugar.Internal
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.Types
import qualified Revision.Deltum.Property as Property
import           Revision.Deltum.Transaction (Transaction)
import qualified Revision.Deltum.Transaction as Transaction

import           Lamdu.Prelude

type T = Transaction

deleteItem ::
    Monad m =>
    ExprIRef.ValIProperty m -> ExprIRef.ValI m ->
    ConvertM m (T m EntityId)
deleteItem stored restI =
    ConvertM.typeProtectedSetToVal ?? stored ?? restI <&> Lens.mapped %~ EntityId.ofValI

convertAddItem ::
    Monad m =>
    (T.Tag -> ExprIRef.ValI m -> T m (DataOps.CompositeExtendResult m)) ->
    Set T.Tag ->
    Input.Payload m a ->
    ConvertM m (TagSelection InternalName (T m) EntityId)
convertAddItem extendOp existingTags pl =
    do
        addItem <-
            ConvertM.typeProtectedSetToVal
            <&>
            \protectedSetToVal tag ->
            do
                DataOps.CompositeExtendResult newValI resultI <- extendOp tag (stored ^. Property.pVal)
                _ <- protectedSetToVal stored resultI
                Transaction.setP (Anchors.assocTagOrder tag) (Set.size existingTags)
                EntityId.ofValI newValI & pure
        convertTagSelection existingTags (EntityId.ofTag (pl ^. Input.entityId)) addItem
    where
        stored = pl ^. Input.stored

convertOpenCompositeActions ::
    Monad m => V.Leaf -> ExprIRef.ValIProperty m -> ConvertM m (OpenCompositeActions (T m))
convertOpenCompositeActions leaf stored =
    ConvertM.typeProtectedSetToVal
    <&>
    \protectedSetToVal ->
    OpenCompositeActions
    { _openCompositeClose =
        ExprIRef.newValBody (V.BLeaf leaf)
        >>= protectedSetToVal stored
        <&> EntityId.ofValI
    }

convertEmptyComposite ::
    Monad m =>
    (T.Tag -> ExprIRef.ValI m -> T m (DataOps.CompositeExtendResult m)) ->
    Input.Payload m a ->
    ConvertM m (Composite InternalName (T m) expr)
convertEmptyComposite extendOp exprPl =
    do
        actions <-
            ConvertM.postProcess
            <&>
            \postProcess ->
            ClosedCompositeActions
            { _closedCompositeOpen =
                DataOps.replaceWithHole (exprPl ^. Input.stored)
                <* postProcess
                <&> EntityId.ofValI
            }
        addItem <- convertAddItem extendOp mempty exprPl
        pure Composite
            { _cItems = []
            , _cTail = ClosedComposite actions
            , _cAddItem = addItem
            }

convertCompositeItem ::
    (Monad m, Monoid a) =>
    (T.Tag -> ExprIRef.ValI m -> ExprIRef.ValI m -> ExprIRef.ValBody m) ->
    ExprIRef.ValIProperty m ->
    ExprIRef.ValI m ->
    EntityId -> T.Tag -> Val (Input.Payload m a) ->
    ConvertM m (CompositeItem InternalName (T m) (ExpressionU m a))
convertCompositeItem cons stored restI inst tag expr =
    do
        exprS <- ConvertM.convertSubexpression expr
        delItem <- deleteItem stored restI
        protectedSetToVal <- ConvertM.typeProtectedSetToVal
        let setTag newTag =
                do
                    cons newTag (expr ^. Val.payload . Input.stored . Property.pVal) restI
                        & ExprIRef.writeValBody valI
                    protectedSetToVal stored valI & void
                where
                    valI = stored ^. Property.pVal
        tagS <- convertTag tag mempty (EntityId.ofTag inst) setTag
        pure CompositeItem
            { _ciTag = tagS
            , _ciExpr = exprS
            , _ciDelete = delItem
            }
