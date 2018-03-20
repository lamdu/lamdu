
module Lamdu.Sugar.Convert.Composite
    ( convertEmptyComposite, convertCompositeExtend, convertOneItemOpenComposite
    ) where

import qualified Control.Lens as Lens
import qualified Data.Property as Property
import qualified Data.Set as Set
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Calc.Val as V
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Convert.Monad (ConvertM)
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import           Lamdu.Sugar.Convert.Tag (convertTag, convertTagSelection, AllowAnonTag(..))
import           Lamdu.Sugar.Internal
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.Types
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
        convertTagSelection nameWithoutContext existingTags RequireTag (EntityId.ofTag (pl ^. Input.entityId)) addItem
    where
        stored = pl ^. Input.stored

convertCompositeExtend ::
    Monad m =>
    (T.Tag -> ExprIRef.ValI m -> ExprIRef.ValI m -> ExprIRef.ValBody m) ->
    (T.Tag -> ExprIRef.ValI m -> T m (DataOps.CompositeExtendResult m)) ->
    expr ->
    Input.Payload m a ->
    (T.Tag, ExprIRef.ValI m, Input.Payload m a) ->
    Composite InternalName (T m) expr ->
    ConvertM m (Composite InternalName (T m) expr)
convertCompositeExtend cons extendOp valS exprPl extendV restC =
    do
        itemS <-
            convertCompositeItem cons (exprPl ^. Input.stored)
            (extendV ^. _3 . Input.entityId) (Set.fromList restTags) valS
            (extendV & _3 %~ (^. Input.stored . Property.pVal))
        addItem <- convertAddItem extendOp (Set.fromList (extendV ^. _1 : restTags)) exprPl
        restC
            & cAddItem .~ addItem
            & cItems %~ (itemS :)
            & pure
    where
        restTags = restC ^.. cItems . traverse . ciTag . tagInfo . tagVal

convertOneItemOpenComposite ::
    Monad m =>
    V.Leaf ->
    (T.Tag -> ExprIRef.ValI m -> ExprIRef.ValI m -> ExprIRef.ValBody m) ->
    (T.Tag -> ExprIRef.ValI m -> T m (DataOps.CompositeExtendResult m)) ->
    expr -> expr ->
    Input.Payload m a ->
    (T.Tag, ExprIRef.ValI m, Input.Payload m a) ->
    ConvertM m (Composite InternalName (T m) expr)
convertOneItemOpenComposite leaf cons extendOp valS restS exprPl extendV =
    Composite
    <$> ( convertCompositeItem cons
            (exprPl ^. Input.stored) (extendV ^. _3 . Input.entityId) mempty valS
            (extendV & _3 %~ (^. Input.stored . Property.pVal))
            <&> (:[])
        )
    <*> (convertOpenCompositeActions leaf (extendV ^. _3 . Input.stored) <&> (`OpenComposite` restS))
    <*> convertAddItem extendOp (Set.singleton (extendV ^. _1)) exprPl

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
    Monad m =>
    (T.Tag -> ExprIRef.ValI m -> ExprIRef.ValI m -> ExprIRef.ValBody m) ->
    ExprIRef.ValIProperty m ->
    EntityId -> Set T.Tag -> expr ->
    -- Using tuple in place of shared RecExtend/Case structure (no such in lamdu-calculus)
    (T.Tag, ExprIRef.ValI m, ExprIRef.ValI m) ->
    ConvertM m (CompositeItem InternalName (T m) expr)
convertCompositeItem cons stored inst forbiddenTags exprS (tag, exprI, restI) =
    do
        delItem <- deleteItem stored restI
        protectedSetToVal <- ConvertM.typeProtectedSetToVal
        let setTag newTag =
                do
                    cons newTag exprI restI & ExprIRef.writeValBody valI
                    protectedSetToVal stored valI & void
                where
                    valI = stored ^. Property.pVal
        tagS <- convertTag tag nameWithoutContext forbiddenTags (EntityId.ofTag inst) setTag
        pure CompositeItem
            { _ciTag = tagS
            , _ciExpr = exprS
            , _ciDelete = delItem
            }
