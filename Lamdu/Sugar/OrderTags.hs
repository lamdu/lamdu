module Lamdu.Sugar.OrderTags
    ( orderDef
    , orderedFlatComposite
    ) where

import Control.Lens.Operators
import Control.Monad ((>=>))
import Control.MonadA (MonadA)
import Data.List.Utils (sortOn)
import Data.Store.Transaction (Transaction)
import Lamdu.Data.Anchors (assocTagOrder)
import Lamdu.Expr.FlatComposite (FlatComposite(..))
import Lamdu.Expr.Type (Type)
import qualified Control.Lens as Lens
import qualified Data.Map as Map
import qualified Data.Store.Transaction as Transaction
import qualified Lamdu.Expr.FlatComposite as FlatComposite
import qualified Lamdu.Expr.Lens as ExprLens
import qualified Lamdu.Expr.Scheme as S
import qualified Lamdu.Expr.Type as T
import qualified Lamdu.Sugar.Lens as SugarLens
import qualified Lamdu.Sugar.Types as Sugar

type Order m x = x -> Transaction m x

orderByTag :: MonadA m => (a -> T.Tag) -> Order m [a]
orderByTag toTag =
    fmap (map fst . sortOn snd) . mapM loadOrder
    where
        loadOrder x =
            toTag x
            & assocTagOrder
            & Transaction.getP
            <&> (,) x

orderComposite :: MonadA m => Order m (T.Composite T.Product)
orderComposite c =
    fields
    & Map.toList
    & orderByTag fst
    <&> foldr (uncurry T.CExtend) (maybe T.CEmpty T.CVar mExt)
    >>= T.compositeTypes orderType
    where
        FlatComposite fields mExt = FlatComposite.fromComposite c

orderType :: MonadA m => Order m Type
orderType t =
    t
    & ExprLens._TRecord orderComposite
    >>= T.nextLayer orderType

orderRecordFields ::
    MonadA m => Order m [Sugar.RecordField name f a]
orderRecordFields = orderByTag (^. Sugar.rfTag . Sugar.tagVal)

orderRecord ::
    MonadA m => Order m (Sugar.Record name f a)
orderRecord = Sugar.rItems %%~ orderRecordFields

orderApply :: MonadA m => Order m (Sugar.Apply name a)
orderApply = Sugar.aAnnotatedArgs %%~ orderByTag (^. Sugar.aaTag . Sugar.tagVal)

orderHoleResult :: MonadA m => Order m (Sugar.HoleResult name m)
orderHoleResult = Sugar.holeResultConverted %%~ orderExpr

orderHole :: MonadA m => Sugar.Hole name m a -> Sugar.Hole name m a
orderHole =
    Sugar.holeMActions . Lens._Just . Sugar.holeResults .
    Lens.mapped . Lens.mapped . Lens._2 %~ (>>= orderHoleResult)

orderBody :: MonadA m => Order m (Sugar.Body name m a)
orderBody (Sugar.BodyLam b) = orderBinder b <&> Sugar.BodyLam
orderBody (Sugar.BodyRecord r) = orderRecord r <&> Sugar.BodyRecord
orderBody (Sugar.BodyApply a) = orderApply a <&> Sugar.BodyApply
orderBody (Sugar.BodyHole a) = orderHole a & Sugar.BodyHole & return
orderBody b = return b

orderExpr :: MonadA m => Order m (Sugar.Expression name m a)
orderExpr e =
    e
    & Sugar.rPayload . Sugar.plAnnotation . Sugar.aInferredType %%~ orderType
    >>= Sugar.rBody %%~ orderBody
    >>= Sugar.rBody . Lens.traversed %%~ orderExpr

orderParams :: MonadA m => Order m [Sugar.FuncParam T.Tag name m]
orderParams xs =
    xs
    & Lens.traversed . Sugar.fpAnnotation . Sugar.aInferredType %%~ orderType
    >>= orderByTag (^. Sugar.fpVarInfo)

orderBinder ::
    MonadA m => Order m (Sugar.Binder name m a)
orderBinder b =
    b
    & Sugar.bParams . Sugar._FieldParams %%~ orderParams

orderDef ::
    MonadA m => Order m (Sugar.Definition name m (Sugar.Expression name m a))
orderDef def =
    def
    & SugarLens.defSchemes . S.schemeType %%~ orderType
    >>= Sugar.drBody . Sugar._DefinitionBodyExpression . Sugar.deContent
        %%~ (orderBinder >=> Lens.traversed %%~ orderExpr)

orderedFlatComposite ::
    T.Composite a -> ([(T.Tag, T.Type)], Maybe (T.Var (T.Composite a)))
orderedFlatComposite T.CEmpty = ([], Nothing)
orderedFlatComposite (T.CVar x) = ([], Just x)
orderedFlatComposite (T.CExtend tag typ rest) =
    orderedFlatComposite rest
    & Lens._1 %~ (:) (tag, typ)
