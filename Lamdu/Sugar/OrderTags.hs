module Lamdu.Sugar.OrderTags
    ( orderDef
    ) where

import Control.Lens.Operators
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
    where
        FlatComposite fields mExt = FlatComposite.fromComposite c

orderType :: MonadA m => Order m Type
orderType = ExprLens._TRecord %%~ orderComposite

orderRecordFields ::
    MonadA m => Order m [Sugar.RecordField name f (Sugar.Expression name f a)]
orderRecordFields xs =
    xs
    & Lens.traversed . Sugar.rfExpr %%~ orderExpr
    >>= orderByTag (^. Sugar.rfTag . Sugar.tagVal)

orderRecord ::
    MonadA m => Order m (Sugar.Record name f (Sugar.Expression name f a))
orderRecord = Sugar.rItems %%~ orderRecordFields

orderBody :: MonadA m => Order m (Sugar.Body name f (Sugar.Expression name f a))
orderBody (Sugar.BodyLam b) = orderBinder b <&> Sugar.BodyLam
orderBody (Sugar.BodyRecord r) = orderRecord r <&> Sugar.BodyRecord
orderBody b = b & Lens.traversed %%~ orderExpr

orderExpr :: MonadA m => Order m (Sugar.Expression name f a)
orderExpr e =
    e
    & Sugar.rPayload . Sugar.plInferredType %%~ orderType
    >>= Sugar.rBody %%~ orderBody

orderParams :: MonadA m => Order m [Sugar.FuncParam T.Tag name f]
orderParams xs =
    xs
    & Lens.traversed . Sugar.fpInferredType %%~ orderType
    >>= orderByTag (^. Sugar.fpVarInfo)

orderBinder ::
    MonadA m => Order m (Sugar.Binder name f (Sugar.Expression name f a))
orderBinder b =
    b
    & Sugar.dParams . Sugar._FieldParams %%~ orderParams
    >>= Sugar.dBody %%~ orderExpr

orderDef ::
    MonadA m => Order m (Sugar.Definition name f (Sugar.Expression name f a))
orderDef def =
    def
    & SugarLens.defSchemes . S.schemeType %%~ orderType
    >>= Sugar.drBody . Sugar._DefinitionBodyExpression . Sugar.deContent
        %%~ orderBinder
