module Lamdu.Sugar.OrderTags
    ( orderDef
    ) where

import Control.Lens.Operators
import Control.MonadA (MonadA)
import Lamdu.Expr.FlatComposite (FlatComposite(..))
import Lamdu.Expr.Type (Type)
import qualified Control.Lens as Lens
import qualified Data.Map as Map
import qualified Lamdu.Expr.FlatComposite as FlatComposite
import qualified Lamdu.Expr.Lens as ExprLens
import qualified Lamdu.Expr.Scheme as S
import qualified Lamdu.Expr.Type as T
import qualified Lamdu.Sugar.Lens as SugarLens
import qualified Lamdu.Sugar.Types as Sugar

-- TODO: read order of tags
orderComposite ::
    MonadA m => T.Composite T.Product -> m (T.Composite T.Product)
orderComposite c =
    fields
    & Map.toList
    & return -- TODO: order fields
    <&> foldr (uncurry T.CExtend) (maybe T.CEmpty T.CVar mExt)
    where
        FlatComposite fields mExt = FlatComposite.fromComposite c

orderType :: MonadA m => Type -> m Type
orderType = ExprLens._TRecord %%~ orderComposite

orderRecordFields :: MonadA m =>
    [Sugar.RecordField name f (Sugar.Expression name f a)] ->
    m [Sugar.RecordField name f (Sugar.Expression name f a)]
orderRecordFields xs =
    xs
    & Lens.traversed . Sugar.rfExpr %%~ orderExpr
    -- >>= TODO - order the fields

orderRecord :: MonadA m =>
    Sugar.Record name f (Sugar.Expression name f a) ->
    m (Sugar.Record name f (Sugar.Expression name f a))
orderRecord = Sugar.rItems %%~ orderRecordFields

orderBody :: MonadA m =>
    Sugar.Body name f (Sugar.Expression name f a) ->
    m (Sugar.Body name f (Sugar.Expression name f a))
orderBody (Sugar.BodyLam b) = orderBinder b <&> Sugar.BodyLam
orderBody (Sugar.BodyRecord r) = orderRecord r <&> Sugar.BodyRecord
orderBody b = b & Lens.traversed %%~ orderExpr

orderExpr ::
    MonadA m => Sugar.Expression name f a -> m (Sugar.Expression name f a)
orderExpr e =
    e
    & Sugar.rPayload . Sugar.plInferredType %%~ orderType
    >>= Sugar.rBody %%~ orderBody

orderParams ::
    MonadA m => [Sugar.FuncParam name f] -> m [Sugar.FuncParam name f]
orderParams xs =
    xs
    & Lens.traversed . Sugar.fpInferredType %%~ orderType
    -- >>= TODO: order the params

orderBinder :: MonadA m =>
  Sugar.Binder name f (Sugar.Expression name f a) ->
  m (Sugar.Binder name f (Sugar.Expression name f a))
orderBinder b =
    b
    & Sugar.dParams %%~ orderParams
    >>= Sugar.dBody %%~ orderExpr

orderDef ::
    MonadA m =>
    Sugar.Definition name f (Sugar.Expression name f a) ->
    m (Sugar.Definition name f (Sugar.Expression name f a))
orderDef def =
    def
    & SugarLens.defSchemes . S.schemeType %%~ orderType
    >>= Sugar.drBody . Sugar._DefinitionBodyExpression . Sugar.deContent
        %%~ orderBinder
