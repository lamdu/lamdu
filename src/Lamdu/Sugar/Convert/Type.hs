-- | Convert Lamdu.Calc.Type datatypes to sugared counterparts

module Lamdu.Sugar.Convert.Type
    ( convertType
    , convertScheme
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.Transaction (MonadTransaction)
import           Hyper.Type.AST.FuncType (FuncType(..))
import           Hyper.Type.AST.Nominal (NominalInst(..))
import           Hyper.Type.AST.Row (RowExtend(..))
import qualified Hyper.Type.AST.Scheme as S
import qualified Lamdu.Calc.Type as T
import           Lamdu.Data.Anchors (anonTag)
import qualified Lamdu.Sugar.Convert.TId as ConvertTId
import qualified Lamdu.Sugar.Convert.Tag as ConvertTag
import           Lamdu.Sugar.Internal
import           Lamdu.Sugar.Internal.EntityId
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.Types

import           Lamdu.Prelude

convertComposite ::
    MonadTransaction n m =>
    EntityId -> Pure # T.Row ->
    m (CompositeFields InternalName (Annotated EntityId # Type InternalName))
convertComposite entityId (Pure (T.RExtend (RowExtend tag typ rest))) =
    do
        typS <- convertType (EntityId.ofTypeOf entityId) typ
        convertComposite (EntityId.ofRestOfComposite entityId) rest
            <&> compositeFields %~ ((tagS, typS): )
    where
        tagS = ConvertTag.withoutContext entityId tag

convertComposite _ (Pure (T.RVar v)) =
    CompositeFields mempty (Just (nameWithContext Nothing v anonTag)) & pure
convertComposite _ (Pure T.REmpty) = CompositeFields mempty Nothing & pure

convertType ::
    MonadTransaction n m =>
    EntityId -> Pure # T.Type -> m (Annotated EntityId # Type InternalName)
convertType entityId typ =
    case typ ^. _Pure of
    T.TVar tv -> nameWithContext Nothing tv anonTag & TVar & pure
    T.TFun (FuncType param res) ->
        FuncType
        <$> convertType (ofFunParam entityId) param
        <*> convertType (ofFunResult entityId) res
        <&> TFun
    T.TInst (NominalInst tid args)
        | Lens.has traverse rParams -> error "Currently row-params are unsupported"
        | otherwise ->
            TInst
            <$> ConvertTId.convert tid
            <*> traverse convertTypeParam (tParams ^@.. Lens.itraversed)
        where
            T.Types (S.QVarInstances tParams) (S.QVarInstances rParams) = args
            convertTypeParam (tv, val) =
                (,)
                <$> taggedName Nothing tv
                <*> convertType (EntityId.ofTInstParam tv entityId) val
    T.TRecord composite -> TRecord <$> convertComposite entityId composite
    T.TVariant composite -> TVariant <$> convertComposite entityId composite
    <&> Ann (Const entityId)

convertScheme :: MonadTransaction n m => EntityId -> Pure # T.Scheme -> m (Scheme InternalName)
convertScheme entityId (Pure (S.Scheme tvs typ)) =
    Scheme tvs <$> convertType entityId typ
