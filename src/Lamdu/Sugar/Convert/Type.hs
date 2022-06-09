-- | Convert Lamdu.Calc.Type datatypes to sugared counterparts

module Lamdu.Sugar.Convert.Type
    ( convertType
    , convertScheme
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.Transaction (MonadTransaction)
import qualified Data.ByteString as BS
import           Hyper.Syntax (FuncType(..))
import           Hyper.Syntax.Nominal (NominalInst(..))
import           Hyper.Syntax.Row (RowExtend(..))
import qualified Hyper.Syntax.Scheme as S
import           Lamdu.Calc.Identifier (Identifier(..))
import qualified Lamdu.Calc.Type as T
import           Lamdu.Data.Anchors (anonTag)
import qualified Lamdu.Sugar.Convert.TId as ConvertTId
import qualified Lamdu.Sugar.Convert.Tag as ConvertTag
import           Lamdu.Sugar.Internal
import           Lamdu.Sugar.Internal.EntityId
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.Types

import           Lamdu.Prelude
import qualified Lamdu.Sugar.Convert.TId as ConvertTid

convertComposite ::
    (MonadTransaction n m, ConvertTid.JumpToNominal m o) =>
    EntityId -> Pure # T.Row ->
    m (CompositeFields InternalName o # Annotated EntityId)
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
    (MonadTransaction n m, ConvertTid.JumpToNominal m o) =>
    EntityId -> Pure # T.Type -> m (Annotated EntityId # Type InternalName o)
convertType entityId typ =
    case typ ^. _Pure of
    T.TVar tv
        | isTag tv -> nameWithoutContext (T.Tag (tv ^. T._Var)) & TVar & pure
        | otherwise -> nameWithContext Nothing tv anonTag & TVar & pure
        where
            isTag (T.Var (Identifier i)) = BS.length i == 16
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
                convertType (EntityId.ofTInstParam tv entityId) val
                <&> (,) (ConvertTag.withoutContext (EntityId.ofTag entityId t) t)
                where
                    t = tv ^. T._Var & T.Tag

    T.TRecord composite -> TRecord <$> convertComposite entityId composite
    T.TVariant composite -> TVariant <$> convertComposite entityId composite
    <&> Ann (Const entityId)

convertScheme ::
    (MonadTransaction n m, ConvertTid.JumpToNominal m o) =>
    EntityId -> Pure # T.Scheme -> m (Scheme InternalName o)
convertScheme entityId (Pure (S.Scheme tvs typ)) =
    Scheme tvs <$> convertType entityId typ
