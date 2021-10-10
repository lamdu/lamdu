-- | Convert Lamdu.Calc.Type datatypes to sugared counterparts

module Lamdu.Sugar.Convert.Type
    ( convertType
    , convertTypeWith -- a type with goto Nom
    , convertScheme
    , convertSchemeWith -- a type with goto Nom action
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.Unit (Unit(..))
import           Control.Monad.Transaction (MonadTransaction)
import           Hyper.Syntax (FuncType(..))
import           Hyper.Syntax.Nominal (NominalInst(..))
import           Hyper.Syntax.Row (RowExtend(..))
import qualified Hyper.Syntax.Scheme as S
import qualified Lamdu.Calc.Type as T
import           Lamdu.Data.Anchors (HasCodeAnchors, anonTag)
import qualified Lamdu.Sugar.Convert.TId as ConvertTId
import qualified Lamdu.Sugar.Convert.Tag as ConvertTag
import           Lamdu.Sugar.Internal
import           Lamdu.Sugar.Internal.EntityId
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.Types

import           Lamdu.Prelude

convertCompositeWith ::
    (MonadTransaction n m, MonadReader env m, HasCodeAnchors env n) =>
    _ -> EntityId -> Pure # T.Row ->
    m (CompositeFields InternalName (Annotated EntityId # Type InternalName o))
convertCompositeWith gotoNom entityId (Pure (T.RExtend (RowExtend tag typ rest))) =
    do
        typS <- convertTypeWith gotoNom (EntityId.ofTypeOf entityId) typ
        convertCompositeWith gotoNom (EntityId.ofRestOfComposite entityId) rest
            <&> compositeFields %~ ((tagS, typS): )
    where
        tagS = ConvertTag.withoutContext entityId tag

convertCompositeWith _ _ (Pure (T.RVar v)) =
    CompositeFields mempty (Just (nameWithContext Nothing v anonTag)) & pure
convertCompositeWith _ _ (Pure T.REmpty) = CompositeFields mempty Nothing & pure

convertType ::
    (MonadTransaction n m, MonadReader env m, HasCodeAnchors env n) =>
    EntityId -> Pure # T.Type -> m (Annotated EntityId # Type InternalName Unit)
convertType = convertTypeWith (const Unit)

convertTypeWith ::
    (MonadTransaction n m, MonadReader env m, HasCodeAnchors env n) =>
    _ -> EntityId -> Pure # T.Type -> m (Annotated EntityId # Type InternalName o)
convertTypeWith gotoNom entityId typ =
    case typ ^. _Pure of
    T.TVar tv -> nameWithContext Nothing tv anonTag & TVar & pure
    T.TFun (FuncType param res) ->
        FuncType
        <$> convertTypeWith gotoNom (ofFunParam entityId) param
        <*> convertTypeWith gotoNom (ofFunResult entityId) res
        <&> TFun
    T.TInst (NominalInst tid args)
        | Lens.has traverse rParams -> error "Currently row-params are unsupported"
        | otherwise ->
            TInst
            <$> (ConvertTId.convert tid <&> tidGotoDefinition .~ gotoNom tid)
            <*> traverse convertTypeParam (tParams ^@.. Lens.itraversed)
        where
            T.Types (S.QVarInstances tParams) (S.QVarInstances rParams) = args
            convertTypeParam (tv, val) =
                (,)
                <$> taggedName Nothing tv
                <*> convertTypeWith gotoNom (EntityId.ofTInstParam tv entityId) val
    T.TRecord composite -> TRecord <$> convertCompositeWith gotoNom entityId composite
    T.TVariant composite -> TVariant <$> convertCompositeWith gotoNom entityId composite
    <&> Ann (Const entityId)

convertScheme ::
    (MonadTransaction n m, MonadReader env m, HasCodeAnchors env n) =>
    EntityId -> Pure # T.Scheme -> m (Scheme InternalName Unit)
convertScheme = convertSchemeWith (const Unit)

convertSchemeWith ::
    (MonadTransaction n m, MonadReader env m, HasCodeAnchors env n) =>
    _ -> EntityId -> Pure # T.Scheme -> m (Scheme InternalName o)
convertSchemeWith gotoNom entityId (Pure (S.Scheme tvs typ)) =
    Scheme tvs <$> convertTypeWith gotoNom entityId typ
