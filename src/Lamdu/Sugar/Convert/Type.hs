-- | Convert Lamdu.Calc.Type datatypes to sugared counterparts

module Lamdu.Sugar.Convert.Type
    ( convertType
    , convertScheme
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.Transaction (MonadTransaction)
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Calc.Type.Scheme as Scheme
import           Lamdu.Data.Anchors (anonTag)
import qualified Lamdu.Sugar.Convert.TId as ConvertTId
import           Lamdu.Sugar.Internal
import           Lamdu.Sugar.Internal.EntityId
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.Types

import           Lamdu.Prelude

convertComposite ::
    MonadTransaction n m =>
    EntityId -> T.Composite p -> m (CompositeFields p InternalName (Type InternalName))
convertComposite entityId (T.CExtend tag typ rest) =
    do
        typS <- convertType (EntityId.ofTypeOf entityId) typ
        convertComposite (EntityId.ofRestOfComposite entityId) rest
            <&> compositeFields %~ ((info, typS): )
    where
        info =
            TagInfo
            { _tagName = nameWithoutContext tag
            , _tagInstance = EntityId.ofTag entityId tag
            , _tagVal = tag
            }
convertComposite _ (T.CVar v) =
    CompositeFields mempty (Just (nameWithContext v anonTag)) & pure
convertComposite _ T.CEmpty = CompositeFields mempty Nothing & pure

convertType :: MonadTransaction n m => EntityId -> T.Type -> m (Type InternalName)
convertType entityId typ =
    case typ of
    T.TVar tv -> nameWithContext tv anonTag & TVar & pure
    T.TFun param res ->
        TFun
        <$> convertType (ofFunParam entityId) param
        <*> convertType (ofFunResult entityId) res
    T.TInst tid args ->
        TInst
        <$> ConvertTId.convert tid
        <*> Lens.itraverse convertParam args
        where
            convertParam p = convertType (EntityId.ofTInstParam p entityId)
    T.TRecord composite -> TRecord <$> convertComposite entityId composite
    T.TVariant composite -> TVariant <$> convertComposite entityId composite
    <&> Type entityId

convertScheme :: MonadTransaction n m => EntityId -> Scheme.Scheme -> m (Scheme InternalName)
convertScheme entityId (Scheme.Scheme tvs cs typ) =
    Scheme tvs cs <$> convertType entityId typ
