
module Lamdu.Sugar.Convert.Tag
    ( convertTag, convertTagSelection, convertTaggedEntityWith, convertTaggedEntity
    , AllowAnonTag(..)
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.Transaction (MonadTransaction, getP, setP)
import           Data.Property (MkProperty)
import qualified Data.Set as Set
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.Expr.UniqueId as UniqueId
import           Lamdu.Sugar.Convert.Monad (ConvertM)
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import           Lamdu.Sugar.Internal
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.Types
import           Revision.Deltum.Transaction (Transaction)

import           Lamdu.Prelude

type T = Transaction

data AllowAnonTag = AllowAnon | RequireTag

getPublishedTagsProp :: Monad m => ConvertM m (MkProperty (T m) (Set T.Tag))
getPublishedTagsProp =
    Lens.view ConvertM.scCodeAnchors <&> Anchors.tags

-- forbiddenTags are sibling tags in the same record/funcParams/etc,
-- NOT type-level constraints on tags. Violation of constraints is
-- allowed, generating ordinary type errors
convertTag ::
    Monad m =>
    T.Tag -> (T.Tag -> name) -> Set T.Tag -> (T.Tag -> EntityId) ->
    (T.Tag -> T m ()) ->
    ConvertM m (Tag name (T m))
convertTag tag name forbiddenTags mkInstance setTag =
    getPublishedTagsProp
    <&> convertTagWith tag name forbiddenTags RequireTag mkInstance setTag

convertTagWith ::
    Monad m =>
    T.Tag -> (T.Tag -> name) -> Set T.Tag -> AllowAnonTag -> (T.Tag -> EntityId) ->
    (T.Tag -> T m ()) -> MkProperty (T m) (Set T.Tag) ->
    Tag name (T m)
convertTagWith tag name forbiddenTags allowAnon mkInstance setTag publishedTagsProp =
    convertTagSelectionWith name forbiddenTags allowAnon mkInstance setTag publishedTagsProp
    & Tag (TagInfo (name tag) (mkInstance tag) tag)

convertTagSelection ::
    Monad m =>
    (T.Tag -> name) -> Set T.Tag -> AllowAnonTag -> (T.Tag -> EntityId) -> (T.Tag -> T m a) ->
    ConvertM m (TagSelection name (T m) a)
convertTagSelection name forbiddenTags allowAnon mkInstance setTag =
    getPublishedTagsProp
    <&> convertTagSelectionWith name forbiddenTags allowAnon mkInstance setTag

convertTagSelectionWith ::
    Monad m =>
    (T.Tag -> name) -> Set T.Tag -> AllowAnonTag -> (T.Tag -> EntityId) -> (T.Tag -> T m a) ->
    MkProperty (T m) (Set T.Tag) ->
    TagSelection name (T m) a
convertTagSelectionWith name forbiddenTags allowAnon mkInstance setTag publishedTagsProp =
    TagSelection
    { _tsOptions =
        getP publishedTagsProp
        <&> (`Set.difference` forbiddenTags)
        <&> Set.toList
        <&> map toOption
    , _tsNewTag =
        \newName ->
        do
            newTag <- DataOps.genNewTag
            DataOps.setTagName publishedTagsProp newTag newName
            setTag newTag <&> (,) (mkInstance newTag)
    , _tsAnon =
        case allowAnon of
        RequireTag -> Nothing
        AllowAnon -> setTag Anchors.anonTag <&> (,) (mkInstance Anchors.anonTag) & Just
    }
    where
        toOption x =
            TagOption
            { _toInfo = TagInfo (name x) (mkInstance x) x
            , _toPick = setTag x
            }

-- | Convert a "Entity" (param, def, TId) via its associated tag
convertTaggedEntityWith ::
    (UniqueId.ToUUID a, MonadTransaction n m) =>
    a -> MkProperty (T n) (Set T.Tag) -> m (Tag InternalName (T n))
convertTaggedEntityWith entity publishedTagsProp =
    getP prop
    <&>
    \entityTag ->
    convertTagWith entityTag (nameWithContext entity) mempty AllowAnon
    (EntityId.ofTaggedEntity entity) (setP prop) publishedTagsProp
    where
        prop = Anchors.assocTag entity

convertTaggedEntity :: (UniqueId.ToUUID a, Monad m) => a -> ConvertM m (Tag InternalName (T m))
convertTaggedEntity entity = getPublishedTagsProp >>= convertTaggedEntityWith entity
