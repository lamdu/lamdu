
module Lamdu.Sugar.Convert.Tag
    ( convertTag, convertTagSelection, convertTaggedEntityWith, convertTaggedEntity
    , AllowAnonTag(..)
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.Transaction (MonadTransaction, getP, setP)
import qualified Data.Property as Property
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

publishedTagsGetter :: Monad m => ConvertM m (T m (Set T.Tag))
publishedTagsGetter =
    Lens.view ConvertM.scCodeAnchors <&> Anchors.tags <&> Property.getP

-- forbiddenTags are sibling tags in the same record/funcParams/etc,
-- NOT type-level constraints on tags. Violation of constraints is
-- allowed, generating ordinary type errors
convertTag ::
    Monad m =>
    T.Tag -> (T.Tag -> name) -> Set T.Tag -> (T.Tag -> EntityId) ->
    (T.Tag -> T m ()) ->
    ConvertM m (Tag name (T m))
convertTag tag name forbiddenTags mkInstance setTag =
    publishedTagsGetter
    <&> convertTagWith tag name forbiddenTags RequireTag mkInstance setTag

convertTagWith ::
    Monad m =>
    T.Tag -> (T.Tag -> name) -> Set T.Tag -> AllowAnonTag -> (T.Tag -> EntityId) ->
    (T.Tag -> T m ()) -> T m (Set T.Tag) ->
    Tag name (T m)
convertTagWith tag name forbiddenTags allowAnon mkInstance setTag getPublishedTags =
    convertTagSelectionWith name forbiddenTags allowAnon mkInstance setTag getPublishedTags
    & Tag (TagInfo (mkInstance tag) tag) (name tag)

convertTagSelection ::
    Monad m =>
    (T.Tag -> name) -> Set T.Tag -> AllowAnonTag -> (T.Tag -> EntityId) -> (T.Tag -> T m a) ->
    ConvertM m (TagSelection name (T m) a)
convertTagSelection name forbiddenTags allowAnon mkInstance setTag =
    publishedTagsGetter
    <&> convertTagSelectionWith name forbiddenTags allowAnon mkInstance setTag

convertTagSelectionWith ::
    Monad m =>
    (T.Tag -> name) -> Set T.Tag -> AllowAnonTag -> (T.Tag -> EntityId) -> (T.Tag -> T m a) ->
    T m (Set T.Tag) ->
    TagSelection name (T m) a
convertTagSelectionWith name forbiddenTags allowAnon mkInstance setTag getPublishedTags =
    TagSelection
    { _tsOptions =
        getPublishedTags
        <&> (`Set.difference` forbiddenTags)
        <&> Set.toList
        <&> map toOption
    , _tsNewTag =
        do
            newTag <- DataOps.genNewTag
            setTag newTag <&> (,,) (name newTag) (mkInfo newTag)
    , _tsAnon =
        case allowAnon of
        RequireTag -> Nothing
        AllowAnon -> setTag Anchors.anonTag <&> (,) (mkInstance Anchors.anonTag) & Just
    }
    where
        mkInfo t = TagInfo (mkInstance t) t
        toOption x =
            TagOption
            { _toName = name x
            , _toInfo = mkInfo x
            , _toPick = setTag x
            }

-- | Convert a "Entity" (param, def, TId) via its associated tag
convertTaggedEntityWith ::
    (UniqueId.ToUUID a, MonadTransaction n m) =>
    a -> T n (Set T.Tag) -> m (Tag InternalName (T n))
convertTaggedEntityWith entity getPublishedTags =
    getP prop
    <&>
    \entityTag ->
    convertTagWith entityTag (nameWithContext entity) mempty AllowAnon
    (EntityId.ofTaggedEntity entity) (setP prop) getPublishedTags
    where
        prop = Anchors.assocTag entity

convertTaggedEntity :: (UniqueId.ToUUID a, Monad m) => a -> ConvertM m (Tag InternalName (T m))
convertTaggedEntity entity = publishedTagsGetter >>= convertTaggedEntityWith entity
