module Lamdu.Sugar.Convert.Tag
    ( ref, replace, withoutContext
    , taggedEntity
    , taggedEntityWith
    , AllowAnonTag(..)
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.Transaction (MonadTransaction, getP, setP)
import           Data.Property (MkProperty')
import qualified Data.Property as Property
import qualified Data.Set as Set
import qualified GUI.Momentu.Direction as Dir
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.Expr.UniqueId as UniqueId
import           Lamdu.I18N.LangId (LangId)
import           Lamdu.Sugar.Convert.Monad (ConvertM)
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import           Lamdu.Sugar.Internal hiding (replaceWith)
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.Types
import           Revision.Deltum.Transaction (Transaction)

import           Lamdu.Prelude

type T = Transaction

data AllowAnonTag = AllowAnon | RequireTag

getTagsProp :: Monad m => ConvertM m (MkProperty' (T m) (Set T.Tag))
getTagsProp =
    Lens.view ConvertM.scCodeAnchors <&> Anchors.tags

withoutContext :: EntityId -> T.Tag -> Tag InternalName
withoutContext entityId tag =
    Tag
    { _tagName = nameWithoutContext tag
    , _tagInstance = EntityId.ofTag entityId tag
    , _tagVal = tag
    }

-- forbiddenTags are sibling tags in the same record/funcParams/etc,
-- NOT type-level constraints on tags. Violation of constraints is
-- allowed, generating ordinary type errors
ref ::
    Monad m =>
    T.Tag -> (T.Tag -> name) -> Set T.Tag -> (T.Tag -> EntityId) ->
    (T.Tag -> T m ()) ->
    ConvertM m (TagRef name (T m) (T m))
ref tag name forbiddenTags mkInstance setTag =
    do
        env <- Lens.view id
        getTagsProp
            <&> refWith env tag name forbiddenTags RequireTag mkInstance setTag

refWith ::
    (Monad m, Has LangId env, Has Dir.Layout env) =>
    env ->
    T.Tag -> (T.Tag -> name) -> Set T.Tag -> AllowAnonTag -> (T.Tag -> EntityId) ->
    (T.Tag -> T m ()) -> MkProperty' (T m) (Set T.Tag) ->
    TagRef name (T m) (T m)
refWith env tag name forbiddenTags allowAnon mkInstance setTag tagsProp =
    replaceWith env name forbiddenTags allowAnon mkInstance setTag tagsProp
    & TagRef Tag
    { _tagName = name tag
    , _tagInstance = mkInstance tag
    , _tagVal = tag
    }

replace ::
    Monad m =>
    (T.Tag -> name) -> Set T.Tag -> AllowAnonTag -> (T.Tag -> EntityId) -> (T.Tag -> T m a) ->
    ConvertM m (TagReplace name (T m) (T m) a)
replace name forbiddenTags allowAnon mkInstance setTag =
    do
        env <- Lens.view id
        getTagsProp <&>
            replaceWith env name forbiddenTags allowAnon mkInstance setTag

replaceWith ::
    (Monad m, Has LangId env, Has Dir.Layout env) =>
    env ->
    (T.Tag -> name) -> Set T.Tag -> AllowAnonTag -> (T.Tag -> EntityId) -> (T.Tag -> T m a) ->
    MkProperty' (T m) (Set T.Tag) ->
    TagReplace name (T m) (T m) a
replaceWith env name forbiddenTags allowAnon mkInstance setTag tagsProp =
    TagReplace
    { _tsOptions =
        getP tagsProp
        <&> (`Set.difference` forbiddenTags)
        <&> Set.toList
        <&> map toOption
    , _tsNewTag =
        \newName ->
        do
            newTag <- DataOps.genNewTag
            Property.setP (DataOps.assocTagName env newTag) newName
            Property.modP tagsProp (Lens.contains newTag .~ True)
            setTag newTag <&> (,) (mkInstance newTag)
    , _tsAnon =
        case allowAnon of
        RequireTag -> Nothing
        AllowAnon -> setTag Anchors.anonTag <&> (,) (mkInstance Anchors.anonTag) & Just
    }
    where
        toOption x =
            TagOption
            { _toInfo =
                Tag
                { _tagName = name x
                , _tagInstance = mkInstance x
                , _tagVal = x
                }
            , _toPick = setTag x
            }

-- | Convert a "Entity" (param, def, TId) via its associated tag
taggedEntityWith ::
    ( UniqueId.ToUUID a, MonadTransaction n m
    , Has LangId env, Has Dir.Layout env
    ) =>
    env ->
    a -> MkProperty' (T n) (Set T.Tag) -> m (TagRef InternalName (T n) (T n))
taggedEntityWith env entity tagsProp =
    getP prop
    <&>
    \entityTag ->
    refWith env entityTag (nameWithContext entity) mempty AllowAnon
    (EntityId.ofTaggedEntity entity) (setP prop) tagsProp
    where
        prop = Anchors.assocTag entity

taggedEntity ::
    (UniqueId.ToUUID a, Monad m) => a -> ConvertM m (TagRef InternalName (T m) (T m))
taggedEntity entity =
    do
        env <- Lens.view id
        getTagsProp >>= taggedEntityWith env entity
