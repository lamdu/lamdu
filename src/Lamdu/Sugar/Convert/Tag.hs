module Lamdu.Sugar.Convert.Tag
    ( ref, replace, withoutContext
    , taggedEntity
    , taggedEntityWith
    , AllowAnonTag(..)
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.Once (OnceT)
import           Control.Monad.Transaction (MonadTransaction, getP, setP)
import           Data.Property (MkProperty')
import qualified Data.Property as Property
import qualified Data.Set as Set
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.Expr.UniqueId as UniqueId
import           Lamdu.Sugar.Convert.Monad (ConvertM)
import           Lamdu.Sugar.Internal hiding (replaceWith)
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.Types
import           Revision.Deltum.Transaction (Transaction)

import           Lamdu.Prelude

type T = Transaction

data AllowAnonTag = AllowAnon | RequireTag

getTagsProp ::
    Anchors.HasCodeAnchors env m => env -> MkProperty' (T m) (Set T.Tag)
getTagsProp = Lens.view Anchors.codeAnchors <&> Anchors.tags

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
    ConvertM m (TagRef name (OnceT (T m)) (T m))
ref tag name forbiddenTags mkInstance setTag =
    Lens.view id
    <&> \env ->
    getTagsProp env
    & refWith (env ^. Anchors.codeAnchors) tag name forbiddenTags RequireTag
    mkInstance setTag

refWith ::
    Monad m =>
    Anchors.CodeAnchors m ->
    T.Tag -> (T.Tag -> name) -> Set T.Tag -> AllowAnonTag -> (T.Tag -> EntityId) ->
    (T.Tag -> T m ()) -> MkProperty' (T m) (Set T.Tag) ->
    TagRef name (OnceT (T m)) (T m)
refWith cp tag name forbiddenTags allowAnon mkInstance setTag tagsProp =
    TagRef
    { _tagRefTag = Tag
        { _tagName = name tag
        , _tagInstance = mkInstance tag
        , _tagVal = tag
        }
    , _tagRefReplace =
        replaceWith name forbiddenTags allowAnon mkInstance setTag tagsProp
    , _tagRefJumpTo =
        if tag == Anchors.anonTag
        then Nothing
        else Just
             (EntityId.ofTagPane tag
                 <$ DataOps.newPane cp (Anchors.PaneTag tag))
    }

replace ::
    Monad m =>
    (T.Tag -> name) -> Set T.Tag -> AllowAnonTag -> (T.Tag -> EntityId) -> (T.Tag -> T m a) ->
    ConvertM m (TagReplace name (OnceT (T m)) (T m) a)
replace name forbiddenTags allowAnon mkInstance setTag =
    Lens.view id
    <&> \env ->
    getTagsProp env
    & replaceWith name forbiddenTags allowAnon mkInstance setTag

replaceWith ::
    Monad m =>
    (T.Tag -> name) -> Set T.Tag -> AllowAnonTag -> (T.Tag -> EntityId) ->
    (T.Tag -> T m a) ->
    MkProperty' (T m) (Set T.Tag) ->
    TagReplace name (OnceT (T m)) (T m) a
replaceWith name forbiddenTags allowAnon mkInstance setTag tagsProp =
    TagReplace
    { _tsOptions =
        getP tagsProp & lift
        <&> (`Set.difference` forbiddenTags)
        <&> Set.toList
        <&> map toOption
    , _tsNewTag =
        DataOps.genNewTag
        <&>
        ( \newTag ->
            toOption newTag
            & toPick %~ (Property.modP tagsProp (Lens.contains newTag .~ True) >>)
        )
        -- TODO: Do only once!
        & lift
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

-- NOTE: Used for panes, outside ConvertM, so has no ConvertM.Context env
-- | Convert a "Entity" (param, def, TId) via its associated tag
taggedEntityWith ::
    (UniqueId.ToUUID a, MonadTransaction n m) =>
    Anchors.CodeAnchors n ->
    a -> MkProperty' (T n) (Set T.Tag) -> m (TagRef InternalName (OnceT (T n)) (T n))
taggedEntityWith cp entity tagsProp =
    getP prop
    <&>
    \entityTag ->
    refWith cp entityTag (nameWithContext entity) mempty AllowAnon
    (EntityId.ofTaggedEntity entity) (setP prop) tagsProp
    where
        prop = Anchors.assocTag entity

taggedEntity ::
    (UniqueId.ToUUID a, Monad m) =>
    a -> ConvertM m (TagRef InternalName (OnceT (T m)) (T m))
taggedEntity entity =
    do
        env <- Lens.view id
        taggedEntityWith (env ^. Anchors.codeAnchors) entity (getTagsProp env)
