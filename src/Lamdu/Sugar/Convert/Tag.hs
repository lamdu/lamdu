{-# LANGUAGE TemplateHaskell #-}
module Lamdu.Sugar.Convert.Tag
    ( ref, replace, withoutContext
    , taggedEntity
    , taggedEntityWith
    , NameContext(..), ncMVarInfo, ncUuid
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.Once (MonadOnce(..), OnceT)
import           Control.Monad.Transaction (MonadTransaction, getP, setP)
import           Data.Property (MkProperty')
import qualified Data.Property as Property
import qualified Data.Set as Set
import           Data.UUID (UUID)
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.Expr.UniqueId as UniqueId
import           Lamdu.Sugar.Convert.NameRef (jumpToTag)
import           Lamdu.Sugar.Internal hiding (replaceWith)
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.Types
import           Revision.Deltum.Transaction (Transaction)

import           Lamdu.Prelude

type T = Transaction

data NameContext = NameContext
    { _ncMVarInfo :: Maybe VarInfo
    , _ncUuid :: UUID
    }

Lens.makeLenses ''NameContext

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
    (MonadTransaction n m, MonadReader env m, Anchors.HasCodeAnchors env n) =>
    T.Tag -> Maybe NameContext -> Set T.Tag -> (T.Tag -> EntityId) ->
    (T.Tag -> T n ()) ->
    m (OnceT (T n) (TagRef InternalName (OnceT (T n)) (T n)))
ref tag nameCtx forbiddenTags mkInstance setTag =
    Lens.view Anchors.codeAnchors
    <&> \anchors -> refWith anchors tag name forbiddenTags mkInstance setTag
    where
        withContext (NameContext varInfo var) = nameWithContext varInfo var
        name = maybe nameWithoutContext withContext nameCtx

refWith ::
    Monad m =>
    Anchors.CodeAnchors m ->
    T.Tag -> (T.Tag -> name) -> Set T.Tag -> (T.Tag -> EntityId) ->
    (T.Tag -> T m ()) ->
    OnceT (T m) (TagRef name (OnceT (T m)) (T m))
refWith cp tag name forbiddenTags mkInstance setTag =
    replaceWith name forbiddenTags mkInstance setTag tagsProp
    <&>
    \r ->
    TagRef
    { _tagRefTag = Tag
        { _tagName = name tag
        , _tagInstance = mkInstance tag
        , _tagVal = tag
        }
    , _tagRefReplace = r
    , _tagRefJumpTo =
        if tag == Anchors.anonTag
        then Nothing
        else jumpToTag cp tag & Just
    }
    where
        tagsProp = Anchors.tags cp

replace ::
    (MonadTransaction n m, MonadReader env m, Anchors.HasCodeAnchors env n) =>
    (T.Tag -> name) -> Set T.Tag -> (T.Tag -> EntityId) -> (T.Tag -> T n ()) ->
    m (OnceT (T n) (TagChoice name (OnceT (T n)) (T n)))
replace name forbiddenTags mkInstance setTag =
    Lens.view Anchors.codeAnchors <&> Anchors.tags
    <&> replaceWith name forbiddenTags mkInstance setTag

replaceWith ::
    Monad m =>
    (T.Tag -> name) -> Set T.Tag -> (T.Tag -> EntityId) ->
    (T.Tag -> T m ()) ->
    MkProperty' (T m) (Set T.Tag) ->
    OnceT (T m) (TagChoice name (OnceT (T m)) (T m))
replaceWith name forbiddenTags mkInstance setTag tagsProp =
    DataOps.genNewTag & lift & once
    <&>
    \mkNewTag ->
    TagChoice
    { _tcOptions =
        getP tagsProp & lift
        <&> (`Set.difference` forbiddenTags)
        <&> (^.. Lens.folded . Lens.to toOption)
    , _tcNewTag =
        mkNewTag
        <&>
        \newTag ->
        toOption newTag
        & toPick %~ (Property.modP tagsProp (Lens.contains newTag .~ True) >>)
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
    Anchors.CodeAnchors n -> Maybe VarInfo -> a ->
    m (OnceT (T n) (OptionalTag InternalName (OnceT (T n)) (T n)))
taggedEntityWith cp mVarInfo entity =
    getP prop
    <&>
    \entityTag ->
    refWith cp entityTag (nameWithContext mVarInfo entity)
    mempty mkInstance (setP prop)
    <&> (`OptionalTag` toAnon)
    where
        toAnon = mkInstance Anchors.anonTag <$ setP prop Anchors.anonTag
        mkInstance = EntityId.ofTaggedEntity entity
        prop = Anchors.assocTag entity

taggedEntity ::
    (UniqueId.ToUUID a, MonadTransaction n m, MonadReader env m, Anchors.HasCodeAnchors env n) =>
    Maybe VarInfo -> a -> m (OnceT (T n) (OptionalTag InternalName (OnceT (T n)) (T n)))
taggedEntity mVarInfo entity =
    Lens.view Anchors.codeAnchors >>= \x -> taggedEntityWith x mVarInfo entity
