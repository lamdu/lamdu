{-# LANGUAGE FlexibleContexts #-}

module Lamdu.Sugar.Convert.Tag
    ( convertTag, convertTagSelection, convertTaggedEntityWith, convertTaggedEntity
    , AllowAnonTag(..)
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.Transaction (MonadTransaction, getP, setP)
import           Data.Has (Has(..))
import           Data.Property (MkProperty')
import qualified Data.Property as Property
import qualified Data.Set as Set
import qualified GUI.Momentu.Direction as Dir
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Ops as DataOps
import           Lamdu.Data.Tag (HasLanguageIdentifier)
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

getTagsProp :: Monad m => ConvertM m (MkProperty' (T m) (Set T.Tag))
getTagsProp =
    Lens.view ConvertM.scCodeAnchors <&> Anchors.tags

-- forbiddenTags are sibling tags in the same record/funcParams/etc,
-- NOT type-level constraints on tags. Violation of constraints is
-- allowed, generating ordinary type errors
convertTag ::
    Monad m =>
    T.Tag -> (T.Tag -> name) -> Set T.Tag -> (T.Tag -> EntityId) ->
    (T.Tag -> T m ()) ->
    ConvertM m (Tag name (T m) (T m))
convertTag tag name forbiddenTags mkInstance setTag =
    do
        env <- Lens.view id
        getTagsProp
            <&> convertTagWith env tag name forbiddenTags RequireTag mkInstance setTag

convertTagWith ::
    (Monad m, HasLanguageIdentifier env, Has Dir.Layout env) =>
    env ->
    T.Tag -> (T.Tag -> name) -> Set T.Tag -> AllowAnonTag -> (T.Tag -> EntityId) ->
    (T.Tag -> T m ()) -> MkProperty' (T m) (Set T.Tag) ->
    Tag name (T m) (T m)
convertTagWith env tag name forbiddenTags allowAnon mkInstance setTag tagsProp =
    convertTagSelectionWith env name forbiddenTags allowAnon mkInstance setTag tagsProp
    & Tag (TagInfo (name tag) (mkInstance tag) tag)

convertTagSelection ::
    Monad m =>
    (T.Tag -> name) -> Set T.Tag -> AllowAnonTag -> (T.Tag -> EntityId) -> (T.Tag -> T m a) ->
    ConvertM m (TagSelection name (T m) (T m) a)
convertTagSelection name forbiddenTags allowAnon mkInstance setTag =
    do
        env <- Lens.view id
        getTagsProp <&>
            convertTagSelectionWith env name forbiddenTags allowAnon mkInstance setTag

convertTagSelectionWith ::
    (Monad m, HasLanguageIdentifier env, Has Dir.Layout env) =>
    env ->
    (T.Tag -> name) -> Set T.Tag -> AllowAnonTag -> (T.Tag -> EntityId) -> (T.Tag -> T m a) ->
    MkProperty' (T m) (Set T.Tag) ->
    TagSelection name (T m) (T m) a
convertTagSelectionWith env name forbiddenTags allowAnon mkInstance setTag tagsProp =
    TagSelection
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
            { _toInfo = TagInfo (name x) (mkInstance x) x
            , _toPick = setTag x
            }

-- | Convert a "Entity" (param, def, TId) via its associated tag
convertTaggedEntityWith ::
    ( UniqueId.ToUUID a, MonadTransaction n m
    , HasLanguageIdentifier env, Has Dir.Layout env
    ) =>
    env ->
    a -> MkProperty' (T n) (Set T.Tag) -> m (Tag InternalName (T n) (T n))
convertTaggedEntityWith env entity tagsProp =
    getP prop
    <&>
    \entityTag ->
    convertTagWith env entityTag (nameWithContext entity) mempty AllowAnon
    (EntityId.ofTaggedEntity entity) (setP prop) tagsProp
    where
        prop = Anchors.assocTag entity

convertTaggedEntity ::
    (UniqueId.ToUUID a, Monad m) => a -> ConvertM m (Tag InternalName (T m) (T m))
convertTaggedEntity entity =
    do
        env <- Lens.view id
        getTagsProp >>= convertTaggedEntityWith env entity
