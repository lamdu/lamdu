{-# LANGUAGE TemplateHaskell #-}
module Lamdu.Sugar.Convert.Tag
    ( ref, replace, withoutContext
    , taggedEntity
    , taggedEntityWith
    , TagResultInfo(..), trEntityId, trPick
    , NameContext(..), ncMVarInfo, ncUuid
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.Once (MonadOnce(..), OnceT, Typeable)
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

data TagResultInfo m = TagResultInfo
    { _trEntityId :: EntityId
    , _trPick :: m ()
    }
Lens.makeLenses ''TagResultInfo

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
    (MonadTransaction n m, MonadReader env m, Anchors.HasCodeAnchors env n, Typeable a) =>
    T.Tag -> Maybe NameContext -> Set T.Tag -> OnceT (T n) a -> (a -> T.Tag -> TagResultInfo (T n)) ->
    m (OnceT (T n) (TagRef InternalName (OnceT (T n)) (T n)))
ref tag nameCtx forbiddenTags resultSeed resultInfo =
    Lens.view Anchors.codeAnchors
    <&> \anchors -> refWith anchors tag name forbiddenTags resultSeed resultInfo
    where
        withContext (NameContext varInfo var) = nameWithContext varInfo var
        name = maybe nameWithoutContext withContext nameCtx

refWith ::
    (Monad m, Typeable a) =>
    Anchors.CodeAnchors m ->
    T.Tag -> (T.Tag -> name) -> Set T.Tag ->
    OnceT (T m) a -> (a -> T.Tag -> TagResultInfo (T m)) ->
    OnceT (T m) (TagRef name (OnceT (T m)) (T m))
refWith cp tag name forbiddenTags resultSeed resultInfo =
    do
        r <- replaceWith name forbiddenTags resultSeed resultInfo tagsProp
        seed <- resultSeed
        pure TagRef
            { _tagRefTag = Tag
                { _tagName = name tag
                , _tagInstance = resultInfo seed tag ^. trEntityId
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
    (MonadReader env m, Monad n, Anchors.HasCodeAnchors env n, Typeable a) =>
    (T.Tag -> name) -> Set T.Tag -> OnceT (T n) a -> (a -> T.Tag -> TagResultInfo (T n)) ->
    m (OnceT (T n) (OnceT (T n) (TagChoice name (T n))))
replace name forbiddenTags resultSeed resultInfo =
    Lens.view Anchors.codeAnchors <&> Anchors.tags
    <&> replaceWith name forbiddenTags resultSeed resultInfo

replaceWith ::
    (Monad m, Typeable a) =>
    (T.Tag -> name) -> Set T.Tag ->
    OnceT (T m) a -> (a -> T.Tag -> TagResultInfo (T m)) ->
    MkProperty' (T m) (Set T.Tag) ->
    OnceT (T m) (OnceT (T m) (TagChoice name (T m)))
replaceWith name forbiddenTags resultSeed resultInfo tagsProp =
    (,,) <$> resultSeed <*> lift DataOps.genNewTag <*> lift (getP tagsProp) & once <&> Lens.mapped %~
    \(seed, newTag, tags) ->
    let toOption x =
            TagOption
            { _toInfo =
                Tag
                { _tagName = name x
                , _tagInstance = resultInfo seed x ^. trEntityId
                , _tagVal = x
                }
            , _toPick = resultInfo seed x ^. trPick
            }
    in
    TagChoice
    { _tcOptions = Set.difference tags forbiddenTags ^.. Lens.folded . Lens.to toOption
    , _tcNewTag = toOption newTag & toPick %~ (Property.modP tagsProp (Lens.contains newTag .~ True) >>)
    }

-- NOTE: Used for panes, outside ConvertM, so has no ConvertM.Context env
-- | Convert a "Entity" (param, def, TId) via its associated tag
taggedEntityWith ::
    (UniqueId.ToUUID a, Monad n) =>
    Anchors.CodeAnchors n -> Maybe VarInfo -> a ->
    OnceT (T n) (OptionalTag InternalName (OnceT (T n)) (T n))
taggedEntityWith cp mVarInfo entity =
    do
        entityTag <- getP prop
        refWith cp entityTag (nameWithContext mVarInfo entity)
            mempty (pure ()) resultInfo
    <&> (`OptionalTag` toAnon)
    where
        toAnon = mkInstance Anchors.anonTag <$ setP prop Anchors.anonTag
        resultInfo () = TagResultInfo <$> mkInstance <*> setP prop
        mkInstance = EntityId.ofTaggedEntity entity
        prop = Anchors.assocTag entity

taggedEntity ::
    (UniqueId.ToUUID a, MonadTransaction n m, MonadReader env m, Anchors.HasCodeAnchors env n) =>
    Maybe VarInfo -> a -> m (OnceT (T n) (OptionalTag InternalName (OnceT (T n)) (T n)))
taggedEntity mVarInfo entity =
    Lens.view Anchors.codeAnchors <&> \x -> taggedEntityWith x mVarInfo entity
