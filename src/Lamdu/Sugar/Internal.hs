{-# LANGUAGE TemplateHaskell #-}

module Lamdu.Sugar.Internal
    ( ConvertPayload(..), pStored, pUserData
    , InternalName(..), inTag, inContext
    , internalNameMatch
    , nameWithoutContext, nameWithContext, taggedName
    , ExpressionU
    , replaceWith
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.Transaction (MonadTransaction, getP)
import qualified Data.Property as Property
import           Data.UUID.Types (UUID)
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.UniqueId as UniqueId
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.Types
import           Revision.Deltum.Transaction (Transaction)

import           Lamdu.Prelude

type T = Transaction

data ConvertPayload m a = ConvertPayload
    { -- Stored of top-level subtree for sugar expression subtree
      _pStored :: ExprIRef.ValP m
    , _pUserData :: a
    } deriving (Functor, Foldable, Traversable)

-- | Tags have internal names.
--
-- Some entities (e.g: record fields) directly contain tags, so their
-- InternalName has a Nothing as the inContext
--
-- Other entities (e.g: Nominals[TId] or Definitions/Parameters[Var])
-- are associated with tags, so their InternalName has the UUID of the
-- Nominal/Def/Parameter as context
data InternalName = InternalName
    { _inContext :: Maybe UUID
    , _inTag :: T.Tag
    } deriving (Eq, Ord, Show)

-- 2 Internal names clash if their UUIDs mismatch OR if they
-- positively have Vars that mismatch
--
-- i.e: Having no Var (e.g: a record field) means it matches the exact
-- same tag set for a Var
internalNameMatch :: InternalName -> InternalName -> Maybe InternalName
internalNameMatch a@(InternalName aMVar aUuid) b@(InternalName bMVar bUuid)
    | aUuid /= bUuid = Nothing
    | otherwise =
        case (aMVar, bMVar) of
        (Just aVar, Just bVar)
            | aVar == bVar -> Just a
            | otherwise -> Nothing
        (Nothing, _) -> Just b
        (_, Nothing) -> Just a

nameWithoutContext :: T.Tag -> InternalName
nameWithoutContext tag =
    InternalName
    { _inContext = Nothing
    , _inTag = tag
    }

nameWithContext :: UniqueId.ToUUID a => a -> T.Tag -> InternalName
nameWithContext param tag =
    InternalName
    { _inContext = Just (UniqueId.toUUID param)
    , _inTag = tag
    }

taggedName :: (MonadTransaction n m, UniqueId.ToUUID a) => a -> m InternalName
taggedName x = Anchors.assocTag x & getP <&> nameWithContext x


type ExpressionU m a = Expression InternalName (T m) (T m) (ConvertPayload m a)

replaceWith ::
    Monad m => ExprIRef.ValP m -> ExprIRef.ValP m ->
    T m EntityId
replaceWith parentP replacerP =
    EntityId.ofValI replacerI <$ Property.set parentP replacerI
    where
        replacerI = Property.value replacerP

Lens.makeLenses ''ConvertPayload
Lens.makeLenses ''InternalName
