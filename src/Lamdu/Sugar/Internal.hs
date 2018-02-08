{-# LANGUAGE NoImplicitPrelude, DeriveTraversable, TemplateHaskell #-}

module Lamdu.Sugar.Internal
    ( ConvertPayload(..), pStored, pUserData
    , InternalName(..), inUUID
    , ExpressionU
    , replaceWith
    ) where

import qualified Control.Lens as Lens
import           Data.UUID.Types (UUID)
import qualified Data.Store.Property as Property
import           Data.Store.Transaction (Transaction)
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.Types

import           Lamdu.Prelude

type T = Transaction

data ConvertPayload m a = ConvertPayload
    { -- Stored of top-level subtree for sugar expression subtree
      _pStored :: ExprIRef.ValIProperty m
    , _pUserData :: a
    } deriving (Functor, Foldable, Traversable)

newtype InternalName = InternalName
    { _inUUID :: UUID
    } deriving (Eq, Ord)

type ExpressionU m a = Expression InternalName (T m) (ConvertPayload m a)

replaceWith ::
    Monad m => ExprIRef.ValIProperty m -> ExprIRef.ValIProperty m ->
    T m EntityId
replaceWith parentP replacerP =
    EntityId.ofValI replacerI <$ Property.set parentP replacerI
    where
        replacerI = Property.value replacerP

Lens.makeLenses ''ConvertPayload
Lens.makeLenses ''InternalName
