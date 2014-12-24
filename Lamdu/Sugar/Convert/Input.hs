-- | Preprocess of input to sugar 
{-# LANGUAGE RecordWildCards, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Lamdu.Sugar.Convert.Input
  ( Payload(..), entityId, guid, inferred, mStored, userData
  , mkPayload, mkUnstoredPayload
  ) where

import Control.Lens (Lens, Lens')
import Control.Lens.Operators
import Data.Foldable (Foldable)
import Data.Store.Guid (Guid)
import Data.Traversable (Traversable)
import Lamdu.Sugar.EntityId (EntityId)
import qualified Data.Store.IRef as IRef
import qualified Data.Store.Property as Property
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Infer as Infer
import qualified Lamdu.Sugar.Internal.EntityId as EntityId

data Payload m a
  = Payload
    { _entityId :: EntityId
    , -- Used as a hole id that later GUI uses to associate data with
      -- Need to replace this with some mechanism that avoids exposing
      -- Guids to GUI
      _guid :: Guid
    , _inferred :: Infer.Payload
    , _mStored :: Maybe (ExprIRef.ValIProperty m)
    , _userData :: a
    } deriving (Functor, Foldable, Traversable)


entityId :: Lens' (Payload m a) EntityId
entityId f Payload{..} = f _entityId <&> \_entityId -> Payload{..}

guid :: Lens' (Payload m a) Guid
guid f Payload{..} = f _guid <&> \_guid -> Payload{..}

inferred :: Lens' (Payload m a) Infer.Payload
inferred f Payload{..} = f _inferred <&> \_inferred -> Payload{..}

mStored :: Lens' (Payload m a) (Maybe (ExprIRef.ValIProperty m))
mStored f Payload{..} = f _mStored <&> \_mStored -> Payload{..}

userData :: Lens (Payload m a) (Payload m b) a b
userData f Payload{..} = f _userData <&> \_userData -> Payload{..}

mkPayload :: a -> (Infer.Payload, ExprIRef.ValIProperty m) -> Payload m a
mkPayload _userData (_inferred, stored) =
  Payload{..}
  where
    _guid = IRef.guid $ ExprIRef.unValI $ Property.value stored
    _entityId = EntityId.ofValI $ Property.value stored
    _mStored = Just stored

mkUnstoredPayload :: a -> Infer.Payload -> Guid -> EntityId -> Payload m a
mkUnstoredPayload _userData _inferred _guid _entityId =
  Payload{..}
  where
    _mStored = Nothing
