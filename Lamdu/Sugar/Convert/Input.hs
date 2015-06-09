-- | Preprocess of input to sugar
{-# LANGUAGE RecordWildCards, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Lamdu.Sugar.Convert.Input
  ( Payload(..), entityId, inferred, mStored, evalResults, evalLamArgs, userData
  , mkPayload, mkUnstoredPayload
  ) where

import Control.Lens (Lens, Lens')
import Control.Lens.Operators
import Data.Foldable (Foldable)
import Data.Map (Map)
import Data.Store.Guid (Guid)
import Data.Traversable (Traversable)
import Lamdu.Eval.Results (ComputedVal)
import Lamdu.Eval.Val (ScopeId)
import Lamdu.Sugar.EntityId (EntityId)
import qualified Data.Map as Map
import qualified Data.Store.IRef as IRef
import qualified Data.Store.Property as Property
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Infer as Infer
import qualified Lamdu.Sugar.Internal.EntityId as EntityId

data Payload m a
  = Payload
    { _entityId :: EntityId
    , _inferred :: Infer.Payload
    , _mStored :: Maybe (ExprIRef.ValIProperty m)
    , _evalResults :: Map ScopeId (ComputedVal ())
    , _evalLamArgs :: Map ScopeId (ComputedVal ())
    , _userData :: a
    } deriving (Functor, Foldable, Traversable)


entityId :: Lens' (Payload m a) EntityId
entityId f Payload{..} = f _entityId <&> \_entityId -> Payload{..}

inferred :: Lens' (Payload m a) Infer.Payload
inferred f Payload{..} = f _inferred <&> \_inferred -> Payload{..}

mStored :: Lens' (Payload m a) (Maybe (ExprIRef.ValIProperty m))
mStored f Payload{..} = f _mStored <&> \_mStored -> Payload{..}

evalResults :: Lens' (Payload m a) (Map ScopeId (ComputedVal ()))
evalResults f Payload{..} = f _evalResults <&> \_evalResults -> Payload{..}

evalLamArgs :: Lens' (Payload m a) (Map ScopeId (ComputedVal ()))
evalLamArgs f Payload{..} = f _evalLamArgs <&> \_evalLamArgs -> Payload{..}

userData :: Lens (Payload m a) (Payload m b) a b
userData f Payload{..} = f _userData <&> \_userData -> Payload{..}

mkPayload ::
  a -> Infer.Payload ->
  Map ScopeId (ComputedVal ()) -> Map ScopeId (ComputedVal ()) ->
  ExprIRef.ValIProperty m -> Payload m a
mkPayload _userData _inferred _evalResults _evalLamArgs stored =
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
    _evalResults = Map.empty
    _evalLamArgs = Map.empty
