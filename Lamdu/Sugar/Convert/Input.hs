-- | Preprocess of input to sugar
{-# LANGUAGE NoImplicitPrelude, RecordWildCards, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Lamdu.Sugar.Convert.Input
    ( Payload(..), entityId, inferred, mStored, evalResults, evalAppliesOfLam, userData
    , mkPayload, mkUnstoredPayload
    ) where

import           Prelude.Compat

import           Control.Lens (Lens, Lens')
import           Control.Lens.Operators
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Store.Guid (Guid)
import qualified Data.Store.IRef as IRef
import qualified Data.Store.Property as Property
import           Lamdu.Eval.Val (Val, ScopeId)
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Infer as Infer
import           Lamdu.Sugar.EntityId (EntityId)
import qualified Lamdu.Sugar.Internal.EntityId as EntityId

data Payload m a = Payload
    { _entityId :: EntityId
    , _inferred :: Infer.Payload
    , _mStored :: Maybe (ExprIRef.ValIProperty m)
    , _evalResults :: Map ScopeId (Val ())
    , _evalAppliesOfLam :: Map ScopeId [(ScopeId, Val ())]
    , _userData :: a
    } deriving (Functor, Foldable, Traversable)


entityId :: Lens' (Payload m a) EntityId
entityId f Payload{..} = f _entityId <&> \_entityId -> Payload{..}

inferred :: Lens' (Payload m a) Infer.Payload
inferred f Payload{..} = f _inferred <&> \_inferred -> Payload{..}

mStored :: Lens' (Payload m a) (Maybe (ExprIRef.ValIProperty m))
mStored f Payload{..} = f _mStored <&> \_mStored -> Payload{..}

evalResults :: Lens' (Payload m a) (Map ScopeId (Val ()))
evalResults f Payload{..} = f _evalResults <&> \_evalResults -> Payload{..}

evalAppliesOfLam :: Lens' (Payload m a) (Map ScopeId [(ScopeId, Val ())])
evalAppliesOfLam f Payload{..} = f _evalAppliesOfLam <&> \_evalAppliesOfLam -> Payload{..}

userData :: Lens (Payload m a) (Payload m b) a b
userData f Payload{..} = f _userData <&> \_userData -> Payload{..}

mkPayload ::
    a -> Infer.Payload ->
    Map ScopeId (Val ()) -> Map ScopeId [(ScopeId, Val ())] ->
    ExprIRef.ValIProperty m -> Payload m a
mkPayload _userData _inferred _evalResults _evalAppliesOfLam stored =
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
        _evalAppliesOfLam = Map.empty
