-- | Preprocess of input to sugar
{-# LANGUAGE NoImplicitPrelude, RecordWildCards, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Lamdu.Sugar.Convert.Input
    ( Payload(..), entityId, inferred, mStored, evalResults, userData
    , EvalResultsForExpr(..), eResults, eAppliesOfLam
    , inferredType, inferredScope
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
import           Lamdu.Eval.Val (EvalResult, ScopeId)
import qualified Lamdu.Expr.IRef as ExprIRef
import           Lamdu.Expr.Type (Type)
import qualified Lamdu.Infer as Infer
import           Lamdu.Sugar.EntityId (EntityId)
import qualified Lamdu.Sugar.Internal.EntityId as EntityId

data EvalResultsForExpr = EvalResultsForExpr
    { _eResults :: Map ScopeId (EvalResult ())
    , _eAppliesOfLam :: Map ScopeId [(ScopeId, EvalResult ())]
    }

data Payload m a = Payload
    { _entityId :: EntityId
    , _inferred :: Infer.Payload
    , _mStored :: Maybe (ExprIRef.ValIProperty m)
    , _evalResults :: EvalResultsForExpr
    , _userData :: a
    } deriving (Functor, Foldable, Traversable)

eResults :: Lens' EvalResultsForExpr (Map ScopeId (EvalResult ()))
eResults f EvalResultsForExpr{..} = f _eResults <&> \_eResults -> EvalResultsForExpr{..}

eAppliesOfLam :: Lens' EvalResultsForExpr (Map ScopeId [(ScopeId, EvalResult ())])
eAppliesOfLam f EvalResultsForExpr{..} = f _eAppliesOfLam <&> \_eAppliesOfLam -> EvalResultsForExpr{..}

entityId :: Lens' (Payload m a) EntityId
entityId f Payload{..} = f _entityId <&> \_entityId -> Payload{..}

inferred :: Lens' (Payload m a) Infer.Payload
inferred f Payload{..} = f _inferred <&> \_inferred -> Payload{..}

inferredType :: Lens' (Payload m a) Type
inferredType = inferred . Infer.plType

inferredScope :: Lens' (Payload m a) Infer.Scope
inferredScope = inferred . Infer.plScope

mStored :: Lens' (Payload m a) (Maybe (ExprIRef.ValIProperty m))
mStored f Payload{..} = f _mStored <&> \_mStored -> Payload{..}

evalResults :: Lens' (Payload m a) EvalResultsForExpr
evalResults f Payload{..} = f _evalResults <&> \_evalResults -> Payload{..}

userData :: Lens (Payload m a) (Payload m b) a b
userData f Payload{..} = f _userData <&> \_userData -> Payload{..}

mkPayload ::
    a -> Infer.Payload ->
    Map ScopeId (EvalResult ()) -> Map ScopeId [(ScopeId, EvalResult ())] ->
    ExprIRef.ValIProperty m -> Payload m a
mkPayload _userData _inferred eRes eAppsOfLam stored =
    Payload{..}
    where
        _guid = IRef.guid $ ExprIRef.unValI $ Property.value stored
        _entityId = EntityId.ofValI $ Property.value stored
        _mStored = Just stored
        _evalResults = EvalResultsForExpr eRes eAppsOfLam

mkUnstoredPayload :: a -> Infer.Payload -> Guid -> EntityId -> Payload m a
mkUnstoredPayload _userData _inferred _guid _entityId =
    Payload{..}
    where
        _mStored = Nothing
        _evalResults = EvalResultsForExpr Map.empty Map.empty
