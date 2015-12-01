-- | Preprocess of input to sugar
{-# LANGUAGE NoImplicitPrelude, RecordWildCards, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Lamdu.Sugar.Convert.Input
    ( Payload(..)
        , entityId, inferred, mStored, evalResults, userData
    , EvalResultsForExpr(..), eResults, eAppliesOfLam, emptyEvalResults
    , inferredType, inferredScope
    , mkPayload, mkUnstoredPayload
    , preparePayloads
    ) where

import           Control.Lens (Lens, Lens')
import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.MonadA (MonadA)
import           Data.CurAndPrev (CurAndPrev(..))
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Store.Property as Property
import           Data.Store.Transaction (Property)
import           Lamdu.Eval.Results (EvalResults, erExprValues, erAppliesOfLam)
import           Lamdu.Eval.Val (EvalResult, ScopeId)
import           Lamdu.Expr.IRef (ValI, ValIProperty)
import           Lamdu.Expr.Type (Type)
import           Lamdu.Expr.Val (Val(..))
import qualified Lamdu.Infer as Infer
import           Lamdu.Sugar.EntityId (EntityId)
import qualified Lamdu.Sugar.Internal.EntityId as EntityId

import           Prelude.Compat

data EvalResultsForExpr = EvalResultsForExpr
    { _eResults :: Map ScopeId (EvalResult ())
    , _eAppliesOfLam :: Map ScopeId [(ScopeId, EvalResult ())]
    }

data Payload m a = Payload
    { _entityId :: EntityId
    , _inferred :: Infer.Payload
    , _mStored :: Maybe (ValIProperty m)
    , _evalResults :: CurAndPrev EvalResultsForExpr
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

mStored :: Lens' (Payload m a) (Maybe (ValIProperty m))
mStored f Payload{..} = f _mStored <&> \_mStored -> Payload{..}

evalResults :: Lens' (Payload m a) (CurAndPrev EvalResultsForExpr)
evalResults f Payload{..} = f _evalResults <&> \_evalResults -> Payload{..}

userData :: Lens (Payload m a) (Payload m b) a b
userData f Payload{..} = f _userData <&> \_userData -> Payload{..}

propEntityId :: Property m (ValI m) -> EntityId
propEntityId = EntityId.ofValI . Property.value

mkPayload ::
    a -> Infer.Payload ->
    CurAndPrev EvalResultsForExpr -> ValIProperty m ->
    Payload m a
mkPayload _userData _inferred _evalResults stored =
    Payload{..}
    where
        _entityId = propEntityId stored
        _mStored = Just stored

mkUnstoredPayload :: a -> Infer.Payload -> EntityId -> Payload m a
mkUnstoredPayload _userData _inferred _entityId =
    Payload{..}
    where
        _mStored = Nothing
        _evalResults = CurAndPrev emptyEvalResults emptyEvalResults

emptyEvalResults :: EvalResultsForExpr
emptyEvalResults = EvalResultsForExpr Map.empty Map.empty

preparePayloads ::
    MonadA m =>
    CurAndPrev (EvalResults (ValI m)) ->
    Val (Infer.Payload, ValIProperty m) ->
    Val (Payload m [EntityId])
preparePayloads evalRes inferredVal =
    inferredVal <&> f
    where
        f (inferPl, valIProp) =
            mkPayload [propEntityId valIProp] inferPl
            (evalRes <&> exprEvalRes execId)
            valIProp
            where
                execId = Property.value valIProp
        exprEvalRes pl r =
            EvalResultsForExpr
            (r ^. erExprValues . Lens.at pl . Lens._Just)
            (r ^. erAppliesOfLam . Lens.at pl . Lens._Just)
