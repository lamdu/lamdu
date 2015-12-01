-- | Preprocess of input to sugar
{-# LANGUAGE NoImplicitPrelude, RecordWildCards, DeriveFunctor, DeriveFoldable, DeriveTraversable, TemplateHaskell #-}
module Lamdu.Sugar.Convert.Input
    ( Payload(..)
        , entityId, inferred, mStored, evalResults, userData
    , EvalResultsForExpr(..), eResults, eAppliesOfLam, emptyEvalResults
    , inferredType, inferredScope
    , preparePayloads, prepareUnstoredPayloads
    ) where

import           Control.Lens (Lens')
import qualified Control.Lens as Lens
import           Control.Lens.Operators
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

Lens.makeLenses ''EvalResultsForExpr
Lens.makeLenses ''Payload

inferredType :: Lens' (Payload m a) Type
inferredType = inferred . Infer.plType

inferredScope :: Lens' (Payload m a) Infer.Scope
inferredScope = inferred . Infer.plScope

propEntityId :: Property m (ValI m) -> EntityId
propEntityId = EntityId.ofValI . Property.value

emptyEvalResults :: EvalResultsForExpr
emptyEvalResults = EvalResultsForExpr Map.empty Map.empty

-- Unstored and without eval results (e.g: hole result)
prepareUnstoredPayloads ::
    Val (Infer.Payload, EntityId, a) ->
    Val (Payload m a)
prepareUnstoredPayloads val =
    val <&> f
    where
        f (inferPl, eId, x) =
            Payload
            { _userData = x
            , _inferred = inferPl
            , _entityId = eId
            , _mStored = Nothing
            , _evalResults = CurAndPrev emptyEvalResults emptyEvalResults
            }

preparePayloads ::
    CurAndPrev (EvalResults (ValI m)) ->
    Val (Infer.Payload, ValIProperty m) ->
    Val (Payload m ())
preparePayloads evalRes inferredVal =
    inferredVal <&> f
    where
        f (inferPl, valIProp) =
            Payload
            { _entityId = propEntityId valIProp
            , _mStored = Just valIProp
            , _inferred = inferPl
            , _evalResults = evalRes <&> exprEvalRes execId
            , _userData = ()
            }
            where
                execId = Property.value valIProp
        exprEvalRes pl r =
            EvalResultsForExpr
            (r ^. erExprValues . Lens.at pl . Lens._Just)
            (r ^. erAppliesOfLam . Lens.at pl . Lens._Just)
