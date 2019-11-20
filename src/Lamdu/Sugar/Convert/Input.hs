-- | Preprocess of input to sugar
{-# LANGUAGE TemplateHaskell, TypeApplications #-}
module Lamdu.Sugar.Convert.Input
    ( Payload(..)
        , varRefsOfLambda, entityId, inferredType, inferResult, stored
        , evalResults, userData, localsInScope, inferScope
    , EvalResultsForExpr(..), eResults, eAppliesOfLam, emptyEvalResults
    , preparePayloads
    , SugarInput(..)
    ) where

import qualified Control.Lens as Lens
import           Data.CurAndPrev (CurAndPrev(..))
import qualified Data.Map as Map
import           Hyper
import           Hyper.Unify.Binding (UVar)
import qualified Lamdu.Calc.Term as V
import           Lamdu.Calc.Type (Type)
import qualified Lamdu.Eval.Results as ER
import           Lamdu.Expr.IRef (ValP)
import           Lamdu.Sugar.EntityId (EntityId)

import           Lamdu.Prelude

data EvalResultsForExpr = EvalResultsForExpr
    { _eResults :: Map ER.ScopeId (ER.Val (Tree Pure Type))
    , _eAppliesOfLam :: Map ER.ScopeId [(ER.ScopeId, ER.Val (Tree Pure Type))]
    }

data Payload m a = Payload
    { _entityId :: EntityId
    , _inferredType :: Tree Pure Type
    , -- The inference result before binding universal quantifiers,
      -- Useful for resuming inference in holes.
      _inferResult :: Tree UVar Type
    , _inferScope :: Tree V.Scope UVar
    , _localsInScope :: [V.Var]
    , _stored :: ValP m
    , _evalResults :: CurAndPrev EvalResultsForExpr
    , -- The GetVars of this lambda's var if this is a lambda
      _varRefsOfLambda :: [EntityId]
    , _userData :: a
    } deriving (Functor, Foldable, Traversable)

Lens.makeLenses ''EvalResultsForExpr
Lens.makeLenses ''Payload

emptyEvalResults :: EvalResultsForExpr
emptyEvalResults = EvalResultsForExpr Map.empty Map.empty

data PreparePayloadsRes pl t = PreparePayloadsRes
    { ppVarMap :: Map V.Var [EntityId]
    , ppRes :: Ann (Const pl) t
    }

class SugarInput t where
    preparePayloadsH ::
        Annotated (EntityId, [EntityId] -> pl) t ->
        Tree (PreparePayloadsRes pl) t
    initLocalsInScope ::
        [V.Var] ->
        Annotated (Payload m a) t ->
        Annotated (Payload m a) t

instance SugarInput V.Term where
    preparePayloadsH (Ann (Const (x, mkPayload)) body) =
        PreparePayloadsRes
        { ppVarMap =
            childrenVars
            & case body of
            V.BLeaf (V.LVar var) -> Lens.at var <>~ Just [x]
            V.BLam (V.Lam var _) -> Lens.at var .~ Nothing
            _ -> id
        , ppRes =
            hmap (const ppRes) b
            & Ann
            ( case body of
                V.BLam (V.Lam var _) -> childrenVars ^. Lens.ix var
                _ -> []
                & mkPayload
                & Const
            )
        }
        where
            childrenVars = Map.unionsWith (++) (hfoldMap (const ((:[]) . ppVarMap)) b)
            b = hmap (Proxy @SugarInput #> preparePayloadsH) body
    initLocalsInScope locals (Ann (Const pl) body) =
        case body of
        V.BLam (V.Lam var b) -> initLocalsInScope (var : locals) b & V.Lam var & V.BLam
        x -> hmap (Proxy @SugarInput #> initLocalsInScope locals) x
        & Ann (Const (pl & localsInScope <>~ locals))

preparePayloads ::
    SugarInput t =>
    Annotated (EntityId, [EntityId] -> pl) t ->
    Annotated pl t
preparePayloads = ppRes . preparePayloadsH
