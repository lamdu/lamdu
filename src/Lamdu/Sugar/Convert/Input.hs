-- | Preprocess of input to sugar
{-# LANGUAGE TemplateHaskell, TypeApplications, FlexibleInstances #-}
{-# LANGUAGE DataKinds, UndecidableInstances, GADTs, TypeFamilies #-}
module Lamdu.Sugar.Convert.Input
    ( Payload(..)
        , varRefsOfLambda, entityId, inferRes, stored
        , evalResults, userData, localsInScope, inferScope
        , inferredType, inferredTypeUVar
    , EvalResultsForExpr(..), eResults, eAppliesOfLam, emptyEvalResults
    , PreparePayloadInput(..), preparePayloads
    , SugarInput(..)
    ) where

import qualified Control.Lens as Lens
import           Data.CurAndPrev (CurAndPrev(..))
import qualified Data.Map as Map
import           Hyper
import           Hyper.Infer (InferResult, inferResult)
import           Hyper.Type.Prune (Prune)
import           Hyper.Unify.Binding (UVar)
import           Hyper.Unify.Generalize (GTerm(..))
import qualified Lamdu.Calc.Term as V
import           Lamdu.Calc.Type (Type, Row)
import qualified Lamdu.Eval.Results as ER
import           Lamdu.Expr.IRef (HRef)
import           Lamdu.Sugar.EntityId (EntityId)

import           Lamdu.Prelude

data EvalResultsForExpr = EvalResultsForExpr
    { _eResults :: Map ER.ScopeId (ER.Val (Pure # Type))
    , _eAppliesOfLam :: Map ER.ScopeId [(ER.ScopeId, ER.Val (Pure # Type))]
    } deriving Show

data Payload m a h = Payload
    { _entityId :: EntityId
    , _inferRes :: InferResult (Pure :*: UVar) h
    , _inferScope :: V.Scope # UVar
    , _localsInScope :: [V.Var]
    , _stored :: HRef m h
    , _evalResults :: CurAndPrev EvalResultsForExpr
    , -- The GetVars of this lambda's var if this is a lambda
      _varRefsOfLambda :: [EntityId]
    , _userData :: a
    }

Lens.makeLenses ''EvalResultsForExpr
Lens.makeLenses ''Payload
makeHTraversableAndBases ''Payload

inferredType :: Lens' (Payload m a # V.Term) (Pure # Type)
inferredType = inferRes . inferResult . Lens._1

inferredTypeUVar :: Lens' (Payload m a # V.Term) (UVar # Type)
inferredTypeUVar = inferRes . inferResult . Lens._2

emptyEvalResults :: EvalResultsForExpr
emptyEvalResults = EvalResultsForExpr Map.empty Map.empty

data PreparePayloadInput (pl :: HyperType) t = PreparePayloadInput
    { ppEntityId :: EntityId -- used to build a map from Var => EntityId of all getVars
    , ppMakePl :: [EntityId] -> pl t
    }

data PreparePayloadsRes (pl :: HyperType) t = PreparePayloadsRes
    { ppVarMap :: Map V.Var [EntityId]
    , ppRes :: Ann pl t
    }

class SugarInput t where
    preparePayloadsH ::
        Ann (PreparePayloadInput pl) # t ->
        PreparePayloadsRes pl # t
    initScopes ::
        V.Scope # UVar -> [V.Var] ->
        Ann (Payload m a) # t ->
        Ann (Payload m a) # t
    initScopes _ _ = id

instance SugarInput V.Term where
    preparePayloadsH (Ann (PreparePayloadInput eId mkPayload) body) =
        PreparePayloadsRes
        { ppVarMap =
            childrenVars
            & case body of
            V.BLeaf (V.LVar var) -> Lens.at var <>~ Just [eId]
            V.BLam (V.TypedLam var _ _) -> Lens.at var .~ Nothing
            _ -> id
        , ppRes =
            hmap (const ppRes) b
            & Ann
            ( case body of
                V.BLam (V.TypedLam var _ _) -> childrenVars ^. Lens.ix var
                _ -> []
                & mkPayload
            )
        }
        where
            childrenVars = Map.unionsWith (++) (hfoldMap (const ((:[]) . ppVarMap)) b)
            b = hmap (Proxy @SugarInput #> preparePayloadsH) body
    initScopes iScope locals (Ann pl body) =
        case body of
        V.BLam (V.TypedLam var t b) ->
            initScopes innerScope (var : locals) b
            & V.TypedLam var t
            & V.BLam
            where
                innerScope =
                    iScope
                    & V.scopeVarTypes . Lens.at var ?~
                        _HFlip # GMono (t ^. hAnn . inferRes . inferResult . Lens._2)
        x -> hmap (Proxy @SugarInput #> initScopes iScope locals) x
        & Ann (pl & localsInScope <>~ locals & inferScope .~ iScope)

instance SugarInput (HCompose Prune Type) where
    preparePayloadsH (Ann (PreparePayloadInput _ mkPayload) body) =
        PreparePayloadsRes
        { ppVarMap = mempty
        , ppRes =
            hmap (Proxy @SugarInput #> ppRes . preparePayloadsH) body
            & Ann (mkPayload [])
        }

instance SugarInput (HCompose Prune Row) where
    preparePayloadsH (Ann (PreparePayloadInput _ mkPayload) body) =
        PreparePayloadsRes
        { ppVarMap = mempty
        , ppRes =
            hmap (Proxy @SugarInput #> ppRes . preparePayloadsH) body
            & Ann (mkPayload [])
        }

preparePayloads ::
    SugarInput t =>
    Ann (PreparePayloadInput pl) # t ->
    Ann pl # t
preparePayloads = ppRes . preparePayloadsH
