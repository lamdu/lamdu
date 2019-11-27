-- | Preprocess of input to sugar
{-# LANGUAGE TemplateHaskell, TypeApplications, TypeOperators #-}
{-# LANGUAGE DataKinds, UndecidableInstances, GADTs, TypeFamilies #-}
module Lamdu.Sugar.Convert.Input
    ( Payload(..)
        , varRefsOfLambda, entityId, inferRes, stored
        , evalResults, userData, localsInScope, inferScope
        , inferredType
    , EvalResultsForExpr(..), eResults, eAppliesOfLam, emptyEvalResults
    , PreparePayloadInput(..), preparePayloads
    , SugarInput(..)
    ) where

import qualified Control.Lens as Lens
import           Data.CurAndPrev (CurAndPrev(..))
import qualified Data.Map as Map
import           Hyper
import           Hyper.Infer (InferResult, inferResult)
import           Hyper.Type.AST.FuncType (funcType, funcIn)
import           Hyper.Unify.Binding (UVar)
import           Hyper.Unify.Generalize (GTerm(..))
import           Hyper.Unify.Lookup (semiPruneLookup)
import           Hyper.Unify.Term (_UTerm, uBody)
import           Lamdu.Calc.Infer (InferState, runPureInfer)
import qualified Lamdu.Calc.Term as V
import           Lamdu.Calc.Type (Type)
import qualified Lamdu.Eval.Results as ER
import           Lamdu.Expr.IRef (HRef)
import           Lamdu.Sugar.EntityId (EntityId)

import           Lamdu.Prelude

data EvalResultsForExpr = EvalResultsForExpr
    { _eResults :: Map ER.ScopeId (ER.Val (Pure # Type))
    , _eAppliesOfLam :: Map ER.ScopeId [(ER.ScopeId, ER.Val (Pure # Type))]
    }

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

emptyEvalResults :: EvalResultsForExpr
emptyEvalResults = EvalResultsForExpr Map.empty Map.empty

data PreparePayloadInput (pl :: HyperType) t = PreparePayloadInput
    { ppEntityId :: EntityId
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
        -- InferState is passed temporarily to lookup lambda type bodies with UVars
        -- (to get type of parameter).
        -- This will be removed when switching to typed lambdas.
        InferState ->
        V.Scope # UVar ->
        [V.Var] ->
        Ann (Payload m a) # t ->
        Ann (Payload m a) # t

instance SugarInput V.Term where
    preparePayloadsH (Ann (PreparePayloadInput x mkPayload) body) =
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
            )
        }
        where
            childrenVars = Map.unionsWith (++) (hfoldMap (const ((:[]) . ppVarMap)) b)
            b = hmap (Proxy @SugarInput #> preparePayloadsH) body
    initScopes inferState iScope locals (Ann pl body) =
        case body of
        V.BLam (V.Lam var b) ->
            initScopes inferState innerScope (var : locals) b & V.Lam var & V.BLam
            where
                mArgType =
                    runPureInfer () inferState (semiPruneLookup (pl ^. inferRes . inferResult . Lens._2))
                    ^? Lens._Right . Lens._1 . Lens._2 . _UTerm . uBody . funcType . funcIn
                innerScope =
                    maybe iScope (\x -> iScope & V.scopeVarTypes . Lens.at var ?~ _HFlip # GMono x) mArgType
        x -> hmap (Proxy @SugarInput #> initScopes inferState iScope locals) x
        & Ann (pl & localsInScope <>~ locals & inferScope .~ iScope)

preparePayloads ::
    SugarInput t =>
    Ann (PreparePayloadInput pl) # t ->
    Ann pl # t
preparePayloads = ppRes . preparePayloadsH
