-- | Preprocess of input to sugar
{-# LANGUAGE TemplateHaskell, TypeApplications, TypeOperators #-}
module Lamdu.Sugar.Convert.Input
    ( Payload(..)
        , varRefsOfLambda, entityId, inferRes, stored
        , evalResults, userData, localsInScope, inferScope
        , inferredType
    , EvalResultsForExpr(..), eResults, eAppliesOfLam, emptyEvalResults
    , preparePayloads
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
    { _eResults :: Map ER.ScopeId (ER.Val (Tree Pure Type))
    , _eAppliesOfLam :: Map ER.ScopeId [(ER.ScopeId, ER.Val (Tree Pure Type))]
    }

data Payload m a = Payload
    { _entityId :: EntityId
    , _inferRes :: Tree (InferResult (Pure :*: UVar)) V.Term
    , _inferScope :: Tree V.Scope UVar
    , _localsInScope :: [V.Var]
    , _stored :: Tree (HRef m) V.Term
    , _evalResults :: CurAndPrev EvalResultsForExpr
    , -- The GetVars of this lambda's var if this is a lambda
      _varRefsOfLambda :: [EntityId]
    , _userData :: a
    }

Lens.makeLenses ''EvalResultsForExpr
Lens.makeLenses ''Payload

inferredType :: Lens' (Payload m a) (Tree Pure Type)
inferredType = inferRes . inferResult . Lens._1

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
    initScopes ::
        -- InferState is passed temporarily to lookup lambda type bodies with UVars
        -- (to get type of parameter).
        -- This will be removed when switching to typed lambdas.
        InferState ->
        Tree V.Scope UVar ->
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
    initScopes inferState iScope locals (Ann (Const pl) body) =
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
        & Ann (Const (pl & localsInScope <>~ locals & inferScope .~ iScope))

preparePayloads ::
    SugarInput t =>
    Annotated (EntityId, [EntityId] -> pl) t ->
    Annotated pl t
preparePayloads = ppRes . preparePayloadsH
